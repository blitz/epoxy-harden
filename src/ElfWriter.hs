module ElfWriter (writeElf) where

import           Control.Monad        (when)
import           Data.Binary.Put
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as BL
import           Data.Foldable        (foldl')
import           Data.Int
import           Interval
import           PhysMem

-- A simplified ELF writer.
--
-- Epoxy is loaded as ELF file that is only there to get memory to
-- where it needs to be in physical memory and let the boot loader
-- know where to find the kernel's entry point. So we only support a
-- tiny subset of the ELF format [1].
--
-- As an input we get a Memory structure that describes what regions
-- of physical memory are populated. Each populated region becomes one
-- segment in the ELF. Each segment needs a PHDR and all of the PHDRs
-- are pointed to by the file header (EHDR) of the ELF file.
--
-- The structure of the ELF file we are creating thus looks like this:
--
-- Offset
-- 0            +----------------------------+
--              | EHDR                       |
--              |  points to PHDRs below     |
-- ehdrSize     +----------------------------+
--              | PHDR 0                     |
--              |  points to segment0        |
--              |  initialized data          |
--              +----------------------------+
--              | PHDR 1                     |
--              |  points to segment1        |
--              |  initialized data          |
--              +----------------------------+
--              | ... more PHDRs ...         |
-- beHeaderSize +----------------------------+
--              | Segment 0 initialized data |
--              +----------------------------+
--              | Segment 1 initialized data |
--              +----------------------------+
--              | ... more segment data ...  |
--              +----------------------------+
--
-- [1] https://en.wikipedia.org/wiki/Executable_and_Linkable_Format

data BootArchitecture = RiscV64 | RiscV32

data BootClass = Class32 | Class64
  deriving (Eq)

bootClass :: BootArchitecture -> BootClass
bootClass RiscV64 = Class64
bootClass RiscV32 = Class32

data BootSegment = BootSegment
  { bsPhys :: Int64,
    bsData :: B.ByteString }

data BootElf = BootElf
  { beArch       :: BootArchitecture,
    beEntryPoint :: Int64,
    beSegments   :: [BootSegment] }

toBootSegment :: MemoryChunk -> BootSegment
toBootSegment chunk = BootSegment (fromIvl $ interval chunk) (BL.toStrict $ storage chunk)

toBootElf :: BootArchitecture -> Int64 -> Memory -> BootElf
toBootElf arch entryPoint memory = BootElf arch entryPoint (toBootSegment <$> memory)

-- TODO This is needlessly inefficient.
bsFileSize :: BootSegment -> Int64
bsFileSize s = fromIntegral $ B.length bytes - zeroesAtEnd
  where zeroesAtEnd = B.length $ B.takeWhile (== 0) $ B.reverse bytes
        bytes = bsData s

bsFileData :: BootSegment -> B.ByteString
bsFileData s = B.take (fromIntegral $ bsFileSize s) (bsData s)

bsMemSize :: BootSegment -> Int64
bsMemSize = fromIntegral . B.length . bsData

-- Header sizes

ehdrLen :: BootClass -> Int
ehdrLen Class64 = 0x40
ehdrLen Class32 = 0x34

phdrLen :: BootClass -> Int
phdrLen Class64 = 0x38
phdrLen Class32 = 0x20

shdrLen :: BootClass -> Int
shdrLen Class64 = 0x40
shdrLen Class32 = 0x28

-- The offset at which the segment data is serialized into the
-- resulting ELF. This is right after all headers.
beHeaderSize :: BootElf -> Int64
beHeaderSize elf = fromIntegral $ ehdrLen c + (phdrLen c * length (beSegments elf))
  where c = bootClass $ beArch elf

-- These are low-level representations of the program and segment
-- headers for the ELF file we want to generate.

class32Bit :: Int8
class32Bit = 0x01

class64Bit :: Int8
class64Bit = 0x02

dataLittleEndian :: Int8
dataLittleEndian = 0x01

typeExec :: Int16
typeExec = 0x02

data Ehdr = Ehdr
  { ehdrClass      :: BootClass,
    ehdrData       :: Int8,
    ehdrMachine    :: Int16,
    ehdrPhdrCount  :: Int16,
    ehdrEntryPoint :: Int64 }

data Phdr = Phdr
  { phdrClass      :: BootClass,
    phdrFileOffset :: Int64,
    phdrVirtAddr   :: Int64,
    phdrPhysAddr   :: Int64,
    phdrFileSize   :: Int64,
    phdrMemSize    :: Int64 }

data SerializedElf = SerializedElf
  { selfEhdr  :: Ehdr,
    selfPhdrs :: [Phdr],
    selfData  :: B.ByteString }

-- Given a list of lengths of strings, returns a list of offsets where
-- these strings start if they are all concatenated.
--
-- Example: [5, 10, 10] -> [5, 10, 15]
cumulativeOffsets :: Integral i => [i] -> [i]
cumulativeOffsets = reverse . snd . foldl' f (0, [])
  where f (o, l) e = (o + e, o:l)

-- Returns a list of file offsets for the data of each ELF segment.
segmentDataOffsets :: BootElf -> [Int64]
segmentDataOffsets elf = (+ beHeaderSize elf) <$> cumulativeOffsets lengths
  where lengths = bsFileSize <$> beSegments elf

toPhdr :: BootClass -> Int64 -> BootSegment -> Phdr
toPhdr c offset segment = Phdr c offset (bsPhys segment) (bsPhys segment) (bsFileSize segment) (bsMemSize segment)

elfClass :: BootArchitecture -> Int16
elfClass RiscV64 = 0x00F3
elfClass RiscV32 = 0x00F3

serializeElf :: BootElf -> SerializedElf
serializeElf elf = SerializedElf ehdr phdrs bytes
  where
    ehdr = Ehdr c dataLittleEndian (elfClass (beArch elf)) (fromIntegral (length $ beSegments elf)) (beEntryPoint elf)
    phdrs = uncurry (toPhdr c) <$> zip (segmentDataOffsets elf) (beSegments elf)
    bytes = B.concat $ bsFileData <$> beSegments elf
    c = bootClass $ beArch elf

putIntNative :: Integral w => BootClass -> w -> Put
putIntNative Class32 = putInt32le . fromIntegral
putIntNative Class64 = putInt64le . fromIntegral

putEhdr :: Ehdr -> Put
putEhdr ehdr = do
  putInt32be 0x7F454c46        -- Magic

  putInt8 (case c of
              Class32 -> 1
              Class64 -> 2)

  putInt8 dataLittleEndian
  putInt8 1                    -- Version
  putInt8 0                    -- System-V ABI

  putInt64be 0

  -- The fields below use the endianness indicated above.

  putInt16le typeExec
  putInt16le $ ehdrMachine ehdr
  putInt32le 1                 -- Version

  putIntNative c $ ehdrEntryPoint ehdr
  putIntNative c $ ehdrLen c   -- Start of Phdrs
  putIntNative c 0             -- Start of Shdrs (we have none)

  putInt32le 0                 -- Flags
  putInt16le (fromIntegral $ ehdrLen c)
  putInt16le (fromIntegral $ phdrLen c)
  putInt16le (ehdrPhdrCount ehdr)
  putInt16le (fromIntegral $ shdrLen c)
  putInt16le 0                 -- Shdr count
  putInt16le 0                 -- Stridx
  where c = ehdrClass ehdr

putPhdr :: Phdr -> Put
putPhdr phdr = do
  putInt32le 1                 -- PT_LOAD

  when (c == Class64)
    putFlags

  putIntNative c $ phdrFileOffset phdr
  putIntNative c $ phdrVirtAddr phdr
  putIntNative c $ phdrPhysAddr phdr
  putIntNative c $ phdrFileSize phdr
  putIntNative c $ phdrMemSize phdr

  when (c == Class32)
    putFlags

  putIntNative c 1             -- Alignment
  where
    c = phdrClass phdr
    putFlags = putInt32le 7    -- flags (RWX)

putElf :: SerializedElf -> BL.ByteString
putElf elf = runPut $ do
  putEhdr $ selfEhdr elf
  mapM_ putPhdr $ selfPhdrs elf
  putByteString $ selfData elf

writeElf :: Int64 -> Memory -> B.ByteString
writeElf entryPoint
  = BL.toStrict . putElf . serializeElf . toBootElf RiscV64 entryPoint . flatten
