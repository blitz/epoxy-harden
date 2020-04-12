module ElfWriter (bootElfFromMemory) where

import           Control.Monad        (when)
import           Data.Binary.Put
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as BL
import           Data.Foldable        (foldl')
import           Data.Word
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
  { bsPhys :: Word64,
    bsData :: B.ByteString }

data BootElf = BootElf
  { beArch       :: BootArchitecture,
    beEntryPoint :: Word64,
    beSegments   :: [BootSegment] }

toBootSegment :: MemoryChunk -> BootSegment
toBootSegment chunk = BootSegment (fromInteger (fromIvl $ interval chunk)) (BL.toStrict $ storage chunk)

toBootElf :: BootArchitecture -> Word64 -> Memory -> BootElf
toBootElf arch entryPoint memory = BootElf arch entryPoint (toBootSegment <$> memory)

-- TODO This is needlessly inefficient.
bsFileSize :: BootSegment -> Word64
bsFileSize s = fromIntegral $ B.length bytes - zeroesAtEnd
  where zeroesAtEnd = B.length $ B.takeWhile (== 0) $ B.reverse bytes
        bytes = bsData s

bsFileData :: BootSegment -> B.ByteString
bsFileData s = B.take (fromIntegral $ bsFileSize s) (bsData s)

bsMemSize :: BootSegment -> Word64
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
beHeaderSize :: BootElf -> Word64
beHeaderSize elf = fromIntegral $ ehdrLen c + (phdrLen c * length (beSegments elf))
  where c = bootClass $ beArch elf

-- These are low-level representations of the program and segment
-- headers for the ELF file we want to generate.

class32Bit :: Word8
class32Bit = 0x01

class64Bit :: Word8
class64Bit = 0x02

dataLittleEndian :: Word8
dataLittleEndian = 0x01

typeExec :: Word16
typeExec = 0x02

data Ehdr = Ehdr
  { ehdrClass      :: BootClass,
    ehdrData       :: Word8,
    ehdrMachine    :: Word16,
    ehdrPhdrCount  :: Word16,
    ehdrEntryPoint :: Word64 }

data Phdr = Phdr
  { phdrClass      :: BootClass,
    phdrFileOffset :: Word64,
    phdrVirtAddr   :: Word64,
    phdrPhysAddr   :: Word64,
    phdrFileSize   :: Word64,
    phdrMemSize    :: Word64 }

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
segmentDataOffsets :: BootElf -> [Word64]
segmentDataOffsets elf = (+ beHeaderSize elf) <$> cumulativeOffsets lengths
  where lengths = bsFileSize <$> beSegments elf

toPhdr :: BootClass -> Word64 -> BootSegment -> Phdr
toPhdr c offset segment = Phdr c offset (bsPhys segment) (bsPhys segment) (bsFileSize segment) (bsMemSize segment)

elfClass :: BootArchitecture -> Word16
elfClass RiscV64 = 0x00F3
elfClass RiscV32 = 0x00F3

serializeElf :: BootElf -> SerializedElf
serializeElf elf = SerializedElf ehdr phdrs bytes
  where
    ehdr = Ehdr c dataLittleEndian (elfClass (beArch elf)) (fromIntegral (length $ beSegments elf)) (beEntryPoint elf)
    phdrs = uncurry (toPhdr c) <$> zip (segmentDataOffsets elf) (beSegments elf)
    bytes = B.concat $ bsFileData <$> beSegments elf
    c = bootClass $ beArch elf

putWordNative :: Integral w => BootClass -> w -> Put
putWordNative Class32 = putWord32le . fromIntegral
putWordNative Class64 = putWord64le . fromIntegral

putEhdr :: Ehdr -> Put
putEhdr ehdr = do
  putWord32be 0x7F454c46        -- Magic

  putWord8 (case c of
              Class32 -> 1
              Class64 -> 2)

  putWord8 dataLittleEndian
  putWord8 1                    -- Version
  putWord8 0                    -- System-V ABI

  putWord64be 0

  -- The fields below use the endianness indicated above.

  putWord16le typeExec
  putWord16le $ ehdrMachine ehdr
  putWord32le 1                 -- Version

  putWordNative c $ ehdrEntryPoint ehdr
  putWordNative c $ ehdrLen c   -- Start of Phdrs
  putWordNative c 0             -- Start of Shdrs (we have none)

  putWord32le 0                 -- Flags
  putWord16le (fromIntegral $ ehdrLen c)
  putWord16le (fromIntegral $ phdrLen c)
  putWord16le (ehdrPhdrCount ehdr)
  putWord16le (fromIntegral $ shdrLen c)
  putWord16le 0                 -- Shdr count
  putWord16le 0                 -- Stridx
  where c = ehdrClass ehdr

putPhdr :: Phdr -> Put
putPhdr phdr = do
  putWord32le 1                 -- PT_LOAD

  when (c == Class64)
    putFlags

  putWordNative c $ phdrFileOffset phdr
  putWordNative c $ phdrVirtAddr phdr
  putWordNative c $ phdrPhysAddr phdr
  putWordNative c $ phdrFileSize phdr
  putWordNative c $ phdrMemSize phdr

  when (c == Class32)
    putFlags

  putWordNative c 1             -- Alignment
  where
    c = phdrClass phdr
    putFlags = putWord32le 7    -- flags (RWX)

putElf :: SerializedElf -> BL.ByteString
putElf elf = runPut $ do
  putEhdr $ selfEhdr elf
  mapM_ putPhdr $ selfPhdrs elf
  putByteString $ selfData elf

bootElfFromMemory :: Word64 -> Memory -> B.ByteString
bootElfFromMemory entryPoint
  = BL.toStrict . putElf . serializeElf . toBootElf RiscV64 entryPoint . flatten
