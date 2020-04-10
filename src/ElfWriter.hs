module ElfWriter (bootElfFromMemory) where

import           Data.Binary.Put
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as BL
import           Data.Foldable        (foldl')
import           Data.Word
import           Interval
import           PhysMem

-- https://en.wikipedia.org/wiki/Executable_and_Linkable_Format

-- The following types describe a very simplified ELF file that is
-- only used to get the boot loader to load the right memory at the
-- right position.

data BootArchitecture = RiscV64

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
--
-- TODO: Support 32-bit ELFs.

ehdrLen :: Int
ehdrLen = 0x40

phdrLen :: Int
phdrLen = 0x38

shdrLen :: Int
shdrLen = 0x40

-- The offset at which the segment data is serialized into the
-- resulting ELF. This is right after all headers.
beHeaderSize :: BootElf -> Word64
beHeaderSize elf = fromIntegral $ ehdrLen + (phdrLen * length (beSegments elf))

-- These are low-level representations of the program and segment
-- headers for the ELF file we want to generate.

dataLittleEndian :: Word8
dataLittleEndian = 0x01

machineRiscV :: Word8
machineRiscV = 0xf3

typeExec :: Word8
typeExec = 0x02

data Ehdr = Ehdr
  { ehdrEntryPoint :: Word64,
    ehdrData       :: Word8,
    ehdrMachine    :: Word8,
    ehdrPhdrCount  :: Word16 }

data Phdr = Phdr
  { fileOffset :: Word64,
    virtAddr   :: Word64,
    physAddr   :: Word64,
    fileSize   :: Word64,
    memSize    :: Word64 }

data SerializedElf = SerializedElf
  { selfEhdr  :: Ehdr,
    selfPhdrs :: [Phdr],
    selfData  :: B.ByteString
  }

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

toPhdr :: Word64 -> BootSegment -> Phdr
toPhdr offset segment = Phdr offset (bsPhys segment) (bsPhys segment) (bsFileSize segment) (bsMemSize segment)

toSerializedElf :: BootElf -> SerializedElf
toSerializedElf elf = SerializedElf ehdr phdrs bytes
  where
    ehdr = Ehdr (beEntryPoint elf) dataLittleEndian machineRiscV (fromIntegral (length $ beSegments elf))
    phdrs = uncurry toPhdr <$> zip (segmentDataOffsets elf) (beSegments elf)
    bytes = B.concat $ bsFileData <$> beSegments elf

serializeEhdr :: Ehdr -> Put
serializeEhdr ehdr = do
  putWord32be 0x7F454c46        -- Magic
  -- TODO Use values from ehdr
  putWord32be 0x02010100        -- 64-Bit ELF Little-Endian
  putWord64be 0

  -- TODO Use values from ehdr
  putWord64be 0x0200f30001000000 -- Type, Machine, Version
  putWord64le (ehdrEntryPoint ehdr)

  putWord64le (fromIntegral ehdrLen) -- Start of Phdrs
  putWord64le 0                 -- Start of Shdrs (we have none)
  putWord32le 0                 -- Flags
  putWord16le (fromIntegral ehdrLen)
  putWord16le (fromIntegral phdrLen)
  putWord16le (ehdrPhdrCount ehdr)
  putWord16le (fromIntegral shdrLen)
  putWord16le 0                 -- Shdr count
  putWord16le 0                 -- Stridx

serializePhdr :: Phdr -> Put
serializePhdr phdr = do
  putWord32le 1                 -- PT_LOAD
  putWord32le 7                 -- flags (RWX)
  putWord64le (fileOffset phdr)
  putWord64le (virtAddr phdr)
  putWord64le (physAddr phdr)
  putWord64le (fileSize phdr)
  putWord64le (memSize phdr)
  putWord64le 1                 -- Alignment

serializeElf :: SerializedElf -> BL.ByteString
serializeElf elf = runPut $ do
  serializeEhdr $ selfEhdr elf
  mapM_ serializePhdr $ selfPhdrs elf
  putByteString $ selfData elf

bootElfFromMemory :: Word64 -> Memory -> B.ByteString
bootElfFromMemory e = BL.toStrict . serializeElf . toSerializedElf . toBootElf RiscV64 e . flatten
