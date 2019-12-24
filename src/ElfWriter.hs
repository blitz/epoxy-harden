module ElfWriter where

import           Data.Binary.Put
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as BL
import           Data.Word
import           Interval
import           PhysMem

-- https://en.wikipedia.org/wiki/Executable_and_Linkable_Format

data Ehdr = Ehdr
  { entryPoint   :: Word64,
    segmentCount :: Int }

data Phdr = Phdr
  { fileOffset :: Word64,
    virtAddr   :: Word64,
    physAddr   :: Word64,
    fileSize   :: Word64,
    memSize    :: Word64 }

ehdrLen :: Int
ehdrLen = 0x40

phdrLen :: Int
phdrLen = 0x38

serializeEhdr :: Ehdr -> Put
serializeEhdr ehdr = do
  putWord32be 0x7F454c46        -- Magic
  putWord32be 0x02010100        -- 64-Bit ELF Little-Endian
  putWord64be 0

  putWord64be 0x0200f30001000000 -- Type, Machine, Version
  putWord64le (entryPoint ehdr)

  putWord64le (fromIntegral ehdrLen) -- Start of Phdrs
  putWord64le 0                 -- Start of Shdrs (we have none)
  putWord32le 0                 -- Flags
  putWord16le (fromIntegral ehdrLen)
  putWord16le (fromIntegral phdrLen)
  putWord16le (fromIntegral (segmentCount ehdr))
  putWord16le 0x40              -- Shdr size
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

bootElfFromMemory :: Word64 -> Memory -> B.ByteString
bootElfFromMemory elfEntryPoint mem = BL.toStrict (runPut (do
                                                           serializeEhdr (Ehdr elfEntryPoint (length flattenedMem))
                                                           mapM_ (serializePhdr . toPhdr) (zip [0..] memoryIntervals)
                                                           mapM_ (putLazyByteString . storage) flattenedMem))
  where
    toPhdr (idx, ivl) = let wordIvl = fromIntegral <$> ivl in
      Phdr { fileOffset = fromInteger (filePos idx),
             virtAddr = fromIvl wordIvl,
             physAddr = fromIvl wordIvl,
             fileSize = size wordIvl,
             memSize = size wordIvl }
    filePos 0 = toInteger headerLen
    filePos n = filePos (n - 1) + size (memoryIntervals !! (n - 1))
    headerLen = ehdrLen + length memoryIntervals * phdrLen
    memoryIntervals = interval <$> flattenedMem
    flattenedMem = flatten mem
