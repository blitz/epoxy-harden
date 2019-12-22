module ElfWriter where

import           Data.Binary.Put
import           Data.Bits
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as BL
import           Data.Word
import           Interval
import           PhysMem

-- https://en.wikipedia.org/wiki/Executable_and_Linkable_Format

data Ehdr = Ehdr
  { entryPoint   :: Word32,
    segmentCount :: Int }

data Phdr = Phdr
  { fileOffset :: Word32,
    virtAddr   :: Word32,
    physAddr   :: Word32,
    fileSize   :: Word32,
    memSize    :: Word32 }

multibootHeaderLen :: Int
multibootHeaderLen = 0xc

ehdrLen :: Int
ehdrLen = 0x34

phdrLen :: Int
phdrLen = 0x20

serializeEhdr :: Ehdr -> Put
serializeEhdr ehdr = do
  putWord32be 0x7F454c46        -- Magic
  putWord32be 0x01010100        -- 32-Bit ELF Little-Endian
  putWord64be 0

  putWord64be 0x0200030001000000 -- Type, Machine, Version
  putWord32le (entryPoint ehdr)

  putWord32le (fromIntegral (ehdrLen + multibootHeaderLen)) -- Start of Phdrs
  putWord32le 0                 -- Start of Shdrs (we have none)
  putWord32le 0                 -- Flags
  putWord16le (fromIntegral ehdrLen)
  putWord16le (fromIntegral phdrLen)
  putWord16le (fromIntegral (segmentCount ehdr))
  putWord16le 0x28              -- Shdr size
  putWord16le 0                 -- Shdr count
  putWord16le 0                 -- Stridx

serializePhdr :: Phdr -> Put
serializePhdr phdr = do
  putWord32le 1                 -- PT_LOAD
  putWord32le (fileOffset phdr)
  putWord32le (virtAddr phdr)
  putWord32le (physAddr phdr)
  putWord32le (fileSize phdr)
  putWord32le (memSize phdr)
  putWord32le 7                 -- flags (RWX)
  putWord32le 1                 -- Alignment

serializeMultibootHeader :: Put
serializeMultibootHeader = do
  putWord32le 0x1BADB002        -- Magic
  putWord32le 0                 -- Flags
  putWord32le (1 + complement 0x1badb002) -- "Checksum"

bootElfFromMemory :: Word32 -> Memory -> B.ByteString
bootElfFromMemory elfEntryPoint mem = BL.toStrict (runPut (do
                                                           serializeEhdr (Ehdr elfEntryPoint (length flattenedMem))
                                                           serializeMultibootHeader
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
    headerLen = ehdrLen + multibootHeaderLen + length memoryIntervals * phdrLen
    memoryIntervals = interval <$> flattenedMem
    flattenedMem = flatten mem
