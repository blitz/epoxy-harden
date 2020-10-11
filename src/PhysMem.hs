module PhysMem (MemoryChunk(..),
                Memory,
                emptyMemory,
                writeMemory,
                readMemory,
                memoryToList) where

import qualified Data.ByteString.Lazy as BL
import           Data.Int             (Int64)
import           Data.List
import           FrameAlloc
import           Interval
import           Util                 (sameByte)

data MemoryChunk = MemoryChunk
  { interval :: ByteInterval,
    storage  :: BL.ByteString }
  deriving (Show)

newtype Memory = Memory [MemoryChunk]
  deriving (Show)

-- |The empty memory where all memory is initialized with zero.
emptyMemory :: Memory
emptyMemory = Memory []

writeMemory :: Int64 -> BL.ByteString -> Memory -> Memory
writeMemory _ mData mem | BL.length mData == 0 = mem
writeMemory pos mData (Memory mem) = Memory (MemoryChunk (fromSize pos $ fromIntegral $ BL.length mData) mData : mem)

readMemory_ :: ByteInterval -> [MemoryChunk] -> BL.ByteString
readMemory_ bIvl [] = sameByte (size bIvl) 0
readMemory_ bIvl@(Interval bIvlFrom bIvlTo) (MemoryChunk mIvl@(Interval mIvlFrom _) mData : rest)
  | intersects bIvl mIvl && offset >= 0 =
      readMemory_ (Interval bIvlFrom mIvlFrom) rest
      <> BL.take (size available) mData
      <> readMemory_ (Interval availTo bIvlTo) rest
  | intersects bIvl mIvl && offset < 0 =
    BL.take (size available) (BL.drop (- offset) mData)
    <> readMemory_ (Interval availTo bIvlTo) rest
  | otherwise = readMemory_ bIvl rest
  where available@(Interval _ availTo) = intersection bIvl mIvl
        offset = mIvlFrom - bIvlFrom

readMemory :: ByteInterval -> Memory -> BL.ByteString
readMemory bIvl (Memory mem) = readMemory_ bIvl mem

-- |Given a memory description simplify its internal description.
--
-- 'writeMemory' just appends new data to the head of the list
-- representing the memory content. 'flatten' "garbage collects" the
-- overwritten memory and makes the internal representation a simple
-- list of non-overlapping memory chunks.
flatten :: Memory -> [MemoryChunk]
flatten (Memory m) = map readIt $ join sortedIvls
  where getIvl (MemoryChunk ivl _) = ivl
        sortedIvls = sortOn fromIvl (getIvl <$> m)
        join (x:y:xs)
          | joinable x y = join (joinAdjacent x y : xs)
          | otherwise = x : join (y:xs)
        join lst = lst
        readIt ivl = MemoryChunk ivl (readMemory_ ivl m)

-- |Turn a memory description into a list of memory chunks.
memoryToList :: Memory -> [MemoryChunk]
memoryToList = flatten
