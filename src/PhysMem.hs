module PhysMem (MemoryChunk(..),
                Memory,
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

type Memory = [MemoryChunk]

writeMemory :: Int64 -> BL.ByteString -> Memory -> Memory
writeMemory _ mData mem | BL.length mData == 0 = mem
writeMemory pos mData mem = MemoryChunk (fromSize pos (fromIntegral (BL.length mData))) mData : mem

readMemory :: ByteInterval -> Memory -> BL.ByteString
readMemory bIvl [] = sameByte (size bIvl) 0
readMemory bIvl@(Interval bIvlFrom bIvlTo) (MemoryChunk mIvl@(Interval mIvlFrom _) mData : rest)
  | intersects bIvl mIvl && offset >= 0 =
      readMemory (Interval bIvlFrom mIvlFrom) rest
      <> BL.take (size available) mData
      <> readMemory (Interval availTo bIvlTo) rest
  | intersects bIvl mIvl && offset < 0 =
    BL.take (size available) (BL.drop (- offset) mData)
    <> readMemory (Interval availTo bIvlTo) rest
  | otherwise = readMemory bIvl rest
  where available@(Interval _ availTo) = intersection bIvl mIvl
        offset = mIvlFrom - bIvlFrom

-- |Given a memory description simplify its internal description.
--
-- 'writeMemory' just appends new data to the head of the list
-- representing the memory content. This function "garbage collects"
-- the overwritten memory and makes the internal representation a
-- simple list of non-overlapping memory chunks.
flatten :: Memory -> Memory
flatten m = map readIt $ join sortedIvls
  where getIvl (MemoryChunk ivl _) = ivl
        sortedIvls = sortOn fromIvl (getIvl <$> m)
        join (x:y:xs)
          | joinable x y = join (joinAdjacent x y : xs)
          | otherwise = x : join (y:xs)
        join lst = lst
        readIt ivl = MemoryChunk ivl (readMemory ivl m)

-- |Turn a memory description into a list of memory chunks.
memoryToList :: Memory -> [MemoryChunk]
memoryToList = flatten
