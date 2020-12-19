{-# LANGUAGE OverloadedStrings #-}
module DtbConvert
  (convert)
where

import qualified Data.ByteString    as B
import           Data.Dtb
import           Data.List          (nub)
import           Data.Maybe         (catMaybes, mapMaybe)
import qualified Data.Text          as T
import           Data.Word          (Word32, Word64)
import qualified Interval           as I
import           MachineDescription

filterReg :: [Property] -> [Property]
filterReg = filter ((== "reg") . propName)

memResToTuple :: MemoryReservation -> (Word64, Word64)
memResToTuple (MemoryReservation addr size) = (addr, size)

freeMemory :: Dtb -> Either T.Text [I.Interval Word64]
freeMemory dtb = case addressSizeCells "/memory" dtb of
  Just regSize -> Right $ fromSize <$> regPairs regSize
  Nothing      -> Left "Missing address size information"
  where
    regPairs :: (Word32, Word32) -> [(Word64, Word64)]
    regPairs regSize = concat $ catMaybes $ asRegList regSize <$> regProps
    fromSize :: (Word64, Word64) -> I.Interval Word64
    fromSize = uncurry I.fromSize
    regProps = filterReg $ concat $ properties <$> memoryNodes
    memoryNodes = filter (matchesPath "memory") $ children $ rootNode dtb

-- | Return a list of reserved memory regions.
--
-- At least for the ULX3S, reserved memory is described by reg
-- properties of children of the /reserved-memory node. Then there is
-- also the extra reserved memory section in the device tree that we
-- need to take care of.
allReservedMemory :: Dtb -> [I.Interval Word64]
allReservedMemory dtb = uncurry I.fromSize <$> ((memResToTuple <$> reservedMemory dtb) ++ case maybeRsvd of
  Just rsvd -> rsvd
  Nothing   -> [])
  where
    maybeRsvd :: Maybe [(Word64, Word64)]
    maybeRsvd =
      do
        regSize <- addressSizeCells "/reserved-memory" dtb
        node <- lookupNode "/reserved-memory" dtb
        let props = concat $ filterReg <$> properties <$> children node
        return $ concat $ mapMaybe (asRegList regSize) props

-- |Subtracts each interval of the second set from the first.
subtractAll :: Ord a => [I.Interval a] -> [I.Interval a] -> [I.Interval a]
subtractAll as bs = concat $ subtractAll <$> as
  where
    -- TODO nub is needlessly inefficient: O(n^2)
    subtractAll a = nub $ concat $ I.subtract a <$> bs

toMemoryMapEntry :: MemoryMapType -> I.Interval Word64 -> MemoryMapEntry
toMemoryMapEntry typ ivl@(I.Interval from _) =
  MemoryMapEntry (fromIntegral from) (fromIntegral (I.size ivl)) typ

toMachineDescription :: Dtb -> Either T.Text MachineDescription
toMachineDescription dtb = do
  freeMem <- freeMemory dtb
  let reservedMem = allReservedMemory dtb
  let actuallyFree = subtractAll freeMem reservedMem
  return $ MachineDescription (toMemoryMapEntry Available <$> actuallyFree)

-- https://stackoverflow.com/questions/63425283/how-can-i-print-encode-a-value-into-dhall
convert :: B.ByteString -> Either T.Text MachineDescription
convert dta = parseDtb dta >>= toMachineDescription
