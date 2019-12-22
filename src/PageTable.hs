module PageTable where

import           Control.Monad.State.Lazy
import           Data.Binary.Put
import           Data.Bits

import           Data.List
import qualified Data.Set                 as Set
import           Data.Tree
import           Data.Word

import           AddressSpace
import           EpoxyState
import           FrameAlloc
import           Interval

-- Build page tables without allocating backing store for page tables first.
-- Once we have constructed all page tables, we can check for identical subtrees
-- across several address spaces and avoid duplicating backing storage for them.

type PageTable = Tree Word64

entries :: PageTable -> [PageTable]
entries = subForest

entry :: PageTable -> Word64
entry = rootLabel

newtype BitSelector = BitSelector (Int, Int)

pageTableEntries :: Int
pageTableEntries = 512

addressBits :: Int -> BitSelector
addressBits level
  | level == 3 = BitSelector (9, 0)
  | level == 2 = BitSelector (18, 9)
  | level == 1 = BitSelector (27, 18)
  | level == 0 = BitSelector (36, 27)
  | otherwise = error "Page table level is out of bounds"

getBits :: BitSelector -> Word64 -> Word64
getBits (BitSelector (top, bottom)) w = shiftR w bottom .&. (shiftL 1 top - 1)

putBits :: BitSelector -> Word64 -> Word64
putBits (BitSelector (_, bottom)) w = shiftL w bottom

-- Mask virtual address bits in page numbers that are not used in page translation.
maskUnused :: Word64 -> Word64
maskUnused w = w .&. (shiftL 1 36 - 1)

ptPermissionBits :: PermissionSet -> Word64
ptPermissionBits s = 1
  .|. test Write (shiftL 1 1) 0
  .|. test User (shiftL 1 2) 0
  .|. test Execute 0 (shiftL 1 63)
  where test p yesValue noValue = if p `elem` s then yesValue else noValue

mappingToPtEntryL :: Word64 -> Integer -> Integer -> PermissionSet -> PageTable
mappingToPtEntryL virt vStart pStart perm = Node val []
  where val = shiftL ((fromInteger pStart) + virt - (maskUnused (fromInteger vStart))) frameOrder
              .|. ptPermissionBits perm

mappingToPtEntry :: Word64 -> AddressSpaceChunk -> PageTable
mappingToPtEntry virt (AddressSpaceChunk vStart (Preloaded pStart _) perm) =
  mappingToPtEntryL virt vStart pStart perm
mappingToPtEntry virt (AddressSpaceChunk vStart (Fixed pStart _) perm) =
  mappingToPtEntryL virt vStart pStart perm
mappingToPtEntry _ _ = error "Can only map preloaded mappings"

allocateLevel :: AddressSpace -> Int -> Word64 -> PageTable
allocateLevel as level fromAddress =
  Node permissionBits [ptEntry (fromIntegral i) | i <- [0..pageTableEntries-1]]
  where permissionBits
          | level == 0 = 0
          | otherwise = ptPermissionBits (Set.unions (map AddressSpace.permissions as))
        ptEntry i
          | level < 3 = nonLeafPtEntry m i
          | level == 3 = leafPtEntry m i
          | otherwise = error "Invalid level"
          where m = matchingMappings i
        -- TODO Refactor!
        nonLeafPtEntry m i
          | not (null m) = allocateLevel m (level + 1) (fromIvl (entryInterval i))
          | otherwise = Node 0 []
        leafPtEntry m i
          | length m > 1 = error "More than one mapping per page?"
          | not (null m) = mappingToPtEntry (fromIvl (entryInterval i)) (head m)
          | otherwise = Node 0 []
        sel = addressBits level
        matchingMappings i = [c | c <- as, intersects (entryInterval i) (maskUnused . fromInteger <$> pageInterval c)]
        -- The virtual address interval covered by page table entry i
        entryInterval i = fromSize (fromAddress + putBits sel i) (putBits sel 1)

constructPageTable :: AddressSpace -> PageTable
constructPageTable as = allocateLevel as 0 0

-- TODO Linear search is not efficient, but we only have a handful of page
-- tables for now.
type PageTableMap = [(PageTable, Word64)]
type DedupState a = StateT PageTableMap (State Epoxy) a

-- The un-memoized version of realizeOnePageTable with open recursion.
realizeOnePageTable :: (PageTable -> DedupState Word64) -> PageTable -> DedupState Word64
realizeOnePageTable _ (Node w []) = return w
realizeOnePageTable recurse (Node w l) = do
  ptData <- wordListToString <$> mapM recurse l
  -- TODO We need to make sure, we only get memory in the low 32-bit here.
  ptFrame <- lift (allocateFramesM 1)
  lift (writeMemoryM (frameToPhys (toInteger ptFrame)) ptData)
  return (w .|. fromIntegral (shiftL ptFrame frameOrder))
  where wordListToString wl = runPut (mapM_ putWord64le wl)

-- Recursively realize a page table with memoization.
realizePageTable :: PageTable -> DedupState Word64
realizePageTable pt = do
  m <- get
  maybe recurse return (snd <$> find ((==) pt . fst) m)
  where recurse = do
          result <- realizeOnePageTable realizePageTable pt
          modify ((:) (pt, result))
          return result

-- Realize a set of page tables and share as much of the page table structure as possible.
realizePageTables :: [PageTable] -> State Epoxy [Word64]
realizePageTables pts = evalStateT (mapM realizePageTable pts) []
