-- TODO This code is not particularly nice and also hardcodes RISC-V Sv39.

module RiscV.PageTable (constructPageTable, realizePageTables) where

import           Control.Monad.State.Lazy
import           Data.Binary.Put
import           Data.Bits

import           Data.Int
import           Data.List
import           Data.Tree

import           AddressSpace
import           EpoxyState
import           FrameAlloc
import           Interval
import qualified RiscV                    as R5

-- Build page tables without allocating backing store for page tables first.
-- Once we have constructed all page tables, we can check for identical subtrees
-- across several address spaces and avoid duplicating backing storage for them.

type PageTable = Tree Int64

entries :: PageTable -> [PageTable]
entries = subForest

entry :: PageTable -> Int64
entry = rootLabel

newtype BitSelector = BitSelector (Int, Int)

pageTableEntries :: Int
pageTableEntries = 512

addressBits :: Int -> BitSelector
addressBits level
  | level == 2 = BitSelector (9, 0)
  | level == 1 = BitSelector (18, 9)
  | level == 0 = BitSelector (27, 18)
  | otherwise = error "Page table level is out of bounds"

getBits :: BitSelector -> Int64 -> Int64
getBits (BitSelector (top, bottom)) w = shiftR w bottom .&. (shiftL 1 top - 1)

putBits :: BitSelector -> Int64 -> Int64
putBits (BitSelector (_, bottom)) w = shiftL w bottom

-- Mask virtual address bits in page numbers that are not used in page translation.
maskUnused :: Int64 -> Int64
maskUnused w = w .&. (shiftL 1 27 - 1)

mappingToPtEntryL :: Int64 -> Int64 -> Int64 -> PermissionSet -> PageTable
mappingToPtEntryL virt vStart pStart perm = Node val []
  where val = R5.makeLeafPte vStartAddr perm
        vStartAddr = shiftL (pStart + virt - maskUnused vStart) frameOrder

mappingToPtEntry :: Int64 -> AddressSpaceChunk -> PageTable
mappingToPtEntry virt (AddressSpaceChunk vStart (Preloaded pStart _) perm) =
  mappingToPtEntryL virt vStart pStart perm
mappingToPtEntry virt (AddressSpaceChunk vStart (Fixed pStart _) perm) =
  mappingToPtEntryL virt vStart pStart perm
mappingToPtEntry _ _ = error "Can only map preloaded mappings"

allocateLevel :: AddressSpace -> Int -> Int64 -> PageTable
allocateLevel as level fromAddress =
  Node 0 [ptEntry (fromIntegral i) | i <- [0..pageTableEntries-1]]
  where ptEntry i
          | level < 2 = nonLeafPtEntry m i
          | level == 2 = leafPtEntry m i
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
        matchingMappings i = [c | c <- as, intersects (entryInterval i) (maskUnused <$> pageInterval c)]
        -- The virtual address interval covered by page table entry i
        entryInterval i = fromSize (fromAddress + putBits sel i) (putBits sel 1)

-- | Create page table structures for an address space.
--
-- All page tables that are created have to be realized with
-- 'realizePageTables' later.
constructPageTable :: AddressSpace -> PageTable
constructPageTable as = allocateLevel as 0 0

-- TODO Linear search is not efficient, but we only have a handful of page
-- tables for now.
type PageTableMap = [(PageTable, Int64)]
type DedupState a = StateT PageTableMap (State Epoxy) a

-- The un-memoized version of realizeOnePageTable with open recursion.
realizeOnePageTable :: (PageTable -> DedupState Int64) -> PageTable -> DedupState Int64
realizeOnePageTable _ (Node w []) = return w
realizeOnePageTable recurse (Node w l) = do
  ptData <- wordListToString <$> mapM recurse l
  ptFrame <- lift (allocateFramesM 1)
  lift (writeMemoryM (frameToPhys ptFrame) ptData)
  return $ R5.makeNonLeafPte $ fromIntegral $ shiftL ptFrame frameOrder
  where wordListToString wl = runPut (mapM_ putInt64le wl)

-- Recursively realize a page table with memoization.
realizePageTable :: PageTable -> DedupState Int64
realizePageTable pt = do
  m <- get
  maybe recurse return (snd <$> find ((==) pt . fst) m)
  where recurse = do
          result <- realizeOnePageTable realizePageTable pt
          modify ((:) (pt, result))
          return result

-- |Realize a set of page tables by allocating physical memory for them.
--
-- An instance of 'PageTable' does not have backing store allocated
-- for itself. This happens in the realization stage in this
-- function. 'realizePageTables' takes a list of page tables to
-- exploit opportunities to share page table structures.
realizePageTables :: [PageTable] -> State Epoxy [Frame]
realizePageTables pts = do
  ptes <- evalStateT (mapM realizePageTable pts) []
  return $ map R5.pteFrame ptes
