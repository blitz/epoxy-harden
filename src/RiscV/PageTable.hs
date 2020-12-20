module RiscV.PageTable (pageTableFeatures, realizePageTable, PageTableFormat(..))
where

import           Data.Binary.Put
import           Data.Bits

import           Data.Int

import           EpoxyState
import           FrameAlloc
import           GenericPageTable  (GenericLeaf (..), GenericPageTable (..))
import           PageTableFeatures (PageTableFeatures (..))
import qualified RiscV             as R5

-- | The different page table formats we support.
data PageTableFormat = Sv39

-- |The page table features for Sv39 RISC-V paging.
pageTableFeatures :: PageTableFormat -> PageTableFeatures
pageTableFeatures Sv39 = PageTableFeatures 3 3 12 3

pteToSATP :: PageTableFormat -> Int64 -> Int64
pteToSATP Sv39 pte = (shiftR pte 10) .|. shiftL 8 60

-- |Allocate backing store for a single page table.
--
-- This function will return a page table entry.
realize1PageTable :: PageTableFormat -> GenericPageTable -> EpoxyState Int64
realize1PageTable Sv39 (Leaf leaf) = return $ case leaf of
  EmptyLeaf            -> 0
  ValidLeaf frame perm -> R5.makeLeafPte (frameToPhys frame) perm
realize1PageTable Sv39 (NonLeaf entries) = do
  ptData <- wordListToString <$> mapM (realize1PageTable Sv39) entries
  ptFrame <- allocateFramesM 1
  writeMemoryM (frameToPhys ptFrame) ptData
  return $ R5.makeNonLeafPte $ frameToPhys ptFrame
  where wordListToString wl = runPut (mapM_ putInt64le wl)

-- |A wrapper around `realize1PageTable` that returns a SATP
-- instead of a page table entry.
realizePageTable :: PageTableFormat -> GenericPageTable -> EpoxyState Int64
realizePageTable ptFmt pt = do
  pte <- realize1PageTable ptFmt pt
  -- TODO Hardcoded RISC-V Sv39 and no ASIDs
  return $ pteToSATP ptFmt pte
