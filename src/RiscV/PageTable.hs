module RiscV.PageTable (pageTableFeatures, realizePageTable, PageTableFormat(..))
where

import           Control.Exception    (assert)
import           Data.Binary.Put
import           Data.Bits            (shiftL, shiftR, (.|.))
import qualified Data.ByteString.Lazy as BL
import           Data.Int             (Int64)
import           EpoxyState
import           FrameAlloc
import           GenericPageTable     (GenericLeaf (..), GenericPageTable (..))
import           PageTableFeatures    (PageTableFeatures (..))
import qualified RiscV                as R5

-- | The different page table formats we support.
data PageTableFormat = Sv32 | Sv39

-- |The page table features for Sv39 RISC-V paging.
pageTableFeatures :: PageTableFormat -> PageTableFeatures
pageTableFeatures Sv39 = PageTableFeatures 3 3 12 3
pageTableFeatures Sv32 = PageTableFeatures 2 2 12 2

pteToSATP :: PageTableFormat -> Int64 -> Int64
pteToSATP Sv39 pte = (pte `shiftR` 10) .|. (8 `shiftL` 60)
pteToSATP Sv32 pte = (pte `shiftR` 10) .|. (1 `shiftL` 31)

-- |Allocate backing store for a single page table.
--
-- This function will return a page table entry.
realize1PageTable :: PageTableFormat -> GenericPageTable -> EpoxyState Int64
realize1PageTable _ (Leaf leaf) = return $ case leaf of
  EmptyLeaf            -> 0
  ValidLeaf frame perm -> R5.makeLeafPte (frameToPhys frame) perm
realize1PageTable arch (NonLeaf entries) = do
  ptData <- wordListToString <$> mapM (realize1PageTable arch) entries
  ptFrame <- allocateFramesM 1
  writeMemoryM (frameToPhys ptFrame)
    $ assert (BL.length ptData == 4096) ptData
  return $ R5.makeNonLeafPte $ frameToPhys ptFrame
  where
    wordListToString wl = runPut (mapM_ putPte wl)
    putPte = case arch of
      Sv32 -> putInt32le . fromIntegral
      Sv39 -> putInt64le

-- |A wrapper around `realize1PageTable` that returns a SATP
-- instead of a page table entry.
realizePageTable :: PageTableFormat -> GenericPageTable -> EpoxyState Int64
realizePageTable ptFmt pt = do
  pte <- realize1PageTable ptFmt pt
  return $ pteToSATP ptFmt pte
