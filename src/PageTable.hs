{-# LANGUAGE ScopedTypeVariables #-}
module PageTable (resolvePageTableFunction) where

import           Control.Exception (assert)
import           Data.Bits

import           AddressSpace
import           Data.List         (find, intercalate)
import           EpoxyState
import           FrameAlloc
import           GenericPageTable  (GenericLeaf (..), GenericPageTable (..))
import           Interval
import           PageTableFeatures (PageTableFeatures (..))
import qualified RiscV.PageTable   as R5

-- |The number of entries in a single page table as a power of two.
pageTableEntryBits :: PageTableFeatures -> Int
pageTableEntryBits cfg = (pageBits cfg) - (entryBits cfg)

-- |The number of entries in a page table.
pageTableEntries :: PageTableFeatures -> Int
pageTableEntries = shiftL 1 . pageTableEntryBits

-- |The size of virtual address space a page table _entry_ covers at a
-- given level in _pages_.
pageTableEntrySize :: PageTableFeatures -> Int -> Page
pageTableEntrySize cfg level = shiftL 1 $ (maxLevels cfg - level - 1) * pageTableEntryBits cfg

-- |Tests whether a given level can be a leaf level.
isLeafLevel :: PageTableFeatures -> Int -> Bool
isLeafLevel cfg level = maxLevels cfg - level <= leafLevels cfg

-- |Tests whether a given level is the last level of the page table.
isLastLevel :: PageTableFeatures -> Int -> Bool
isLastLevel cfg = (== maxLevels cfg - 1)

-- |Mask virtual address bits in page numbers that are not used in page translation.
pageNumberMaskUnused :: PageTableFeatures -> Page -> Page
pageNumberMaskUnused cfg pn = pn .&. (shiftL 1 (maxLevels cfg * pageTableEntryBits cfg) - 1)

leafFromMappingL :: PageTableFeatures ->  Page -> Page -> Frame -> PermissionSet -> GenericLeaf
leafFromMappingL cfg pn pnStart fStart perm = ValidLeaf (fStart + pn - pageNumberMaskUnused cfg pnStart) perm

-- |Create a page table leaf node from a mapping.
leafFromMapping :: PageTableFeatures -> Page -> AddressSpaceChunk -> GenericLeaf
leafFromMapping cfg virt (AddressSpaceChunk vStart (Preloaded pStart _) perm) =
  leafFromMappingL cfg virt vStart pStart perm
leafFromMapping cfg virt (AddressSpaceChunk vStart (Fixed pStart _) perm) =
  leafFromMappingL cfg virt vStart pStart perm
leafFromMapping _ _ _ = error "Can only mapped fixed or preloaded address space chunks"

-- |Compute a page table entry for the given level for a given
-- starting page number. Level -1 is the level that corresponds to the
-- page table pointer register (SATP, CR3, ...).
pageTableLevel :: PageTableFeatures -> AddressSpace -> Int -> Page -> GenericPageTable
pageTableLevel cfg as level fromAddress
  -- If the previous level can contain leaves and we have a mapping
  -- that covers everything, we generate a leaf.
  --
  -- XXX We need to check whether the alignment of the physical
  -- backing store is correct. Otherwise, we cannot generate a leaf!
  | isLeafLevel cfg level && not (null matchingMappings)
  = assert (length matchingMappings == 1)
    Leaf $ leafFromMapping cfg fromAddress $ head matchingMappings
  -- If we can generate leaves and there is nothing interesting in the
  -- address space left, we generate an empty leaf.
  | isLeafLevel cfg level && null intersectingMappings
  = Leaf EmptyLeaf
  -- If this the last level we have to generate a leaf no matter
  -- what. The case where we have a mapping is already covered above.
  | isLastLevel cfg level
  = assert (null matchingMappings)
    Leaf EmptyLeaf
  -- The usual case will be to generate a table and recurse.
  | otherwise =
    assert (not (isLastLevel cfg level))
    NonLeaf $ pageTableLevel cfg as (level + 1) <$> entryFromAddresses
  where
    -- Mappings that completely cover by the current entry.
    matchingMappings = filter (\ck -> isCovered (maskedPageInterval ck) entryInterval) as
    -- The mappings that intersect this entry.
    intersectingMappings = filter (\ck -> intersects (maskedPageInterval ck) entryInterval) as
    -- Generates the page interval, but masks any address bits that
    -- are not used in the address translation.
    maskedPageInterval ck = pageNumberMaskUnused cfg <$> pageInterval ck
    -- The interval that this whole level covers.
    entryInterval = fromSize fromAddress $ entrySize
    -- The size of virtual address space covered by an individual entry.
    entrySize = pageTableEntrySize cfg level
    -- The size of an entry in the next level of the page table.
    nextEntrySize = pageTableEntrySize cfg $ level + 1
    -- Addresses of each individual page table entry.
    entryFromAddresses = take (pageTableEntries cfg) [fromAddress, fromAddress+nextEntrySize..]

toPageTable :: PageTableFeatures -> AddressSpace -> GenericPageTable
toPageTable cfg as = assert (case pt of
                               Leaf _    -> False
                               NonLeaf _ -> True)
                     pt
  where pt = pageTableLevel cfg as (negate 1) 0

type ConstructFunction = AddressSpace -> GenericPageTable
type RealizeFunction = [GenericPageTable] -> EpoxyState [Frame]

-- TODO Page table deduplication could be implemented by adding a new
-- monad that allocates backing store with predefined content.

pageTableFormats :: [(String, ConstructFunction, RealizeFunction)]
pageTableFormats = [
  ("riscv-sv39", toPageTable $ R5.pageTableFeatures R5.Sv39, mapM $ R5.realizePageTable R5.Sv39)
  ]

supportedPageTableFormats :: [String]
supportedPageTableFormats = (\(n, _, _) -> n) <$> pageTableFormats

-- | Return functions to construct and realize page tables of a given
-- type.
resolvePageTableFunction :: String -> (ConstructFunction, RealizeFunction)
resolvePageTableFunction format =
  case find ((== format) . (\(n, _, _) -> n)) pageTableFormats of
      Just (_, constructFn, realizeFn) -> (constructFn, realizeFn)
      Nothing -> error ("Unsupported page table format. We support: " ++ (intercalate ", " supportedPageTableFormats))
