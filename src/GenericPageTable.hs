module GenericPageTable (GenericPageTable(..), GenericLeaf(..)) where

import           AddressSpace (PermissionSet)
import           FrameAlloc   (Frame)

-- |A leaf in the generic page table format described by
-- `GenericPageTable`.
data GenericLeaf
  = EmptyLeaf
  | ValidLeaf Frame PermissionSet
  deriving (Show)

-- |The generic page table type that should be able to represent all
-- page tables we encounter in real hardware.
--
-- Note: All computation is in page/frame numbers!
--
-- TODO It would be nice to express in the type system that each
-- NonLeaf has a fixed-size vector and not use lists here.
data GenericPageTable
  = NonLeaf [GenericPageTable]
  | Leaf GenericLeaf
  deriving (Show)
