module PageTableFeatures (PageTableFeatures(..)) where

-- |A type to describe the page table format for a specific hardware
-- architecture.
data PageTableFeatures = PageTableFeatures
  { maxLevels  :: Int -- ^ The maximum depth of a page table.
  , leafLevels :: Int -- ^ The number of levels that can be leaf levels.

  , pageBits   :: Int
  -- ^ The size in bytes of the smallest page in virtual memory as a
  -- power of 2 (i.e. 12 means 4096 bytes).

  , entryBits  :: Int
  -- ^ The size of a page table entry in bytes as a power of two. We only
  -- support 2 (4-byte entries) or 3 (8-byte entries) here.
  }
  deriving (Show)
