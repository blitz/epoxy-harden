module PageTable (constructPageTable, realizePageTables) where

import           Control.Monad.State.Lazy

import           AddressSpace
import           EpoxyState
import qualified RiscV.PageTable as R5
import           FrameAlloc

-- TODO We need to implement support for different formats here. We
-- also need to separate the deduplication from the actual page table
-- creation.

-- | Create page table structures for an address space.
--
-- All page tables that are created have to be realized with
-- 'realizePageTables' later.
constructPageTable :: AddressSpace -> R5.PageTable
constructPageTable = R5.constructPageTable

-- |Realize a set of page tables by allocating physical memory for them.
--
-- An instance of 'PageTable' does not have backing store allocated
-- for itself. This happens in the realization stage in this
-- function. 'realizePageTables' takes a list of page tables to
-- exploit opportunities to share page table structures.
realizePageTables :: [R5.PageTable] -> State Epoxy [Frame]
realizePageTables = R5.realizePageTables
