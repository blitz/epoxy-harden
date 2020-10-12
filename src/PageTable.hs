module PageTable (constructPageTable, realizePageTables) where

import           RiscV.PageTable

-- This module only re-exports the RiscV page table functions, but we
-- need to implement support for different formats here. We also need
-- to separate the deduplication from the actual page table creation.
