module StackAllocation ( allocateStacks ) where

import           ApplicationDescription

-- We start the stack area above where static code can be linked.
stackAreaStart = 0x20000000

-- How much stack space do we allocate per stack.
stackSize = 0x4000

-- How much space do we keep between stacks.
stackGuardSize = 0x4000

-- Allocate stacks for all threads with initial stack pointer set to
-- Auto.
--
-- We do this by adding an anonymous shared memory region for each
-- thread and pointing the stack pointer to it.
--
-- TODO: Implement me!
allocateStacks :: ApplicationDescription -> ApplicationDescription
allocateStacks = id
