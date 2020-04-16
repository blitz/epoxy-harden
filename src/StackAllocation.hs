module StackAllocation ( allocateStacks ) where

import           ApplicationDescription
import           Data.List              (partition)
import qualified Data.Set               as Set
import           Numeric.Natural

-- We start the stack area above where static code can be linked.
stackAreaStart = 0x20000000

-- How much stack space do we allocate per stack.
stackSize = 0x4000

-- How much space do we keep between stacks.
stackGuardSize = 0x4000

-- Map a list in groups.
--
-- Partition a list in groups and then map each group as a whole. The
-- resulting lists are concatenated again.
mapGroupedBy :: Ord b => (a -> b) -> ([a] -> [a]) -> [a] -> [a]
mapGroupedBy groupFn mapFn input = concatMap (\k -> mapFn $ filter ((== k) . groupFn) input) allKeys
  where allKeys = Set.toList $ Set.fromList $ groupFn <$> input

-- Return the stack pointer for the nth thread.
stackEndFromId :: Int -> Natural
stackEndFromId n = (fromIntegral n * (stackSize + stackGuardSize)) + stackSize

stackEnds :: [Natural]
stackEnds = stackEndFromId <$> [0..]

hasAllocatedStack :: KObject -> Bool
hasAllocatedStack KObject{impl=Thread{stack=Auto}} = False
hasAllocatedStack KObject{impl=Thread{stack=s}} = True
hasAllocatedStack _ = error "not a thread object"

-- Allocate stacks for the threads of a single process.
allocateProcessStacks :: [KObject] -> [KObject]
allocateProcessStacks threads = allocatedThreads <> fixedThreads
  where
    fixedThreads = fixStack <$> zip nonAllocatedThreads stackEnds
    fixStack (KObject{gid=g, impl=Thread{process=p, stack=Auto}}, stackEnd) =
      KObject g (Thread p (Fixed stackEnd))
    fixStack _ = error "Unexpected thread object"
    (allocatedThreads, nonAllocatedThreads) = partition hasAllocatedStack threads

-- Allocate stacks for threads (from any number of processes).
allocateStacksForThreads :: [KObject] -> [KObject]
allocateStacksForThreads = mapGroupedBy threadProcessGid allocateProcessStacks
  where threadProcessGid KObject{impl=Thread{process=p}} = p

-- Allocate stacks for all threads with initial stack pointer set to
-- Auto.
--
-- We do this by adding an anonymous shared memory region for each
-- thread and pointing the stack pointer to it.
--
-- TODO: Make me generic and use GenericApplicationDescription!
allocateStacks :: ApplicationDescription -> ApplicationDescription
allocateStacks a = ApplicationDescription $ allNonThreads <> allUpdatedThreads
  where
    (allThreads, allNonThreads) = partition isThread $ kobjects a
    allUpdatedThreads = allocateStacksForThreads allThreads
