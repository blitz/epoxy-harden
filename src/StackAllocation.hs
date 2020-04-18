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

-- We don't allow execution on stacks.
stackPermission = RW

-- Map a list in groups.
--
-- Partition a list in groups and then map each group as a whole.
mapGroupedBy :: Ord b => (a -> b) -> (b -> [a] -> c) -> [a] -> [c]
mapGroupedBy groupFn mapFn input = map (\k -> mapFn k $ filter ((== k) . groupFn) input) allKeys
  where allKeys = Set.toList $ Set.fromList $ groupFn <$> input

-- Return the beginning of the stack region for the nth thread.
stackStartFromId :: Int -> Natural
stackStartFromId n = stackAreaStart + (fromIntegral n * (stackSize + stackGuardSize))

-- Return the stack pointer for the nth thread.
stackEndFromId :: Int -> Natural
stackEndFromId n = stackSize + stackStartFromId n

stackStarts :: [Natural]
stackStarts = stackStartFromId <$> [0..]

stackEnds :: [Natural]
stackEnds = stackEndFromId <$> [0..]

hasAllocatedStack :: KObject -> Bool
hasAllocatedStack KObject{impl=Thread{stack=Auto}} = False
hasAllocatedStack KObject{impl=Thread{stack=s}} = True
hasAllocatedStack _ = error "not a thread object"

-- Allocate stacks for the threads of a single process.
--
-- Returns new thread objects and any new address space elements that
-- need to be added to the process.
allocateProcessStacks :: Natural -> [KObject] -> ([KObject], Natural, [AddressSpaceDescElem])
allocateProcessStacks procGid threads = (allocatedThreads <> fixedThreads,
                                         procGid,
                                         stackVaToSharedMem <$> take (length fixedThreads) stackStarts)
  where
    stackVaToSharedMem va = SharedMemory (AnonymousMemory stackSize) va RW
    fixedThreads = fixStack <$> zip nonAllocatedThreads stackEnds
    fixStack (KObject{gid=g, impl=Thread{process=p, stack=Auto}}, stackEnd) =
      KObject g (Thread p (Fixed stackEnd))
    fixStack _ = error "Unexpected thread object"
    (allocatedThreads, nonAllocatedThreads) = partition hasAllocatedStack threads

-- Allocate stacks for threads (from any number of processes).
--
-- Returns a new set of threads and a list of address space elements
-- that need to be added to specific processes.
allocateStacksForThreads :: [KObject] -> ([KObject], [(Natural, [AddressSpaceDescElem])])
allocateStacksForThreads kobjs = foldr combine ([], []) mapResult
  where
    mapResult = mapGroupedBy threadProcessGid allocateProcessStacks kobjs
    combine (newObjs, procGid, newAsElem) (objs, updates) = (objs ++ newObjs, (procGid, newAsElem):updates)
    threadProcessGid KObject{impl=Thread{process=p}} = p


updateProcess :: [(Natural, [AddressSpaceDescElem])] -> KObject -> KObject
updateProcess updates p@KObject{gid=g} = transformProcessAddressSpace p appendNew
  where appendNew = (<> matchingUpdates)
        matchingUpdates = concat $ snd <$> filter ((== g) . fst) updates

-- Apply all address space updates to a kernel object. Accepts
-- non-processes and just passes them through.
maybeUpdateProcess :: [(Natural, [AddressSpaceDescElem])] -> KObject -> KObject
maybeUpdateProcess update procObj@KObject{impl=Process{}} = updateProcess update procObj
maybeUpdateProcess _ kobj = kobj

-- Add address space updates to all processes.
updateProcesses :: [(Natural, [AddressSpaceDescElem])] -> [KObject] -> [KObject]
updateProcesses updates kobjs = maybeUpdateProcess updates <$> kobjs

-- Allocate stacks for all threads with initial stack pointer set to
-- Auto.
--
-- We do this by adding an anonymous shared memory region for each
-- thread and pointing the stack pointer to it.
--
-- TODO: Make me generic and use GenericApplicationDescription!
allocateStacks :: ApplicationDescription -> ApplicationDescription
allocateStacks a = ApplicationDescription $ updateProcesses processUpdates allNonThreads <> allUpdatedThreads
  where
    (allThreads, allNonThreads) = partition isThread $ kobjects a
    (allUpdatedThreads, processUpdates) = allocateStacksForThreads allThreads
