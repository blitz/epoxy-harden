module BootImage ( generateBootImage ) where

import           Control.Monad
import           Control.Monad.State.Lazy
import           Data.Binary.Put
import qualified Data.ByteString            as B
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as B8
import           Data.Elf
import           Data.Int                   (Int64)
import           Data.List
import           Data.Maybe
import           Dhall                      (Natural)

import           AddressSpace
import qualified ApplicationDescription     as AD
import           EpoxyState
import           FrameAlloc
import           Interval
import           MachineDescription
import           PageTable
import           Writer

byteToFrameInterval :: ByteInterval -> FrameInterval
byteToFrameInterval (Interval fromB toB) = Interval (physToFrameDown fromB) (physToFrameUp toB)

toFrameInterval :: MemoryMapEntry -> FrameInterval
toFrameInterval e =
  byteToFrameInterval (fromSize (fromIntegral (baseAddress e)) (fromIntegral (memoryLength e)))

toFreeFrames :: MachineDescription -> FrameIntervalSet
toFreeFrames md =
  [toFrameInterval e | e <- memoryMap md, memoryType e == Available]

-- Writes each frame with backing store into memory.
writeAddressSpace :: AddressSpace -> State Epoxy ()
writeAddressSpace = mapM_ writeChunk
  where writeChunk (AddressSpaceChunk _ (Preloaded f s) _) = writeMemoryM (frameToPhys f) (BL.fromStrict s)
        writeChunk (AddressSpaceChunk _ (Fixed _ _) _) = return ()
        writeChunk _ = error "Can only write allocated ELFs"

-- Allocate frames for an ELF binary. This will find a place in memory
-- for Anywhere frames. This function is idempotent.
allocateAddressSpace :: AddressSpace -> State Epoxy AddressSpace
allocateAddressSpace = mapM allocateChunk
  where allocateChunk (AddressSpaceChunk v (Anywhere s) p) = do
          let pages = virtToPageUp (fromIntegral (B.length s))
          frame <- allocateFramesM pages
          return (AddressSpaceChunk v (Preloaded frame s) p)
        allocateChunk c = return c

-- Allocates and writes back an address space.
realizeAddressSpace :: AddressSpace -> State Epoxy AddressSpace
realizeAddressSpace as = do
  allocatedAs <- allocateAddressSpace as
  writeAddressSpace allocatedAs
  return allocatedAs

-- Loads and allocates the address space for the kernel.
loadKernelElf :: Elf -> State Epoxy AddressSpace
loadKernelElf kernelElf = allocateAddressSpace $ elfToAddressSpace kernelElf noPermissions

-- Return a simplified list of symbols. Only includes with names.
getSymbolList :: Elf -> [(String, Int64)]
getSymbolList elf = map simplify symbolsWithName
  where simplify symbol = (B8.unpack (BL.fromStrict (fromJust (snd (steName symbol)))), fromIntegral $ steValue symbol)
        symbolsWithName = filter hasName (concat (parseSymbolTables elf))
        hasName symbol = isJust (snd (steName symbol))

symbolToVirt :: String -> Elf -> Int64
symbolToVirt name elf = case find hasName (getSymbolList elf) of
  Just (_, vAddr) -> vAddr
  _               -> error ("No symbol: " ++ show name)
  where hasName (n, _) = n == name

patchInt64 :: Int64 -> Int64 -> State Epoxy ()
patchInt64 phys val =
  writeMemoryM phys (runPut (putInt64le val))

-- |Patch root patch table pointers at the given symbol.
--
-- Page table pointers are in an architecture-dependent format
-- (i.e. valid SATP values on RISC-V), but are always stored as 64-bit
-- integers.
patchPt :: Elf -> AddressSpace -> String -> Int64 -> Int -> State Epoxy ()
patchPt elf as sym pageTablePtr idx =
  patchInt64 (fromIntegral (fromIntegral idx * 8 + symPhys)) pageTablePtr
  where symPhys = fromJust $ lookupPhys as $ fromIntegral $ symbolToVirt sym elf

mmapToAsChunk :: Int64 -> MemoryMapEntry -> PermissionSet -> AddressSpaceChunk
mmapToAsChunk v (MemoryMapEntry base len _) = AddressSpaceChunk
  (virtToPageDown $ fromIntegral v)
  (Fixed
    (physToFrameDown $ fromIntegral base)
    (fromIntegral $ physToFrameUp $ fromIntegral len))

toPermSet :: AD.SharedMemoryPermissions -> PermissionSet
toPermSet AD.R  = permSetFromList [ AddressSpace.User, AddressSpace.Read ]
toPermSet AD.RW = permSetFromList [ AddressSpace.User, AddressSpace.Read, AddressSpace.Write ]
toPermSet AD.RX = permSetFromList [ AddressSpace.User, AddressSpace.Read, AddressSpace.Execute ]

sharedMemToAs :: MachineDescription -> Natural -> PermissionSet -> AD.SharedMemorySource -> AddressSpaceChunk
sharedMemToAs mDesc virt perm (AD.AnonymousMemory size)
  = AddressSpaceChunk (virtToPageDown $ fromIntegral virt) (Anywhere (B.replicate (fromIntegral size) 0)) perm
sharedMemToAs mDesc virt perm (AD.NamedSharedMemory key)
  = case maybeMem of
      Just m -> mmapToAsChunk (fromIntegral virt) m perm
      _      -> error "Failed to find shared memory region"
  where maybeMem = findMemoryWithKey mDesc key

-- Creates complete address spaces from a address space description.
descToAddressSpace :: MachineDescription -> AddressSpace -> AD.AddressSpaceDesc -> AddressSpace
descToAddressSpace mDesc kernelAs asDesc = infuseKernel kernelAs $ concatMap createAs asDesc
  where
    createAs :: AD.AddressSpaceDescElem -> AddressSpace
    createAs AD.ELF{AD.binary=b} = elfToAddressSpace b userPermissions
    createAs AD.SharedMemory{AD.source=s, AD.vaDestination=v, AD.permissions=perm}
      | isPageAligned (fromIntegral v) = [sharedMemToAs mDesc v (toPermSet perm) s]
      | otherwise = error "Shared memory region is not page aligned"

generateBootImage :: MachineDescription -> Elf -> AD.ApplicationDescription -> String -> B.ByteString
generateBootImage mDesc kernelElf appDesc outputFormat = evalFromInitial $ do
  kernelAs <- loadKernelElf kernelElf
  userAss <- mapM (loadUserAs kernelAs) $ AD.processes appDesc
  pts <- realizePageTables (map constructPageTable (kernelAs:userAss))
  -- Patch boot page table pointer
  patchPt kernelElf kernelAs "BOOT_SATP" (head pts) 0
  zipWithM_ (patchPt kernelElf kernelAs "USER_SATPS") (tail pts) [0..]
  -- Wrap our memory state into the boot image as ELF segments
  let maybePhysEntry = lookupPhys kernelAs $ fromIntegral $ elfEntry kernelElf
  maybe (error "Invalid entry point into kernel")
    (\x -> writeOutput (fromIntegral x) <$> gets _memory) maybePhysEntry
  where
    loadUserAs kernelAs = realizeAddressSpace . descToAddressSpace mDesc kernelAs . AD.processAddressSpace
    evalFromInitial m = evalState m $ initialEpoxy initialFreeMemory
    initialFreeMemory = toFreeFrames mDesc
    writeOutput = resolveWriterFunction outputFormat
