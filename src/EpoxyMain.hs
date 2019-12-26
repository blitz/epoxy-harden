module EpoxyMain where

import           Control.Lens               as LS
import           Control.Monad
import           Control.Monad.State.Lazy
import           Data.Binary.Put
import           Data.Bits
import qualified Data.ByteString            as B
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as B8
import           Data.Elf
import           Data.List
import           Data.Maybe
import           Data.Word

import           AddressSpace
import           ElfWriter
import           EpoxyState
import           FrameAlloc
import           Interval
import           MachineDescription
import           PageTable

parseElfFile :: FilePath -> IO Elf
parseElfFile path = B.readFile path <&> parseElf

byteToFrameInterval :: ByteInterval -> FrameInterval
byteToFrameInterval (Interval fromB toB) = Interval (physToFrameDown fromB) (physToFrameUp toB)

toFrameInterval :: MemoryMapEntry -> FrameInterval
toFrameInterval e =
  byteToFrameInterval (fromSize (baseAddr e) (memoryLength e))

toFreeFrames :: MachineDescription -> FrameIntervalSet
toFreeFrames md =
  [toFrameInterval e | e <- memoryMap md, memoryType e == Available]

elfLoadSegments :: Elf -> [ElfSegment]
elfLoadSegments elf =  [seg | seg <- elfSegments elf, elfSegmentType seg == PT_LOAD]

writeAddressSpace :: AddressSpace -> State Epoxy ()
writeAddressSpace = mapM_ writeChunk
  where writeChunk (AddressSpaceChunk _ (Preloaded f s) _) = writeMemoryM (frameToPhys f) (BL.fromStrict s)
        writeChunk (AddressSpaceChunk _ (Fixed _ _) _) = return ()
        writeChunk _ = error "Can only write preloaded ELFs"

-- Allocate frames for an ELF binary.
allocateAddressSpace :: AddressSpace -> State Epoxy AddressSpace
allocateAddressSpace = mapM allocateChunk
  where allocateChunk (AddressSpaceChunk v (Anywhere s) p) = do
          let pages = virtToPageUp (toInteger (B.length s))
          frame <- allocateFramesM pages
          return (AddressSpaceChunk v (Preloaded frame s) p)
        allocateChunk c = return c

loadKernelElf :: Elf -> State Epoxy AddressSpace
loadKernelElf kernelElf = do
  as <- allocateAddressSpace $ toAddressSpace kernelElf noPermissions
  writeAddressSpace as
  return as

loadUserElf :: AddressSpace -> Elf -> State Epoxy AddressSpace
loadUserElf kernelAs userElf = do
  as <- allocateAddressSpace $ toAddressSpace userElf userPermissions
  writeAddressSpace as
  return $ infuseKernel kernelAs as

-- Return a simplified list of symbols. Only includes with names.
getSymbolList :: Elf -> [(String, Word64)]
getSymbolList elf = map simplify symbolsWithName
  where simplify symbol = (B8.unpack (BL.fromStrict (fromJust (snd (steName symbol)))), steValue symbol)
        symbolsWithName = filter hasName (concat (parseSymbolTables elf))
        hasName symbol = isJust (snd (steName symbol))

symbolToVirt :: String -> Elf -> Word64
symbolToVirt name elf = case find hasName (getSymbolList elf) of
  Just (_, vAddr) -> vAddr
  _               -> error ("No symbol: " ++ show name)
  where hasName (n, _) = n == name

patchWord64 :: Integer -> Word64 -> State Epoxy ()
patchWord64 phys val =
  writeMemoryM phys (runPut (putWord64le val))

-- TODO Hardcoded RISC-V Sv39 and no ASIDs
ptFrameToSATP :: Frame -> Word64
ptFrameToSATP ptRoot = fromInteger ptRoot .|. shiftL 8 60

patchPt :: Elf -> AddressSpace -> String -> Frame -> Int -> State Epoxy ()
patchPt elf as sym ptFrame idx =
  patchWord64 (fromIntegral (fromIntegral idx * 8 + symPhys)) (ptFrameToSATP ptFrame)
  where symPhys = fromJust $ lookupPhys as $ fromIntegral $ symbolToVirt sym elf

generateBootImage :: MachineDescription -> Elf -> [Elf] -> B.ByteString
generateBootImage mDesc kernelElf processElfs = evalFromInitial $ do
  kernelAs <- loadKernelElf kernelElf
  userAss <- mapM (loadUserElf kernelAs) processElfs
  pts <- realizePageTables (map constructPageTable (kernelAs:userAss))
  -- Patch boot page table pointer
  patchPt kernelElf kernelAs "BOOT_SATP" (head pts) 0
  zipWithM_ (patchPt kernelElf kernelAs "USER_SATPS") (tail pts) [0..]
  -- Wrap our memory state into the boot image as ELF segments
  let maybePhysEntry = lookupPhys kernelAs $ fromIntegral $ elfEntry kernelElf
  maybe (error "Invalid entry point into kernel")
    (\x -> bootElfFromMemory (fromIntegral x) <$> gets _memory) maybePhysEntry
  where
    evalFromInitial m = evalState m initialEpoxy
    initialEpoxy = Epoxy { _allocator = initialFreeMemory,
                           _memory = [] }
    initialFreeMemory = toFreeFrames mDesc
