module EpoxyMain where

import           Control.Lens               as LS
import           Control.Monad
import           Control.Monad.State.Lazy
import           Data.Binary.Put
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

-- Remove the physical memory regions that a loaded ELF images occupies from an allocator.
reserveLoadedElf :: Elf -> FrameIntervalSet -> FrameIntervalSet
reserveLoadedElf elf = reserveFrameIntervals (map toPhysFrameInterval (elfLoadSegments elf))
  where
    toPhysFrameInterval :: ElfSegment -> FrameInterval
    toPhysFrameInterval seg = byteToFrameInterval (fromSize
                                                    (toInteger (elfSegmentPhysAddr seg))
                                                    (toInteger (elfSegmentMemSize seg)))

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
  return $ writeAddressSpace as
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

elfVirtToPhys :: Word64 -> Elf -> Word64
elfVirtToPhys v elf
  | length matchingSegments /= 1 = error "Cannot find physical address"
  | otherwise = elfSegmentPhysAddr match + v - elfSegmentVirtAddr match
  where match = head matchingSegments
        matchingSegments = filter segmentMatches (elfLoadSegments elf)
        segmentMatches s = isInside (fromSize (elfSegmentVirtAddr s) (elfSegmentMemSize s)) v

symbolToPhys :: String -> Elf -> Word64
symbolToPhys name elf = case find hasName (getSymbolList elf) of
  Just (_, vAddr) -> elfVirtToPhys vAddr elf
  _               -> error ("No symbol: " ++ show name)
  where hasName (n, _) = n == name

patchWord32 :: Integer -> Word32 -> State Epoxy ()
patchWord32 phys val =
  writeMemoryM phys (runPut (putWord32le val))

patchPt :: Elf -> String -> Word64 -> Int -> State Epoxy ()
patchPt kernelElf sym ptr idx
  | symPhys <= fromIntegral (maxBound :: Word32) = patchWord32 (fromIntegral (fromIntegral idx * 4 + symPhys))
                                                   (fromIntegral ptr)
  | otherwise = error "Virtual address out of bounds"
  where symPhys = symbolToPhys sym kernelElf

generateBootImage :: MachineDescription -> Elf -> [Elf] -> B.ByteString
generateBootImage mDesc kernelElf processElfs =
  evalState (do
                kernelAs <- loadKernelElf kernelElf
                userAss <- mapM (loadUserElf kernelAs) processElfs
                pts <- realizePageTables (map constructPageTable (kernelAs:userAss))
                -- Patch boot page table pointer
                patchPt kernelElf "BOOT_PAGE_TABLE_PTR" (head pts) 0
                zipWithM_ (patchPt kernelElf "USER_PAGE_TABLE_PTRS") (tail pts) [0..]
                -- Wrap our memory state into the boot image as ELF segments
                bootElfFromMemory (fromIntegral (elfEntry kernelElf)) <$> gets _memory) initialEpoxy
  where initialEpoxy = Epoxy { _allocator = initialFreeMemory,
                               _memory = [] }
        -- Reserve the lower MB to be friendly to boot loaders.
        initialFreeMemory = reserveFrameInterval (Interval 0 0x100) (toFreeFrames mDesc)
