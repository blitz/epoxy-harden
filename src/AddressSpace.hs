module AddressSpace where

import qualified Data.ByteString as B
import           Data.Elf
import           Data.Int        (Int64)
import           Data.Maybe
import           Data.Set        (Set)
import qualified Data.Set        as Set

import           FrameAlloc
import           Interval

data Permission = Read
    | Write
    | Execute
    | User
    deriving (Show, Eq, Ord)

type PermissionSet = Set Permission

noPermissions :: PermissionSet
noPermissions = Set.empty

userPermissions :: PermissionSet
userPermissions = Set.fromList [User]

rwuPermissions :: PermissionSet
rwuPermissions = Set.fromList [Read, Write, User]

elfSegmentFlagToPermission :: ElfSegmentFlag -> Maybe Permission
elfSegmentFlagToPermission sflag =
  case sflag of
    PF_X -> Just Execute
    PF_W -> Just Write
    PF_R -> Just Read
    _    -> Nothing

fromElfSegmentFlags :: [ElfSegmentFlag] -> PermissionSet
fromElfSegmentFlags = Set.fromList . mapMaybe elfSegmentFlagToPermission

data BackingStore = Preloaded Frame B.ByteString
    | Anywhere B.ByteString
    | Fixed Frame Int64
    deriving (Show)

data AddressSpaceChunk = AddressSpaceChunk
    { virtStart    :: Page
    , backingStore :: BackingStore
    , permissions  :: PermissionSet
    }
    deriving (Show)

type AddressSpace = [AddressSpaceChunk]

backingStoreFrame :: BackingStore -> Frame
backingStoreFrame (Preloaded f _) = f
backingStoreFrame (Fixed f _) = f
backingStoreFrame _ = error "Cannot resolve physical address in unallocated space"

backingStoreLength :: BackingStore -> Int64
backingStoreLength (Preloaded _ d) = fromIntegral $ B.length d
backingStoreLength (Anywhere d)    = fromIntegral $ B.length d
backingStoreLength (Fixed _ pages) = pages * pageSize

pageInterval :: AddressSpaceChunk -> PageInterval
pageInterval chunk = fromSize (virtStart chunk) (virtToPageUp
                                                 (backingStoreLength (backingStore chunk)))

zeroes :: Int64 -> B.ByteString
zeroes cnt = B.replicate (fromIntegral cnt) 0

zeroExtend :: B.ByteString -> Int64 -> B.ByteString
zeroExtend bytes len
  | diff >= 0 = bytes <> zeroes diff
  | otherwise = error "zeroExtend asked to shrink vector"
  where diff = len - fromIntegral (B.length bytes)

segmentData :: ElfSegment -> B.ByteString
segmentData seg = zeroExtend (elfSegmentData seg) (fromIntegral (elfSegmentMemSize seg))

-- A usual GCC/newlib toolchain generates segments like these:
--
-- Program Headers:
-- Type           Offset             VirtAddr           PhysAddr
--                 FileSiz            MemSiz              Flags  Align
-- LOAD           0x0000000000000000 0x0000000000010000 0x0000000000010000
--                0x0000000000000530 0x0000000000000530  R E    0x1000
-- LOAD           0x0000000000000530 0x0000000000011530 0x0000000000011530
--                0x0000000000000798 0x0000000000000818  RW     0x1000
--
-- This is apparently done to safe space in the binary. We zero pad
-- the beginning of segments to be page aligned.
zeroPadToPage :: ElfSegment -> ElfSegment
zeroPadToPage (ElfSegment typ flags virt phys align dta msize)
  = ElfSegment typ flags (fromIntegral alignedVirt) (fromIntegral paddedPhys) align paddedData paddedMsize
    where alignedVirt = pageToVirt $ virtToPageDown $ fromIntegral virt
          neededPadding = (fromIntegral virt) - alignedVirt
          paddedPhys = (fromIntegral phys) - neededPadding
          paddedData = zeroes neededPadding <> dta
          paddedMsize = msize + (fromIntegral neededPadding)

elfToAddressSpace :: Elf -> PermissionSet -> AddressSpace
elfToAddressSpace elf defaultPerm =
  [toAddressSpaceChunk $ zeroPadToPage s | s <- elfSegments elf, elfSegmentType s == PT_LOAD && elfSegmentMemSize s > 0]
  where
    toAddressSpaceChunk :: ElfSegment -> AddressSpaceChunk
    toAddressSpaceChunk seg
      | isPageAligned v = AddressSpaceChunk {
          virtStart = virtToPageDown v,
          backingStore = Anywhere (segmentData seg),
          permissions = Set.union defaultPerm (fromElfSegmentFlags (elfSegmentFlags seg))}
      | otherwise = error "Segment is not page aligned"
      where
        v = fromIntegral $ elfSegmentVirtAddr seg

-- Add kernel mappings to a user space address space.
infuseKernel :: AddressSpace -> AddressSpace -> AddressSpace
infuseKernel kernelAs userAs = kernelAs ++ userAs

lookupPhysChunk :: AddressSpaceChunk -> Int64 -> Maybe Int64
lookupPhysChunk chunk virtAddr
  | isInside (pageInterval chunk) page = Just $ frameToPhys chunkFrame + virtAddr - pageToVirt page
  | otherwise = Nothing
  where page = virtToPageDown virtAddr
        chunkFrame = backingStoreFrame $ backingStore chunk

-- Lookup a physical address from a virtual one.
lookupPhys :: AddressSpace -> Int64 -> Maybe Int64
lookupPhys [] virtAddr = Nothing
lookupPhys (head:rest) virtAddr = case lookupPhysChunk head virtAddr of
  r@(Just _) -> r
  Nothing    -> lookupPhys rest virtAddr
