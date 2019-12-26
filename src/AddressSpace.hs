module AddressSpace where

import qualified Data.ByteString as B
import           Data.Elf
import           Data.Maybe
import           Data.Semigroup  ((<>))
import           Data.Set        (Set)
import qualified Data.Set        as Set

import           FrameAlloc
import           Interval

data Permission = Read | Write | Execute | User
  deriving (Show, Eq, Ord)

type PermissionSet = Set Permission

noPermissions :: PermissionSet
noPermissions = Set.empty

userPermissions :: PermissionSet
userPermissions = Set.fromList [User]

elfSegmentFlagToPermission :: ElfSegmentFlag -> Maybe Permission
elfSegmentFlagToPermission sflag =
  case sflag of
    PF_X -> Just Execute
    PF_W -> Just Write
    PF_R -> Just Read
    _    -> Nothing

fromElfSegmentFlags :: [ElfSegmentFlag] -> PermissionSet
fromElfSegmentFlags = Set.fromList . mapMaybe elfSegmentFlagToPermission

data BackingStore = Preloaded Frame B.ByteString | Anywhere B.ByteString | Fixed Frame Int
  deriving (Show)

data AddressSpaceChunk = AddressSpaceChunk
  { virtStart    :: Page,
    backingStore :: BackingStore,
    permissions  :: PermissionSet }
  deriving (Show)

type AddressSpace = [AddressSpaceChunk]

backingStoreFrame :: BackingStore -> Frame
backingStoreFrame (Preloaded f _) = f
backingStoreFrame (Fixed f _) = f
backingStoreFrame _ = error "Cannot resolve physical address in unallocated space"

backingStoreLength :: BackingStore -> Int
backingStoreLength (Preloaded _ d) = B.length d
backingStoreLength (Anywhere d)    = B.length d
backingStoreLength (Fixed _ pages) = pages * fromInteger pageSize

pageInterval :: AddressSpaceChunk -> PageInterval
pageInterval chunk = fromSize (virtStart chunk) (virtToPageUp
                                                 (toInteger (backingStoreLength (backingStore chunk))))

zeroExtend :: B.ByteString -> Integer -> B.ByteString
zeroExtend bytes len
  | diff >= 0 = bytes <> B.replicate (fromInteger diff) 0
  | otherwise = error "zeroExtend asked to shrink vector"
  where diff = len - fromIntegral (B.length bytes)

segmentData :: ElfSegment -> B.ByteString
segmentData seg = zeroExtend (elfSegmentData seg) (toInteger (elfSegmentMemSize seg))

toAddressSpace :: Elf -> PermissionSet -> AddressSpace
toAddressSpace elf defaultPerm =
  [toAddressSpaceChunk s | s <- elfSegments elf, elfSegmentType s == PT_LOAD]
  where
    toAddressSpaceChunk :: ElfSegment -> AddressSpaceChunk
    toAddressSpaceChunk seg
      | isPageAligned v && isPageAligned p = AddressSpaceChunk {
          virtStart = virtToPageDown v,
          backingStore = Anywhere (segmentData seg),
          permissions = Set.union defaultPerm (fromElfSegmentFlags (elfSegmentFlags seg))}
      | otherwise = error "Segment is not page aligned"
      where
        v = toInteger (elfSegmentVirtAddr seg)
        p = toInteger (elfSegmentPhysAddr seg)

-- Add kernel mappings to a user space address space.
infuseKernel :: AddressSpace -> AddressSpace -> AddressSpace
infuseKernel kernelAs userAs = kernelAs ++ userAs

lookupPhysChunk :: AddressSpaceChunk -> Integer -> Maybe Integer
lookupPhysChunk chunk virtAddr
  | isInside (pageInterval chunk) page = Just $ (frameToPhys chunkFrame) + virtAddr - (pageToVirt page)
  | otherwise = Nothing
  where page = virtToPageDown virtAddr
        chunkFrame = backingStoreFrame $ backingStore chunk

-- Lookup a physical address from a virtual one.
lookupPhys :: AddressSpace -> Integer -> Maybe Integer
lookupPhys [] virtAddr = Nothing
lookupPhys (head:rest) virtAddr = case lookupPhysChunk head virtAddr of
  r@(Just _) -> r
  Nothing    -> lookupPhys rest virtAddr
