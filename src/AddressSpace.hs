module AddressSpace where

import           Data.Bits
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

asPreloaded :: Integer -> B.ByteString -> BackingStore
asPreloaded = Preloaded

asAnywhere  :: Integer -> B.ByteString -> BackingStore
asAnywhere _ = Anywhere

toAddressSpace :: Elf -> PermissionSet -> (Integer -> B.ByteString -> BackingStore) -> AddressSpace
toAddressSpace elf defaultPerm bstoreConstructor =
  [toAddressSpaceChunk s | s <- elfSegments elf, elfSegmentType s == PT_LOAD]
  where
    toAddressSpaceChunk :: ElfSegment -> AddressSpaceChunk
    toAddressSpaceChunk seg
      | isPageAligned v && isPageAligned p = AddressSpaceChunk {
          virtStart = virtToPageDown v,
          backingStore = bstoreConstructor (physToFrameDown p) (segmentData seg),
          permissions = Set.union defaultPerm (fromElfSegmentFlags (elfSegmentFlags seg))}
      | otherwise = error "Segment is not page aligned"
      where
        v = toInteger (elfSegmentVirtAddr seg)
        p = toInteger (elfSegmentPhysAddr seg)

-- Remove all mappings that are in the user part of the address space.
removeLowerMappings :: AddressSpace -> AddressSpace
removeLowerMappings = filter isNotLower
  where isNotLower c = not (intersects (pageInterval c) (fromSize 0 (shiftL 1 47)))

-- Add kernel mappings to a user space address space.
infuseKernel :: AddressSpace -> AddressSpace -> AddressSpace
infuseKernel kernelAs userAs = removeLowerMappings kernelAs ++ userAs
