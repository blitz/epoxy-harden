module RiscV where

import qualified AddressSpace as AS
import           Data.Bits
import           Data.Set     (Set)
import qualified Data.Set     as Set
import           Data.Word

import           FrameAlloc   (Frame)

data PteProperty = Valid | Read | Write | Execute | User | Global | Accessed | Dirty
  deriving (Bounded, Enum, Eq, Ord)

-- TODO This could be a bit vector for better performance. Using sets
-- probably makes performance worse.
type PteProperties = Set PteProperty

ptePropToWord :: PteProperty -> Word64
ptePropToWord = shiftL (fromIntegral 1) . fromEnum

ptePropsToWord :: PteProperties -> Word64
ptePropsToWord = foldl (.|.) (fromIntegral 0) . map ptePropToWord . Set.toList

toPteProp :: AS.Permission -> PteProperty
toPteProp AS.Read    = Read
toPteProp AS.Write   = Write
toPteProp AS.Execute = Execute
toPteProp AS.User    = User

toPteProps :: AS.PermissionSet -> PteProperties
toPteProps = Set.map toPteProp

makeLeafPte :: Word64 -> AS.PermissionSet -> Word64
makeLeafPte physAddr perm = (shiftR physAddr 2) .|. (ptePropsToWord props)
  where props = Set.union (toPteProps perm) defaultProps
        defaultProps = Set.fromList [Valid, Accessed, Dirty]

makeNonLeafPte :: Word64 -> Word64
makeNonLeafPte physAddr = (shiftR physAddr 2) .|. (ptePropToWord Valid)

pteFrame :: Word64 -> Frame
pteFrame f = fromIntegral $ shiftR f 10
