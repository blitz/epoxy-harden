module RiscV where

import qualified AddressSpace as AS
import           Data.Bits
import           Data.Int     (Int64)
import           Data.Set     (Set)
import qualified Data.Set     as Set

import           FrameAlloc   (Frame)

data PteProperty = Valid
    | Read
    | Write
    | Execute
    | User
    | Global
    | Accessed
    | Dirty
    deriving (Bounded, Enum, Eq, Ord)

-- TODO This could be a bit vector for better performance. Using sets
-- probably makes performance worse.
type PteProperties = Set PteProperty

ptePropToWord :: PteProperty -> Int64
ptePropToWord = shiftL 1 . fromEnum

ptePropsToWord :: PteProperties -> Int64
ptePropsToWord = foldl (.|.) 0 . map ptePropToWord . Set.toList

toPteProp :: AS.Permission -> PteProperty
toPteProp AS.Read    = Read
toPteProp AS.Write   = Write
toPteProp AS.Execute = Execute
toPteProp AS.User    = User

toPteProps :: AS.PermissionSet -> PteProperties
toPteProps = Set.map toPteProp

makeLeafPte :: Int64 -> AS.PermissionSet -> Int64
makeLeafPte physAddr perm = shiftR physAddr 2 .|. ptePropsToWord props
  where props = Set.union (toPteProps perm) defaultProps
        defaultProps = Set.fromList [Valid, Accessed, Dirty]

makeNonLeafPte :: Int64 -> Int64
makeNonLeafPte physAddr = shiftR physAddr 2 .|. ptePropToWord Valid

pteFrame :: Int64 -> Frame
pteFrame f = fromIntegral $ shiftR f 10
