{-# LANGUAGE TemplateHaskell #-}
module EpoxyState where

import           Control.Lens             as LS
import           Control.Monad.State.Lazy
import qualified Data.ByteString.Lazy     as BL
import           Data.Int                 (Int64)

import           FrameAlloc
import           PhysMem

data Epoxy = Epoxy
    { _allocator :: FrameIntervalSet
    , _memory    :: Memory
    }
    deriving (Show)
makeLenses ''Epoxy

type EpoxyState = State Epoxy

initialEpoxy :: FrameIntervalSet -> Epoxy
initialEpoxy freeFrames = Epoxy freeFrames emptyMemory

allocateFramesM :: Int64 -> State Epoxy Frame
allocateFramesM ivlsize =
  zoom allocator (state (allocateFrames ivlsize) >>= maybe (error "Failed to allocate frames") return)

writeMemoryM :: Int64 -> BL.ByteString -> State Epoxy ()
writeMemoryM addr = zoom memory . modify . writeMemory addr
