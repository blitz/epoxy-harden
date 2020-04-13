{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module FrameAlloc where

import           Data.Bits
import           Data.Int  (Int64)
import           Data.List (find)
import           Interval  as I

type Frame = Int64
type Page = Int64
type ByteInterval = Interval Int64
type FrameInterval = Interval Frame
type PageInterval = Interval Page

type FrameIntervalSet = [FrameInterval]

frameOrder :: Int
frameOrder = 12

pageOrder :: Int
pageOrder = frameOrder

frameSize :: Int64
frameSize = shiftL 1 frameOrder

pageSize :: Int64
pageSize = frameSize

frameToPhys :: Frame -> Int64
frameToPhys f = shiftL f frameOrder

pageToVirt :: Page -> Int64
pageToVirt = frameToPhys

physToFrameDown :: Int64 -> Frame
physToFrameDown b = shiftR b frameOrder

physToFrameUp :: Int64 -> Frame
physToFrameUp b = physToFrameDown (b + frameSize - 1)

virtToPageDown :: Int64 -> Page
virtToPageDown = physToFrameDown

virtToPageUp :: Int64 -> Page
virtToPageUp = physToFrameUp

isPageAligned :: Int64 -> Bool
isPageAligned v = mod v pageSize == 0

findInterval :: Int64 -> FrameIntervalSet -> Maybe FrameInterval
findInterval ivlSize set = toSizedChunk <$> find ((>= ivlSize) . I.size) set
  where toSizedChunk (Interval from _) = I.fromSize from ivlSize

reserveFrameInterval :: FrameInterval -> FrameIntervalSet -> FrameIntervalSet
reserveFrameInterval ivl = concatMap (`I.subtract` ivl)

reserveFrameIntervals :: [FrameInterval] -> FrameIntervalSet -> FrameIntervalSet
reserveFrameIntervals ivls set = foldr reserveFrameInterval set ivls

-- TODO This needs a more complicated version where we allow users to allocate memory in the lower 32-bit.
allocateFrames :: Int64 -> FrameIntervalSet -> (Maybe Frame, FrameIntervalSet)
allocateFrames ivlSize set =  case findInterval ivlSize set of
                                Just ivl -> (Just (fromIvl ivl), reserveFrameInterval ivl set)
                                Nothing  -> (Nothing,  set)
