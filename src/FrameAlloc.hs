{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module FrameAlloc where

import           Data.Bits
import           Data.List (find)
import           Interval  (Interval)
import           Interval  as I

type Frame = Integer
type Page = Integer
type ByteInterval = Interval Integer
type FrameInterval = Interval Frame
type PageInterval = Interval Page

type FrameIntervalSet = [FrameInterval]

frameOrder :: Int
frameOrder = 12

pageOrder :: Int
pageOrder = frameOrder

frameSize :: Integer
frameSize = shiftL 1 frameOrder

pageSize :: Integer
pageSize = frameSize

frameToPhys :: Frame -> Integer
frameToPhys f = shiftL f frameOrder

physToFrameDown :: Integer -> Frame
physToFrameDown b = shiftR b frameOrder

physToFrameUp :: Integer -> Frame
physToFrameUp b = physToFrameDown (b + frameSize - 1)

virtToPageDown :: Integer -> Page
virtToPageDown = physToFrameDown

virtToPageUp :: Integer -> Page
virtToPageUp = physToFrameUp

isPageAligned :: Integer -> Bool
isPageAligned v = mod v pageSize == 0

findInterval :: Integer -> FrameIntervalSet -> Maybe FrameInterval
findInterval ivlSize set = toSizedChunk <$> find ((>= ivlSize) . I.size) set
  where toSizedChunk (Interval from _) = I.fromSize from ivlSize

reserveFrameInterval :: FrameInterval -> FrameIntervalSet -> FrameIntervalSet
reserveFrameInterval ivl = concatMap (`I.subtract` ivl)

reserveFrameIntervals :: [FrameInterval] -> FrameIntervalSet -> FrameIntervalSet
reserveFrameIntervals ivls set = foldr reserveFrameInterval set ivls

-- TODO This needs a more complicated version where we allow users to allocate memory in the lower 32-bit.
allocateFrames :: Integer -> FrameIntervalSet -> (Maybe Frame, FrameIntervalSet)
allocateFrames ivlSize set =  case findInterval ivlSize set of
                                Just ivl -> (Just (fromIvl ivl), reserveFrameInterval ivl set)
                                Nothing  -> (Nothing,  set)
