{-# LANGUAGE ExplicitForAll #-}

module Interval where

-- There is an Interval type in Data.IntervalMap, but it is stupid, because it
-- has a unnecessarily complex internal representation.

-- Half-open Interval
data Interval a =  Interval
                   {
                     fromIvl :: a,
                     toIvl   :: a
                   }
  deriving (Eq)

instance Functor Interval where
  fmap f (Interval from to) = Interval (f from) (f to)

instance Show a => Show (Interval a) where
  show (Interval from to) = "[" ++ show from ++ ", " ++ show to ++ ")"

fromSize :: Ord a => Num a => a -> a -> Interval a
fromSize from isize = Interval from (from + isize)

isEmpty :: Ord a => Interval a -> Bool
isEmpty (Interval from to) = to <= from

intersects :: Ord a => Interval a -> Interval a -> Bool
intersects a@(Interval a_from a_to) b@(Interval b_from b_to)
  | isEmpty a || isEmpty b = False
  | otherwise = not (begins_before || starts_after)
  where begins_before = a_to <= b_from
        starts_after = b_to <= a_from

isAdjacent :: Ord a => Interval a -> Interval a -> Bool
isAdjacent a@(Interval a_from a_to) b@(Interval b_from b_to)
  | not (isEmpty a) && not (isEmpty b) = (a_to == b_from) || (b_to == a_from)
  | otherwise = False

joinable :: Ord a => Interval a -> Interval a -> Bool
joinable a b = isAdjacent a b || intersects a b

-- Join adjacent or intersecting intervals
joinAdjacent :: Ord a => Interval a -> Interval a -> Interval a
joinAdjacent a@(Interval a_from a_to) b@(Interval b_from b_to)
  | joinable a b = Interval (min a_from b_from) (max a_to b_to)
  | otherwise = error "Can only join adjacent intervals"

subtract :: Ord a => Interval a -> Interval a -> [Interval a]
subtract a@(Interval a_from a_to) b@(Interval b_from b_to)
  | not (intersects a b) = [a]
  | otherwise = filter (not . isEmpty) [Interval a_from b_from, Interval b_to a_to]

intersection :: Ord a => Num a => Interval a -> Interval a -> Interval a
intersection a@(Interval fromA toA) b@(Interval fromB toB)
  | intersects a b = Interval (max fromA fromB) (min toA toB)
  | otherwise = Interval 0 0

size :: Ord a => Num a => Interval a -> a
size a@(Interval from to)
  | isEmpty a = 0
  | otherwise = to - from

isInside :: (Ord a, Num a) => Interval a -> a -> Bool
isInside ivl v = intersects ivl (fromSize v 1)
