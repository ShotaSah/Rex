module Rex.Data.RangeSet (
  -- * Types
    Range(..)
  , RangeSet

  , point
  , mapR

  -- * Introducing
  , empty
  , singleton
  , range

  -- * Query
  , null
  , member

  -- * Joins
  , union
  , unions

  -- * Differences
  , difference
  , (\\)

  -- * Spliting
  , splitLE

  -- * Conversion
  , fromList
  , toList
  , fromRanges
  , toRanges
  ) where

import Data.Foldable (foldl')
import Prelude hiding (null)


data Range a = Range !a !a
    deriving (Eq, Ord)

instance (Eq a, Show a) => Show (Range a) where
    show x = "[" ++ showRange x ++ "]"

point :: a -> Range a
point x = Range x x

mapR :: (a -> b) -> Range a -> Range b
mapR f (Range x0 x1) = Range (f x0) (f x1)

newtype RangeSet a = RS { unRS :: [Range a] }
    deriving Eq

instance (Eq a, Show a) => Show (RangeSet a) where
    show x = "[" ++ showRangeSet x ++ "]"

empty :: RangeSet a
empty = RS []

singleton :: a -> RangeSet a
singleton x = RS [point x]

range :: Ord a => a -> a -> RangeSet a
range x0 x1 
  | x0 > x1   = empty
  | otherwise = RS [Range x0 x1]

union :: (Enum a, Ord a) => RangeSet a -> RangeSet a -> RangeSet a
union (RS qs) (RS rs) = 
    RS (maybe rs id (insert qs rs))
  where
    insert (q@(Range x0 x1):qs) (r@(Range y0 y1):rs)
      | y0 <= x0 && x1 <= y1 = insert qs (r:rs)
      | succ y1 < x0 = fmap (r :) (insert (q:qs) rs)
      | succ x1 < y0 = Just (q : maybe (r:rs) id (insert qs (r:rs)))
      | otherwise = 
        let go p0 qs rs
              | p0 == p1 && p0 == p2 = (p0, qs', rs')
              | otherwise = go p2 qs' rs'
              where
                (p1, qs') = merge p0 qs
                (p2, rs') = merge p1 rs
            (p, qs', rs') = go (Range (x0 `min` y0) (x1 `max` y1)) qs rs
         in Just (p : maybe rs' id (insert qs' rs'))
    insert [] _  = Nothing
    insert qs [] = Just qs

    merge q@(Range x0 x1) (r@(Range y0 y1) : rs)
      | succ y1 < x1 = merge q rs
      | succ x1 < y0 = (q, r:rs)
      | otherwise = (Range x0 (x1 `max` y1), rs)
    merge q [] = (q, [])

unions :: (Enum a, Ord a) => [RangeSet a] -> RangeSet a
unions = foldl' union empty

-- Postulation: the range is never zero range
deleteRange :: (Enum a, Ord a) => Range a -> RangeSet a -> RangeSet a
deleteRange q (RS rs) = 
    RS (maybe rs id (delete q rs))
  where
    delete q@(Range x0 x1) (r@(Range y0 y1) : rs)
      | y1 < x0   = fmap (r :) (delete q rs)
      | x1 < y0   = Nothing
      | otherwise = Just $ (if y0 < x0 then (Range y0 (pred x0) :) else id)
                         $ eraseRight x1 (r : rs)
    delete q []   = Nothing

    eraseRight x1 (r@(Range y0 y1) : rs) 
      | x1 < y0   = r : rs
      | x1 < y1   = Range (succ x1) y1 : rs
      | otherwise = eraseRight x1 rs
    eraseRight _ [] = []

difference, (\\) :: (Enum a, Ord a) => RangeSet a -> RangeSet a -> RangeSet a
difference x = foldl' (flip deleteRange) x . unRS

(\\) = difference

splitLE :: (Enum a, Ord a) => a -> RangeSet a -> (RangeSet a, RangeSet a)
splitLE x (RS rs) = 
    case split rs of
        Nothing       -> (RS rs, empty)
        Just ([], _)  -> (empty, RS rs)
        Just (as, bs) -> (RS as, RS bs)
  where
    split (r@(Range y0 y1) : rs)
      | x < y0 = Just ([], r : rs)
      | x > y1 = fmap (\(as,bs) -> (r:as, bs)) (split rs)
      | otherwise = Just (as, bs)
      where x' = succ x
            as = [Range y0 x]
            bs = if x' > y1 then rs else Range x' y1 : rs
    split [] = Nothing

null :: RangeSet a -> Bool
null (RS []) = True
null (RS _)  = False

member :: Ord a => a -> RangeSet a -> Bool
member x = 
    go x . unRS
  where
    go x (Range y z : rs)
      | x < y     = False
      | x > z     = go x rs
      | otherwise = True
    go _ [] = False

fromList :: (Enum a, Ord a) => [a] -> RangeSet a
fromList = foldl' (\xs x -> xs `union` singleton x) empty

toList :: Enum a => RangeSet a -> [a]
toList = concatMap (\(Range x y) -> enumFromTo x y) . unRS

fromRanges :: [Range a] -> RangeSet a
fromRanges = RS

toRanges :: RangeSet a -> [Range a]
toRanges = unRS

showRange :: (Eq a, Show a) => Range a -> String
showRange (Range x y)
  | x == y    = show x
  | otherwise = show x ++ ".." ++ show y

showRangeSet :: (Eq a, Show a) => RangeSet a -> String
showRangeSet (RS rs) =
    go rs
  where
    go [] = ""
    go (r:[]) = showRange r
    go (q:r:rs) = showRange q ++ ',' : go (r:rs)

{-
mapAdd :: Num a => a -> RangeSet a -> RangeSet a
mapAdd t = 
    RS . map (mapR (+ t)) . unRS

mapDiv :: Integral a => a -> RangeSet a -> RangeSet a
mapDiv m = 
    RS . go . map (mapR (`div` m)) . unRS
  where
    go (q@(Range x0 x1) : r@(Range y0 y1) : rs)
      | succ x1 < y0 = q : go (r : rs)
      | x1 >= y1     = go (q : rs)
      | otherwise    = go (Range x0 y1 : rs)
    go rs = rs

mapMod :: Integral a => a -> RangeSet a -> RangeSet a
mapMod m = 
    go empty . map (mapR (`divMod` m)) . unRS
  where
    go acc _ | acc == cover = acc
    go acc rs = case rs of
      Range (q0,r0) (q1,r1) : rs -> 
        if q0 < q1
        then if r0 <= r1
             then cover
             else go (range r0 (m - 1) `union` range 0 r1 `union` acc) rs
        else go (range r0 r1 `union` acc) rs
      [] -> acc

    cover = range 0 (m - 1)

{-# SPECIALIZE mapAdd :: Int -> RangeSet Int -> RangeSet Int #-}
{-# SPECIALIZE mapDiv :: Int -> RangeSet Int -> RangeSet Int #-}
{-# SPECIALIZE mapMod :: Int -> RangeSet Int -> RangeSet Int #-}
-}
