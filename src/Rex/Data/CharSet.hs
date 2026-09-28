module Rex.Data.CharSet (
  -- * Types
    CharSet

  -- * Wrapped functions
  , empty
  , every
  , singleton
  , range
  , unions
  , difference
  , (\\)
  , fromList
  , toList

  -- * Backdoors
  , wrap
  , unwrap
  ) where

import Rex.Data.RangeSet (RangeSet, Range)
import qualified Rex.Data.RangeSet as RS
import Data.Char (chr, ord)
import Data.Foldable (foldl')


newtype CharSet = CS { unCS :: RangeSet Int }

empty :: CharSet
empty = CS RS.empty

every :: CharSet
every = range minBound maxBound

singleton :: Char -> CharSet
singleton = CS . RS.singleton . ord

range :: Char -> Char -> CharSet
range x y = CS (RS.range (ord x) (ord y))

unions :: [CharSet] -> CharSet
unions = CS . foldl' (\x (CS y) -> x `RS.union` y) RS.empty

difference, (\\) :: CharSet -> CharSet -> CharSet
difference (CS x) (CS y) = CS (RS.difference x y)

(\\) = difference

splitLE :: Char -> CharSet -> (CharSet, CharSet)
splitLE c (CS x) = case RS.splitLE (ord c) x of (l,r) -> (CS l, CS r)

fromList :: [Char] -> CharSet
fromList = CS . RS.fromList . map ord

toList :: CharSet -> [Char]
toList = map chr . RS.toList . unCS

wrap :: RangeSet Int -> CharSet
wrap = CS

unwrap :: CharSet -> RangeSet Int
unwrap = unCS

instance Show CharSet where
    show = show . unCS
