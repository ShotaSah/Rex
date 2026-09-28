module Main (
    main
  ) where

import Rex.Data.RangeSet (RangeSet, Range(..))
import qualified Rex.Data.RangeSet as RS

import Data.Set (Set)
import qualified Data.Set as S

import Data.Function (on)
import Test.QuickCheck


newtype Ranges = Ranges { unRanges :: [Range Integer] }

instance Show Ranges where
    show = show . unRanges

instance Arbitrary Ranges where
    arbitrary = do
        n <- choose (0, 10) :: Gen Int
        x <- arbitrary
        let 
            ranges 0 _ = pure []
            ranges n x = do
                y <- arbitrary `suchThat` (> x + 1)
                z <- arbitrary `suchThat` (>= y)
                fmap (Range y z :) (ranges (n - 1) z)
        l <- ranges n x
        pure (Ranges l)

type ZSet = Set Integer
type ZRangeSet = RangeSet Integer

toSet :: Ranges -> ZSet
toSet (Ranges rs) = S.fromList (concatMap (\(Range x y) -> enumFromTo x y) rs)

toRangeSet :: Ranges -> ZRangeSet
toRangeSet (Ranges rs) = RS.fromRanges rs

eqBin :: (ZRangeSet -> ZRangeSet -> ZRangeSet)
      -> (ZSet -> ZSet -> ZSet)
      -> Ranges -> Ranges -> Bool
eqBin f g xs ys = 
    RS.toList ((f `on` toRangeSet) xs ys) == S.toList ((g `on` toSet) xs ys)

eq_union :: Ranges -> Ranges -> Bool
eq_union = eqBin RS.union S.union

eq_difference :: Ranges -> Ranges -> Bool
eq_difference = eqBin RS.difference S.difference

main :: IO ()
main = do
    quickCheck eq_union
    quickCheck eq_difference

