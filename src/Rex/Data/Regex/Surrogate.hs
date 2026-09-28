module Rex.Data.Regex.Surrogate (
    surrogates
  ) where

import Rex.Data.RangeSet (RangeSet, Range(..))
import qualified Rex.Data.RangeSet as RS
import qualified Rex.Data.AlphaSet as AS
import Rex.Data.Regex (Regex(..))
import qualified Rex.Data.Regex as RE

import Data.Set (Set)
import qualified Data.Set as S
import Data.Foldable (foldl')


type Digit = Int
type Digits = [Digit]

digitize :: Int -> Int -> Int -> Digits
digitize n radix = 
    go n
  where
    go 0 _ = []
    go n x = case x `quotRem` radix of
        (q,r) -> r : go (n - 1) q

add :: Int -> Digits -> Digit -> Digits
add radix = 
    go
  where
    go (x:xs) carry = 
        r : case carry' of 0 -> xs
                           _ -> go xs carry'
      where (carry',r) = (x + carry) `divMod` radix

suffxLen :: Digits -> Digits -> Int
suffxLen xs ys = 
    snd (go 0 xs ys)
  where
    go l [] [] = (True, l)
    go l (x:xs) (y:ys)
        | p && x == y = (True , n - 1)
        | otherwise   = (False, n)
      where (p, n) = go (l + 1) xs ys

compress :: Int -> Digits -> Digits -> [[Range Digit]]
compress radix xs ys = 
    -- map reverse $
    case go (suffxLen xs ys) xs ys of
        (ls, rs) -> S.toList (S.fromList ls `S.union` S.fromList rs)
  where
    go n (x:xs) (y:ys) | n <= 1 = let
        zs | x > y      = []
           | otherwise  = [Range x y : map RS.point xs]
     in (zs, zs)

    go n (x:xs) (y:ys) = let
        xs1 | x == mind = xs
            | otherwise = add radix xs 1
        ys1 | y == maxd = ys
            | otherwise = add radix ys (-1)

        (ls, rs) = go (n - 1) xs1 ys1
        ls1 = map (alld :) ls
        rs1 = map (alld :) rs

        ls2 | x == mind = ls1
            | otherwise = (Range x maxd : map RS.point xs) : ls1
        rs2 | y == maxd = rs1
            | otherwise = (Range mind y : map RS.point ys) : rs1
     in (ls2, rs2)

    mind = 0
    maxd = radix - 1
    alld = Range mind maxd

surrogates :: Int -> Int -> [Int] -> RangeSet Int -> Regex enc
surrogates radix off mods alps = 
    foldl' RE.union RE.empty
   [foldl' (flip Cat) Eps
      [Ch (AS.wrap (RS.fromRanges [RS.mapR (+ mod) r]))
        | (mod, r) <- mods `zip` rs]
     | Range x y  <- RS.toRanges alps
     , let xs = digits (x - off)
           ys = digits (y - off)
     , rs <- compress radix xs ys]
  where
    digits = digitize (length mods) radix

