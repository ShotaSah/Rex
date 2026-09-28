{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Rex.Data.AlphaSet (
  -- * Types
    AlphaSet

  -- * Wrapped functions
  , empty
  , singleton
  , range
  , union
  , unions
  , difference
  , (\\)
  , member
  , fromList
  , toList

  -- * Backdoors
  , wrap
  , unwrap
  ) where

import Rex.Codec.Types (Encoding)
import Rex.Data.Alpha (Alpha)
import qualified Rex.Data.Alpha as A

import Rex.Data.RangeSet (RangeSet, Range)
import qualified Rex.Data.RangeSet as RS

import Data.Foldable (foldl')


newtype AlphaSet (enc :: Encoding) = AS { unAS :: RangeSet Int }

empty :: AlphaSet enc
empty = AS RS.empty

singleton :: A.Impl enc => Alpha enc -> AlphaSet enc
singleton = AS . RS.singleton . fromIntegral
{-# INLINE singleton #-}

range :: A.Impl enc => Alpha enc -> Alpha enc -> AlphaSet enc
range x y = AS (RS.fromList [fromIntegral x .. fromIntegral y])
{-# INLINE range #-}

union :: AlphaSet enc -> AlphaSet enc -> AlphaSet enc
union (AS x) (AS y) = AS (x `RS.union` y)

unions :: [AlphaSet enc] -> AlphaSet enc
unions = foldl' union empty

difference :: AlphaSet enc -> AlphaSet enc -> AlphaSet enc
difference (AS x) (AS y) = AS (x RS.\\ y)

(\\) :: AlphaSet enc -> AlphaSet enc -> AlphaSet enc
(\\) = difference

member :: A.Impl enc => Alpha enc -> AlphaSet enc -> Bool
member x (AS s) = fromIntegral x `RS.member` s
{-# INLINE member #-}

fromList :: A.Impl enc => [Alpha enc] -> AlphaSet enc
fromList = AS . RS.fromList . map fromIntegral
{-# INLINE fromList #-}

toList :: A.Impl enc => AlphaSet enc -> [Alpha enc]
toList = map fromIntegral . RS.toList . unAS
{-# INLINE toList #-}

instance Show (AlphaSet enc) where
    show = show . unAS

wrap :: RangeSet Int -> AlphaSet enc
wrap = AS

unwrap :: AlphaSet enc -> RangeSet Int
unwrap = unAS

