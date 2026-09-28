{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Rex.Data.Regex (
    Regex(..)
  , empty
  , union
  , Impl(..)
  ) where

import Rex.Data.CharSet (CharSet)
import Rex.Data.AlphaSet (AlphaSet)
import Rex.Codec.Types (Encoding)


data Regex (enc :: Encoding) = 
    Empty
  | Eps
  | Ch !(AlphaSet enc)
  | Union !(Regex enc) !(Regex enc)
  | Cat !(Regex enc) !(Regex enc)
  | Star !(Regex enc)

empty :: Regex enc
empty = Empty

union :: Regex enc -> Regex enc -> Regex enc
union Empty x = x
union x Empty = x
union x y     = x `Union` y

instance Show (Regex enc) where
    showsPrec _ Empty = showString "{}"
    showsPrec _ Eps = showString "()"
    showsPrec _ (Ch set) = shows set
    showsPrec p (Union x y) = showParen (p > 2) $
        showsPrec 2 x . showString " | " . showsPrec 3 y
    showsPrec p (Cat x y) = showParen (p > 3) $
        showsPrec 3 x . showsPrec 4 y
    showsPrec p (Star x) = showParen (p > 4) $
        showsPrec 10 x . showChar '*'

-- A type class to and only to overload the following stuffs
class Impl (enc :: Encoding) where
    encode :: CharSet -> Regex enc

