{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Rex.Data.Alpha (
    Alpha
  , Impl(..)
  ) where

import Rex.Codec.Types (Encoding)


type family Alpha (enc :: Encoding) :: *

-- A type class to and only to overload the following stuffs
class (Integral (Alpha enc)) => Impl (enc :: Encoding) where

