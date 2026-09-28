{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Rex.Data.Alpha.UTF16 where

import Rex.Data.Alpha
import Rex.Codec.Types (Encoding(UTF16))

-- for implementation
import Data.Word (Word16)


type instance Alpha UTF16 = Word16
instance Impl UTF16 where

