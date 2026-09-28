{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Rex.Data.Alpha.UTF8 where

import Rex.Data.Alpha
import Rex.Codec.Types (Encoding(UTF8))

-- for implementation
import Data.Word (Word8)


type instance Alpha UTF8 = Word8
instance Impl UTF8 where

