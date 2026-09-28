{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Rex.Data.Alpha.UTF32 where

import Rex.Data.Alpha
import Rex.Codec.Types (Encoding(UTF32))

-- for implementation
import Data.Word (Word32)


type instance Alpha UTF32 = Word32
instance Impl UTF32 where

