{-# LANGUAGE DataKinds #-}

-- 
-- Implementation of stuffs in Rex.Data.Regex, for UTF-32 encoding
-- 
module Rex.Data.Regex.UTF32 () where

import Rex.Data.Regex
import Rex.Codec.Types (Encoding(UTF32))

-- for implementation
import qualified Rex.Data.CharSet as CS
import qualified Rex.Data.AlphaSet as AS


instance Impl UTF32 where
    encode = Ch . AS.wrap . CS.unwrap

