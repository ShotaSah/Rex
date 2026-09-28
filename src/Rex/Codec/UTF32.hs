module Rex.Codec.UTF32 (
    encode
  ) where

import Data.Word (Word32)


encode :: Int -> Word32
encode = fromIntegral

