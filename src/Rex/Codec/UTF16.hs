module Rex.Codec.UTF16 (
    encode
  ) where

import Data.Char (ord)
import Data.Word (Word16)
import Data.Bits (shiftL, shiftR, (.&.))


encode1 :: Int -> Word16
encode1 = fromIntegral
{-# INLINE encode1 #-}

encode2 :: Int -> (Word16, Word16)
encode2 x = (h1, h2)
  where x' = x - 0x10000
        h1 = fromIntegral $ 0xD800 + shiftR x' 10
        h2 = fromIntegral $ 0xDC00 + x' .&. 0x3FF
{-# INLINE encode2 #-}

encode :: Int -> [Word16]
encode x
  | x <= 0xFFFF = if x >= 0xD800 && x <= 0xDFFF
                then error "Rex.Codec.UTF16.encode: encoding failed"
                else (\x -> [x]) (encode1 x)
  | otherwise = (\(x,y) -> [x,y]) (encode2 x)

