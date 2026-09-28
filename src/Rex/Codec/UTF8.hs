module Rex.Codec.UTF8 (
    encode1
  , encode2
  , encode3
  , encode4
  , encode
  ) where

import Data.Word (Word8)
import Data.Bits (shiftR, (.&.))

import Prelude hiding (length)


encode1 :: Int -> Word8
encode1 = fromIntegral
{-# INLINE encode1 #-}

encode2 :: Int -> (Word8, Word8)
encode2 x = 
    (o1, o2)
  where o1 = fromIntegral $ 0xC0 + shiftR x 6
        o2 = fromIntegral $ 0x80 + x .&. 0x3F
{-# INLINE encode2 #-}

encode3 :: Int -> (Word8, Word8, Word8)
encode3 x = 
    (o1, o2, o3)
  where o1 = fromIntegral $ 0xE0 + shiftR x 12
        o2 = fromIntegral $ 0x80 + shiftR x 6 .&. 0x3F
        o3 = fromIntegral $ 0x80 + x .&. 0x3F
{-# INLINE encode3 #-}

encode4 :: Int -> (Word8, Word8, Word8, Word8)
encode4 x = 
    (o1, o2, o3, o4)
  where o1 = fromIntegral $ 0xF0 + shiftR x 18
        o2 = fromIntegral $ 0x80 + shiftR x 12 .&. 0x3F
        o3 = fromIntegral $ 0x80 + shiftR x 6 .&. 0x3F
        o4 = fromIntegral $ 0x80 + x .&. 0x3F
{-# INLINE encode4 #-}

encode :: Int -> [Word8]
encode x
  | x <= 0x7F   = (\x -> [x]) (encode1 x)
  | x <= 0x7FF  = (\(x,y) -> [x,y]) (encode2 x)
  | x <= 0xFFFF = (\(x,y,z) -> [x,y,z]) (encode3 x)
  | otherwise   = (\(x,y,z,w) -> [x,y,z,w]) (encode4 x)
