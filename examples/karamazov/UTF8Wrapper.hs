module UTF8Wrapper where

import Data.Word (Word8)
import Data.Bits (shiftR, (.&.))
import Data.Char (ord)


type RexInput = ([Word8], String)

utf8Encode :: Char -> [Word8]
utf8Encode c = map fromIntegral $ case ord c of
  x | x <= 0x7F   -> [ x ]
    | x <= 0x7FF  -> [ 0xC0 + shiftR x 6
                     , 0x80 +        x    .&. 0x3F ]
    | x <= 0xFFFF -> [ 0xE0 + shiftR x 12
                     , 0x80 + shiftR x 6  .&. 0x3F
                     , 0x80 +        x    .&. 0x3F ]
    | otherwise   -> [ 0xF0 + shiftR x 18
                     , 0x80 + shiftR x 12 .&. 0x3F
                     , 0x80 + shiftR x 6  .&. 0x3F
                     , 0x80 +        x    .&. 0x3F ]

rexGetChar :: Int -> RexInput -> Maybe (Word8, Int, RexInput)
rexGetChar l (bs, cs) = l `seq` case (bs, cs) of
    ([], [])   -> Nothing
    ([], c:cs) -> case utf8Encode c of
          b:bs -> Just (b, l + 1, (bs, cs))
    (b:bs, cs) -> Just (b, l, (bs, cs))

fromString :: String -> RexInput
fromString s = ([], s)

toString :: RexInput -> String
toString (_, s) = s

