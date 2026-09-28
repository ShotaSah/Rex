module Rex.Codec.Types (
    Encoding(..)
  ) where

data Encoding = 
    UTF8
  | UTF16
  | UTF32

instance Show Encoding where
    show UTF8  = "UTF-8"
    show UTF16 = "UTF-16"
    show UTF32 = "UTF-32"
