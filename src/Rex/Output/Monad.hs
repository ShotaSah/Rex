module Rex.Output.Monad (
    Output
  , output
  , puts
  , putsLn
  , putc
  , nest
  ) where

import Text.Printf (PrintfType, printf)


newtype Output a = Out
  { unOut :: (a -> Bool -> ShowS) -> Int -> Bool -> ShowS }

output :: Output () -> String
output m = unOut m (\() _ -> id) 0 False ""
{-# INLINE output #-}

instance Functor Output where
    fmap f mx = Out $ \cont -> unOut mx (cont . f)
    {-# INLINE fmap #-}

instance Applicative Output where
    mf <*> mx = Out $ \cont n -> unOut mf (\f -> unOut mx (cont . f) n) n
    {-# INLINE (<*>) #-}
    pure x = Out $ \cont _ -> cont x
    {-# INLINE pure #-}

instance Monad Output where
    mx >>= k = Out $ \cont n -> unOut mx (\x -> unOut (k x) cont n) n
    {-# INLINE (>>=) #-}

puts :: String -> Output ()
puts s = Out $ \cont n bol -> 
    (if bol then showString (replicate n ' ') else id)
  . showString s . cont () False
{-# INLINE puts #-}

putsLn :: String -> Output ()
putsLn s = Out $ \cont n bol -> 
   (if bol then showString (replicate n ' ') else id)
  . showString s . showChar '\n' . cont () True
{-# INLINE putsLn #-}

putc :: Char -> Output ()
putc c = Out $ \cont n bol -> 
    (if bol then showString (replicate n ' ') else id)
  . showChar c . cont () (c == '\n')
{-# INLINE putc #-}

nest :: Int -> Output a -> Output a
nest l mx = Out $ \cont n -> unOut mx cont (n + l)
{-# INLINE nest #-}

