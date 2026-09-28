{-# LANGUAGE DataKinds #-}

-- 
-- Implementation of stuffs in Rex.Data.Regex, for UTF-8 encoding
-- 
module Rex.Data.Regex.UTF8 () where

import Rex.Data.Regex
import Rex.Codec.Types (Encoding(UTF8))

-- for implementation
import qualified Rex.Data.RangeSet as RS
import qualified Rex.Data.CharSet as CS
import qualified Rex.Data.AlphaSet as AS
import Rex.Data.Regex.Surrogate (surrogates)
import Data.Foldable (foldl')


instance Impl UTF8 where
    encode cset = 
        foldl' union empty [oseq1, oseq2, oseq3, oseq4]
      where
        (octs1,cset1) = RS.splitLE 0x7F (CS.unwrap cset)
        (octs2,cset2) = RS.splitLE 0x7FF cset1
        (octs3,octs4) = RS.splitLE 0xFFFF cset2

        gen_oseq = surrogates 0x40 0x0

        oseq1 = Ch (AS.wrap octs1)
        oseq2 = gen_oseq [0x80, 0xC0] octs2
        oseq3 = gen_oseq [0x80, 0x80, 0xE0] octs3
        oseq4 = gen_oseq [0x80, 0x80, 0x80, 0xF0] octs4

