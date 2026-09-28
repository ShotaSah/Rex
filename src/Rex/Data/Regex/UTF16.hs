{-# LANGUAGE DataKinds #-}

-- 
-- Implementation of stuffs in Rex.Data.Regex, for UTF-16 encoding
-- 
module Rex.Data.Regex.UTF16 () where

import Rex.Data.Regex
import Rex.Codec.Types (Encoding(UTF16))

-- for implementation
import qualified Rex.Data.RangeSet as RS
import qualified Rex.Data.CharSet as CS
import qualified Rex.Data.AlphaSet as AS
import Rex.Data.Regex.Surrogate (surrogates)
import Data.Foldable (foldl')


instance Impl UTF16 where
    encode cset = 
        foldl' union empty [hseq1, hseq2]
      where
        (cset1,hexs2) = RS.splitLE 0xFFFF (CS.unwrap cset)
        hexs1 = cset1 RS.\\ RS.range 0xD800 0xDFFF

        hseq1 = Ch (AS.wrap hexs1)
        hseq2 = surrogates 0x400 0x10000 [0xDC00, 0xD800] hexs2

