{
{-# LANGUAGE StrictData #-}
{-# LANGUAGE Rank2Types #-}

module Rex.Syntax.Parser.Lexer (
    Token(..)
  , LexMode
  , LayoutContext
  , LexerState
  , LexResult(..)
  , Lexer(..)
  , pushLexMode
  , popLexMode
  , pushLayout
  , popLayout
  , lexToken
  , runX
  , initialLexerState
  ) where

import Rex.Syntax.SrcLoc
import qualified Rex.Codec.UTF8 as UTF8

import Data.Word (Word8)
import Data.Char (ord, chr, isSpace, isUpper, isLower)
import Data.Maybe (listToMaybe)
import qualified Data.Map as M
import Text.Printf (printf)

import Debug.Trace (trace, traceShow)
}

-- -----------------------------------------------------------------------------
-- Character Sets

$white   = [\ \r\t\v\f]

$upper   = [A-Z]
$lower   = [a-z]
$alpha   = [$upper$lower]

$digit   = [0-9]
$binit   = [0-1]
$octit   = [0-7]
$hexit   = [0-9A-Fa-f]

$varhd   = [$alpha\_]
$varsc   = [$varhd$digit]

$special = [\ \!\"\#\$\%\&\'\(\)\*\+\,\-\.\/\:\;\<\=\>\?\@\[\\\]\^\_\`\{\|\}\~]
$graphic = . # $special

-- -----------------------------------------------------------------------------
-- Regular Expression Macros

@varid    = $varhd$varsc*

@decimal  = $digit+
@binary   = $binit+
@octal    = $octit+
@xdecimal = $hexit+

-- -----------------------------------------------------------------------------
-- Rules

rex :-

$white+           ;
"--".*\n          ;
"{-"              { discard "{-" "-}" }

<0> \n            { begin_ bol }
<bol> {
    \n            ;
    ()            { doBOL }
}

<0> {
    ";"           { special Tsemi }
    "="           { special Tequal }
    "%%"          { special Tdper }
    \%@varid      { lexVarid Tdirec }
}

<0> {
    "|"           { special Tvbar }
    "*"           { special Tstar }
    "+"           { special Tplus }
    "?"           { special Tques }
    \@@varid      { lexVarid Trevar }
}

<0, set>
    "["           { begin set Tobrack }
<set> {
    "]"           { end Tcbrack }
    "-"           { special Tminus }
    $graphic      { lexChar 0 }
    \\$special    { lexChar 1 }
    \\@decimal    { lexCoded 1 10 }
    \\b@binary    { lexCoded 2 2 }
    \\o@octal     { lexCoded 2 8 }
    \\x@xdecimal  { lexCoded 2 16 }
    \\$alpha+     { lexEscaped }
}

<0, set> {
    "("           { special Toparen }
    ")"           { special Tcparen }
    "#"           { special Thash }
    "."           { special Tdot }
    \$@varid      { lexVarid Tcsvar }
}

<0> "{" / { following (not . isSpace) }
                  { begin times Tocurly }
<times> {
    "}"           { end Tccurly }
    ","           { special Tcomma }
    @decimal      { lexInteger 0 10 Tnat }
    0b@binary     { lexInteger 2 2  Tnat }
    0o@octal      { lexInteger 2 8  Tnat }
    0x@xdecimal   { lexInteger 2 16 Tnat }
}

<0> "{"           { content "{" "}" Tcode }

<0> "<"           { begin codes Tless }
<codes> {
    ">"           { end Tgreat }
    ","           { special Tcomma }
    "0"|@varid    { lexVarid Tscvar }
}

<0> \"            { lexString }
<string> {
    \"            { undefined }
}

{
-- -----------------------------------------------------------------------------
-- Tokens

data Token = 
    Toparen          -- '('
  | Tcparen          -- ')'
  | Tobrack          -- '['
  | Tcbrack          -- ']'
  | Tocurly          -- '{'
  | Tccurly          -- '}'
  | Tequal           -- '='
  | Tless            -- '<'
  | Tgreat           -- '>'
  | Tminus           -- '-'
  | Tcomma           -- ','
  | Tsemi            -- ';'
  | Tdper            -- '%%'
  | Tdot             -- '.'
  | Thash            -- '#'
  | Tvbar            -- '|'
  | Tstar            -- '*'
  | Tplus            -- '+'
  | Tques            -- '?'
  | Tdirec !String   -- directive name
  | Tcsvar !String   -- variable name for character sets
  | Trevar !String   -- variable name for regular expressions
  | Tscvar !String   -- variable name for startcodes
  | Tchar !Char      -- character
  | Tstring !String  -- character sequence
  | Tcode !String    -- quoted code
  | Tnat !Int        -- non-negative integer
  | Tclose           -- 
  | Teof

instance Show Token where
    show Toparen        = "'('"
    show Tcparen        = "')'"
    show Tobrack        = "'['"
    show Tcbrack        = "']'"
    show Tocurly        = "'{'"
    show Tccurly        = "'}'"
    show Tequal         = "'='"
    show Tless          = "'<'"
    show Tgreat         = "'>'"
    show Tminus         = "'-'"
    show Tcomma         = "','"
    show Tsemi          = "';'"
    show Tdper          = "'%%'"
    show Tdot           = "'.'"
    show Thash          = "'#'"
    show Tvbar          = "'|'"
    show Tstar          = "'*'"
    show Tplus          = "'+'"
    show Tques          = "'?'"
    show (Tdirec name)  = show name
    show (Tcsvar name)  = show name
    show (Trevar name)  = show name
    show (Tscvar name)  = show name
    show (Tchar ch)     = show ch
    show (Tstring str)  = show str
    show (Tcode _code)  = "<code>"
    show (Tnat n)       = printf "<nat:%d>" n
    show Tclose         = "<close>"

-- -----------------------------------------------------------------------------
-- Predicates

type AccPred = ()

{-
followedBy :: (Char -> Bool) -> () -> AlexInput -> Int -> AlexInput -> Bool
followedBy ctype _ _ _ Inp { inp_stream = rest } = 
    case rest of c:_ | ctype c -> True; _ -> False
-}

following :: (Char -> Bool) -> () -> AlexInput -> Int -> AlexInput -> Bool
following ctype _ inp _ _ = ctype (alexInputPrevChar inp)

-- -----------------------------------------------------------------------------
-- Actions

type Action = AlexInput -> Int -> AlexInput -> Lexer (Located Token)

bracketsSpan :: String -> String -> AlexInput -> AlexInput -> Lexer Int
bracketsSpan op cl Inp { inp_loc = SrcLoc lin col } inp1 = let
    match len (x:xs) inp0 = 
        case alexGetChar inp0 of
        Just (c, inp1)
          | c == x    -> match (len + 1) xs inp1
          | otherwise -> pure $ Left (len + 1, inp1)
        Nothing -> error inp0
    match len [] inp0  = pure $ Right (len, inp0)

    go dep len inp0 = do
        r <- match 0 cl inp0
        case r of
            Left (n, _inp1) -> do
                r <- match 0 op inp0
                case r of
                    Left (n, inp2) -> go dep (len + n) inp2
                    Right (n, inp2) -> go (dep + 1) (len + n) inp2
            Right (n, inp1) -> if dep <= 1
                then do setInput inp1; pure len
                else go (dep - 1) (len + n) inp1
 in go 1 0 inp1
   where
    error inp = lexicalError inp
              $ printf "unterminated %s (line %d, column %d)" (show op) lin col

discard :: String -> String -> Action
discard op cl inp _ inp1 = do
    bracketsSpan op cl inp inp1
    lexToken

content :: String -> String -> (String -> Token) -> Action
content op cl tok inp@Inp { inp_loc = loc } _ inp1@Inp { inp_stream = txt } = do
    len <- bracketsSpan op cl inp inp1
    pure $ L loc (tok (take len txt))

begin_ :: LexMode -> Action
begin_ sc _ _ _ = do
    pushLexMode sc
    lexToken

begin :: LexMode -> Token -> Action
begin sc tok Inp { inp_loc = loc } _ _ = do
    pushLexMode sc
    pure $ L loc tok

end :: Token -> Action
end tok Inp { inp_loc = loc } _ _ = do
    popLexMode
    pure $ L loc tok

doBOL :: Action
doBOL Inp { inp_loc = loc@(SrcLoc _ col) } _ _ = do
    ctx <- topLayout
    case ctx of
        Just base | col < base -> traceShow (col, base) $ do
            popLayout
            pure $ L loc Tclose
        Just base | col == base -> do
            popLexMode
            pure $ L loc Tsemi
        _ -> do
            popLexMode
            lexToken

special :: Token -> Action
special tok Inp { inp_loc = loc } _ _ = pure $ L loc tok

lexVarid :: (String -> Token) -> Action
lexVarid tok Inp { inp_loc = loc, inp_stream = txt } len _ = 
    pure $ L loc (tok (take len txt))

lexInteger :: Int -> Int -> (Int -> Token) -> Action
lexInteger prefx_len radix tok inp@Inp { inp_loc = loc, inp_stream = txt } len _ = 
    case readInt radix (char2int radix) src of
        n -> pure $ L loc (tok n)
  where
    src = take (len - prefx_len) (drop prefx_len txt)

lexCoded :: Int -> Int -> Action
lexCoded prefx_len radix inp@Inp { inp_loc = loc, inp_stream = txt } len _ = 
    case readInt radix (char2int radix) src of
      x | validate x -> pure $ L loc (Tchar (chr x))
        | otherwise  -> repertoireError inp (take len txt)
  where
    src = take (len - prefx_len) (drop prefx_len txt)
    validate x = x <= ord maxBound

lexChar :: Int -> Action
lexChar prefx_len Inp { inp_loc = loc, inp_stream = txt } _ _ = 
    pure $ L loc (Tchar (head (drop prefx_len txt)))

lexEscaped :: Action
lexEscaped inp@Inp { inp_loc = loc, inp_stream = '\\':txt } len _ = 
    case M.lookup (take (len - 1) txt) escMap of
    Just c  -> pure $ L loc (Tchar c)
    Nothing -> lexicalError inp "invalid escape sequence"
  where
    escMap = M.fromList [
        ("NUL", '\NUL')
      , ("SOH", '\SOH')
      , ("STX", '\STX')
      , ("ETX", '\ETX')
      , ("EOT", '\EOT')
      , ("ENQ", '\ENQ')
      , ("ACK", '\ACK')
      , ("a"  , '\a')
      , ("b"  , '\b')
      , ("t"  , '\t')
      , ("n"  , '\n')
      , ("v"  , '\v')
      , ("f"  , '\f')
      , ("r"  , '\r')
      , ("SO" , '\SO')
      , ("SI" , '\SI')
      , ("DLE", '\DLE')
      , ("DC1", '\DC1')
      , ("DC2", '\DC2')
      , ("DC3", '\DC3')
      , ("DC4", '\DC4')
      , ("NAK", '\NAK')
      , ("SYN", '\SYN')
      , ("ETB", '\ETB')
      , ("CAN", '\CAN')
      , ("EM" , '\EM')
      , ("SUB", '\SUB')
      , ("ESC", '\ESC')
      , ("FS" , '\FS')
      , ("GS" , '\GS')
      , ("RS" , '\RS')
      , ("US" , '\US')
      , ("DEL", '\DEL')
      ]

lexString :: Action
lexString inp@Inp { inp_loc = loc@(SrcLoc lin col) } _ inp1@Inp { inp_stream = txt } = do
    len <- bracketsSpan "\"" "\"" inp inp1
    pure $ L loc (Tstring (take len txt))

char2int :: Int -> Char -> Int
char2int 10 = \c -> ord c - ord '0'
char2int  2 = \c -> ord c - ord '0'
char2int  8 = \c -> ord c - ord '0'
char2int 16 = \c -> case c of
               c | isUpper c -> ord c - ord 'A' + 10
                 | isLower c -> ord c - ord 'a' + 10
                 | otherwise -> ord c - ord '0'

readInt :: Int -> (Char -> Int) -> String -> Int
readInt radix c2i = 
    go 0
  where
    go n [] = n
    go n (c:cs) = n `seq` go (n * radix + c2i c) cs

-- -----------------------------------------------------------------------------
-- Lexing Monad

data InputStream = Inp {
    inp_loc    :: SrcLoc
  , inp_prevch :: Char
  , inp_bytes  :: [Word8]
  , inp_stream :: String
  }

type LexMode = Int  -- start code
type LayoutContext = Int  -- column number to judge offside or not

data LexerState = XS {
    xs_inp :: InputStream
  , xs_scd :: [LexMode]
  , xs_ctx :: [LayoutContext]
  }

data LexResult r = 
    LexError String
  | LexOk r LexerState

data Lexer a = 
    X { unX :: forall r. (a -> LexerState -> r)
             -> (String -> r)
             -> LexerState
             -> r }

runX :: Lexer r -> LexerState -> LexResult r
runX m = unX m LexOk LexError
{-# INLINE runX #-}

instance Functor Lexer where
    fmap f mx = X $ \cont -> unX mx (cont . f)
    {-# INLINE fmap #-}

instance Applicative Lexer where
    mf <*> mx = X $ \cont fail -> unX mf (\f -> unX mx (cont . f) fail) fail
    {-# INLINE (<*>) #-}

    pure x = X $ \cont _fail -> cont x
    {-# INLINE pure #-}

instance Monad Lexer where
    mx >>= k = X $ \cont fail -> unX mx (\x -> unX (k x) cont fail) fail
    {-# INLINE (>>=) #-}

instance MonadFail Lexer where
    fail msg = X $ \cont fail _st -> fail msg
    {-# INLINE fail #-}

getState :: Lexer LexerState
getState = X $ \cont _fail st -> cont st st
{-# INLINE getState #-}

getsState :: (LexerState -> a) -> Lexer a
getsState f = f `fmap` getState
{-# INLINE getsState #-}

updState :: (LexerState -> LexerState) -> Lexer ()
updState f = X $ \cont _fail st -> cont () $! f st
{-# INLINE updState #-}

getInput :: Lexer InputStream
getInput = getsState xs_inp
{-# INLINE getInput #-}

setInput :: InputStream -> Lexer ()
setInput inp = updState $ \st -> st { xs_inp = inp }
{-# INLINE setInput #-}

topLexMode :: Lexer LexMode
topLexMode = getsState (head . xs_scd)
{-# INLINE topLexMode #-}

pushLexMode :: LexMode -> Lexer ()
pushLexMode scd = updState $ \st@XS { xs_scd = scds } -> 
    st { xs_scd = scd:scds }
{-# INLINE pushLexMode #-}

popLexMode :: Lexer ()
popLexMode = updState $ \st@XS { xs_scd = _:scds } -> 
    st { xs_scd = scds }
{-# INLINE popLexMode #-}

topLayout :: Lexer (Maybe LayoutContext)
topLayout = getsState (listToMaybe . xs_ctx)
{-# INLINE topLayout #-}

pushLayout :: LayoutContext -> Lexer ()
pushLayout ctx = updState $ \st@XS { xs_ctx = ctxs } -> 
    st { xs_ctx = ctx:ctxs }
{-# INLINE pushLayout #-}

popLayout :: Lexer ()
popLayout = updState $ \st@XS { xs_ctx = _:ctxs } -> 
    st { xs_ctx = ctxs }
{-# INLINE popLayout #-}

lexicalError :: AlexInput -> String -> Lexer a
lexicalError (Inp (SrcLoc lin col) chr bytes str) detail = let
    msg = "lexical error at "
       ++ case str of
           []  -> "EOF"
           c:_ -> printf "(line %d, column %d)" lin col
       ++ ": " ++ detail
 in fail msg

repertoireError :: AlexInput -> String -> Lexer a
repertoireError inp ch = 
    lexicalError inp 
  $ printf "character '%s' is not in Unicode repertoire" ch

-- -----------------------------------------------------------------------------
-- Alex interface

type AlexInput = InputStream

advanceSrcLoc :: Char -> SrcLoc -> SrcLoc
advanceSrcLoc '\n' (SrcLoc l c) = SrcLoc (l + 1) 1
advanceSrcLoc '\t' (SrcLoc l c) = SrcLoc l (c + 8)
advanceSrcLoc  _   (SrcLoc l c) = SrcLoc l (c + 1)

alexGetChar :: AlexInput -> Maybe (Char, AlexInput)
alexGetChar (Inp loc chr [] str) = 
    case str of
    ""     -> Nothing
    c:str' -> Just (c, Inp (advanceSrcLoc c loc) c [] str')

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte (Inp loc chr bytes str) = 
    case (bytes, str) of
    ([], "")      -> Nothing
    ([], c:str')  -> case UTF8.encode (ord c) of
         b:bytes' -> Just (b, Inp (advanceSrcLoc c loc) c bytes' str')
    (b:bytes', _) -> Just (b, Inp loc chr bytes' str)

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar Inp { inp_prevch = c } = c

-- -----------------------------------------------------------------------------
-- The lexing function

lexToken :: Lexer (Located Token)
lexToken = do
    inp <- getInput
    scd <- topLexMode
    case alexScanUser () inp scd of
        AlexEOF -> 
            pure (L (inp_loc inp) Teof)
        AlexSkip inp' len -> do
            setInput inp'
            lexToken
        AlexToken inp' len act -> do
            setInput inp'
            act inp len inp'
        AlexError inp'@Inp { inp_stream = c:_ } -> 
            lexicalError inp' $ printf "unexpected %s" (show c)

-- -----------------------------------------------------------------------------
-- Running lexers

initialLexerState :: String -> LexerState
initialLexerState str = XS {
    xs_inp = Inp (SrcLoc 1 1) '\n' [] str
  , xs_scd = [0]
  , xs_ctx = []
  }
}
