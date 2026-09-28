{
{-# LANGUAGE Rank2Types #-}

module Rex.Syntax.Parser.Parser (
    Code
  , Action
  , CsName
  , ReName
  , ScName
  , CsExpr(..)
  , ReExpr(..)
  , Decl(..)
  , Rule(..)
  , Bind
  , Module(..)
  , parseModule
  ) where

import Rex.Syntax.SrcLoc
import Rex.Syntax.Parser.Lexer
import Data.Foldable (foldl')
import Control.Monad (liftM, ap)
import Text.Printf (printf)

import Debug.Trace (trace, traceShow)
}

%tokentype { Located Token }
%lexer { lexer } { L _ Teof }
%monad { Parser } { >>= } { pure }
%error { inputError }

%token
 "("     { L _ Toparen }
 ")"     { L _ Tcparen }
 "["     { L _ Tobrack }
 "]"     { L _ Tcbrack }
 "{"     { L _ Tocurly }
 "}"     { L _ Tccurly }
 ","     { L _ Tcomma }
 ";"     { L _ Tsemi }
 "%%"    { L _ Tdper }
 "="     { L _ Tequal }
 "<"     { L _ Tless }
 "-"     { L _ Tminus }
 ">"     { L _ Tgreat }
 "."     { L _ Tdot }
 "#"     { L _ Thash }
 "|"     { L _ Tvbar }
 "*"     { L _ Tstar }
 "+"     { L _ Tplus }
 "?"     { L _ Tques }
 CSVAR   { L _ (Tcsvar $$) }
 REVAR   { L _ (Trevar $$) }
 SCVAR   { L _ (Tscvar $$) }
 CHAR    { L _ (Tchar $$) }
 STRING  { L _ (Tstring $$) }
 CODE    { L _ (Tcode $$) }
 NAT     { L _ (Tnat $$) }
 CLOSE   { L _ Tclose }

%name rexModule module

%%

module :: { Module }
  : opt_code open decls sep rules close opt_code
    { Module $1 $3 $5 $7 }

sep :: { () }
  : "%%" ";"
    { () }

decls :: { [Decl] }
  : decls decl ";"
    { $2 : $1 }
  | {- empty -}
    { [] }

decl :: { Decl }
  : CSVAR "=" csexp
    { CsBind $1 $3 }
  | REVAR "=" reexp
    { ReBind $1 $3 }

csexp :: { CsExpr }
  : csdiff
    { $1 }

csdiff :: { CsExpr }
  : csdiff "#" csatom
    { CsDiff $1 $3 }
  | csatom
    { $1 }

csatom :: { CsExpr }
  : "[" csset "]"
    { CsUnion $2 }
  | "."
    { CsNonNL }
  | CHAR
    { CsOne $1 }
  | CHAR "-" CHAR
    { CsRange $1 $3 }
  | CSVAR
    { CsVar $1 }

csset :: { [CsExpr] }
  : csset csexp
    { $2 : $1 }
  | csexp
    { [$1] }

reexp :: { ReExpr }
  : reunion
    { reUnions $1 }

reunion :: { [ReExpr] }
  : reunion "|" recat
    { reCats (reverse $3) : $1 }
  | recat
    { [reCats (reverse $1)] }

recat :: { [ReExpr] }
  : recat rerep
    { $2 : $1 }
  | rerep
    { [$1] }

rerep :: { ReExpr }
  : reatom "{" NAT "," "}"
    { reRepFrom $1 $3 }
  | reatom "{" NAT "," NAT "}"
    { reRepFromTo $1 $3 $5 }
  | reatom "{" NAT "}"
    { reRep $1 $3 }
  | reatom "*"
    { reStar $1 }
  | reatom "+"
    { rePlus $1 }
  | reatom "?"
    { reQues $1 }
  | reatom
    { $1 }

reatom :: { ReExpr }
  : "(" ")"
    { ReEps }
  | csexp
    { ReCh $1 }
  | STRING
    { reString $1 }
  | REVAR
    { ReVar $1 }
  | "(" reexp ")"
    { $2 }

rules :: { [Rule] }
  : rules rule ";"
    { $2 : $1 }
  | {- empty -}
    { [] }

rule :: { Rule }
  : "<" sclist ">" open binds close
    { Rule $2 $5 }
  | bind
    { Rule [] [$1] }

sclist :: { [ScName] }
  : sclist "," SCVAR
    { $3 : $1 }
  | SCVAR
    { [$1] }

binds :: { [Bind] }
  : binds ";" bind
    { $3 : $1 }
  | bind
    { [$1] }

bind :: { Bind }
  : reexp action
    { ($1, $2) }

action :: { Action }
  : ";"
    { Nothing }
  | code
    { Just $1 }

opt_code :: { Code }
  : code
    { $1 }
  | {- empty -}
    { "" }

code :: { Code }
  : CODE
    { $1 }

open :: { () }
  : {- empty -}
    {% pushLayout' }

close :: { () }
  : CLOSE
    {  () }
  | {- empty -}
    {% popLayout' }

{
-- -----------------------------------------------------------------------------
-- Syntax

-- Quoted source code
type Code = String
type Action = Maybe Code

-- Variable name of character set
type CsName = String
-- Variable name of regular expression
type ReName = String
-- Variable name of start code
type ScName = String

-- Expression of character sets
data CsExpr = 
    CsEmpty
  | CsNonNL
  | CsOne Char
  | CsRange Char Char
  | CsUnion [CsExpr]
  | CsDiff CsExpr CsExpr
  | CsVar CsName
    deriving Show

-- Expression of regular expressions
data ReExpr = 
    ReEmpty
  | ReEps 
  | ReCh CsExpr
  | ReUnion ReExpr ReExpr
  | ReCat ReExpr ReExpr
  | ReStar ReExpr
  | ReVar ReName
    deriving Show

-- 
data Decl = 
    CsBind CsName CsExpr
  | ReBind ReName ReExpr
    deriving Show

data Rule = 
    Rule [ScName] [Bind]
    deriving Show

type Bind = (ReExpr, Action)

data Module = Module {
    rexPrelude :: Code
  , rexDecls :: [Decl]
  , rexRules :: [Rule]
  , rexPostlude :: Code
  } deriving Show

-- -----------------------------------------------------------------------------
-- Constructors

reUnion :: ReExpr -> ReExpr -> ReExpr
reUnion ReEmpty x = x
reUnion x ReEmpty = x
reUnion x y       = x `ReUnion` y

reCat :: ReExpr -> ReExpr -> ReExpr
reCat ReEmpty _ = ReEmpty
reCat _ ReEmpty = ReEmpty
reCat ReEps x   = x
reCat x ReEps   = x
reCat x y       = x `ReCat` y

reStar :: ReExpr -> ReExpr
reStar ReEmpty = ReEps
reStar ReEps   = ReEps
reStar x       = ReStar x

rePlus :: ReExpr -> ReExpr
rePlus ReEmpty = ReEmpty
rePlus ReEps   = ReEps
rePlus x       = x `ReCat` ReStar x

reQues :: ReExpr -> ReExpr
reQues ReEmpty = ReEps
reQues ReEps   = ReEps
reQues x       = ReEps `ReUnion` x

reUnions :: [ReExpr] -> ReExpr
reUnions = foldl' reUnion ReEmpty

reCats :: [ReExpr] -> ReExpr
reCats = foldl' reCat ReEps

reRep :: ReExpr -> Int -> ReExpr
reRep x n = foldl' reCat ReEps (replicate n x)

reRepFromTo :: ReExpr -> Int -> Int -> ReExpr
reRepFromTo x m n = foldl' (\x y -> x `reCat` reQues y) (reRep x m) (replicate (n - m) x)

reRepFrom :: ReExpr -> Int -> ReExpr
reRepFrom x n = reRep x n `reCat` reStar x

reString :: String -> ReExpr
reString = foldl' reCat ReEps . map (ReCh . CsOne)

-- -----------------------------------------------------------------------------
-- Parsing Monad

-- Pair of a lexer state and the token derived from the state.
data TokenAt = 
    !LexerState :-> ParseResult (Located Token)
    -- the second field should be lazy, see documentation of 'liftX'

tokenAt :: LexerState -> TokenAt
tokenAt st = 
    st :-> dv
  where
    dv = case runX lexToken st of
        LexError msg  -> ParseError msg
        LexOk tok@(L (SrcLoc _ col) _) st' -> 
            ParseOk tok PS { ps_lastcol = col, ps_inp = tokenAt st' }

data ParserState = PS {
    ps_lastcol :: !Int  -- column number of last token
  , ps_inp :: !TokenAt
  }

data ParseResult r = 
    ParseError !String
  | ParseOk !r !ParserState

newtype Parser r = 
    Y { runY :: ParserState -> ParseResult r }

instance Functor Parser where
    fmap f mx = Y $ \ps0 -> case runY mx ps0 of
        ParseError msg -> ParseError msg
        ParseOk x ps1 -> ParseOk (f x) ps1
    {-# INLINE fmap #-}

instance Applicative Parser where
    mf <*> mx = Y $ \ps0 -> case runY mf ps0 of
        ParseError msg -> ParseError msg
        ParseOk f ps1  -> case runY mx ps1 of
            ParseError msg -> ParseError msg
            ParseOk x ps2 -> ParseOk (f x) ps2
    {-# INLINE (<*>) #-}

    pure x = Y $ \ps -> ParseOk x ps
    {-# INLINE pure #-}

instance Monad Parser where
    mx >>= k = Y $ \ps0 -> case runY mx ps0 of
        ParseError msg -> ParseError msg
        ParseOk x ps1  -> runY (k x) ps1
    {-# INLINE (>>=) #-}

instance MonadFail Parser where
    fail msg = Y $ \_ps -> ParseError msg
    {-# INLINE fail #-}

inputError :: Located Token -> Parser a
inputError (L (SrcLoc lin col) tok) = 
    fail $ printf "parse error on input %s (line %d, column %d)"
                  (show tok) lin col

lexer :: (Located Token -> Parser r) -> Parser r
lexer cont = Y $ \PS { ps_inp = _ :-> dv } -> case dv of
    ParseError msg  -> ParseError msg
    ParseOk tok ps1 -> runY (cont tok) ps1

getState :: Parser ParserState
getState = Y $ \ps -> ParseOk ps ps
{-# INLINE getState #-}

getsState :: (ParserState -> a) -> Parser a
getsState f = f `fmap` getState
{-# INLINE getsState #-}

-- Lift an impure lexing action into parsing action. Note that impure actions
-- update lexer state so derived token at the original state is no longer valid.
-- This causes lexing one more at the new state.
-- See field strictness of 'TokenAt'.
liftX :: Lexer a -> Parser a
liftX m = Y $ \ps@PS { ps_inp = st :-> _ } -> case runX m st of
    LexError msg -> ParseError msg
    LexOk x st'  -> ParseOk x ps { ps_inp = tokenAt st' }
{-# INLINE liftX #-}

pushLayout' :: Parser ()
pushLayout' = do
    col <- getsState ps_lastcol
    liftX $ pushLayout col

popLayout' :: Parser ()
popLayout' = liftX popLayout

parse :: Parser a -> String -> Either String a
parse m str = 
    case runY m initPS of
        ParseError msg -> Left msg
        ParseOk r _st  -> Right r
  where
    initPS = PS { 
        ps_lastcol = 1
      , ps_inp = tokenAt (initialLexerState str)
      }

parseModule :: String -> Either String Module
parseModule = parse rexModule
}
