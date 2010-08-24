{
module Text.OMDT.Lexer where
import Data.Version (Version, parseVersion)
import Numeric
import Text.ParserCombinators.ReadP (readP_to_S)
}

%wrapper "posn"

$digit = [0-9]
$alpha = [a-zA-Z]
$dquot = \"

@version    = v $digit+ (\. $digit+)*
@atom       = $alpha [$alpha $digit]*
@frac       = $digit+ \. $digit*
@int        = $digit+
@string     = \" ([^ \\ $dquot] | \\ .)* \"

tokens :- 
    \; .*                       ;
    $white+                     ;
    \(                          { token' OpenParen    }
    \)                          { token' CloseParen   }
    \[                          { token' OpenBracket  }
    \]                          { token' CloseBracket }
    \/                          { token' Slash        }
    @version                    { version }
    @atom                       { String  }
    @frac                       { frac    }
    @int                        { int     }
    @string                     { string  }

{
token' tok pos str = tok pos

data Token
    = OpenParen     !AlexPosn
    | CloseParen    !AlexPosn
    | OpenBracket   !AlexPosn
    | CloseBracket  !AlexPosn
    | Slash         !AlexPosn
    
    | Version       !AlexPosn !Version
    | String        !AlexPosn !String
    | Int           !AlexPosn !String !Integer
    | Frac          !AlexPosn !String !Rational
    | EOF
    deriving (Eq, Show)

tokenAlexPosn (OpenParen     posn)      = Just posn
tokenAlexPosn (CloseParen    posn)      = Just posn
tokenAlexPosn (OpenBracket   posn)      = Just posn
tokenAlexPosn (CloseBracket  posn)      = Just posn
tokenAlexPosn (Slash         posn)      = Just posn
tokenAlexPosn (Version       posn _)    = Just posn
tokenAlexPosn (String        posn _)    = Just posn
tokenAlexPosn (Int           posn _ _)  = Just posn
tokenAlexPosn (Frac          posn _ _)  = Just posn
tokenAlexPosn EOF                       = Nothing

version pos ('v':raw) = case last (readP_to_S parseVersion raw) of
    (v, "") -> Version pos v
    _       -> error ("version: \"impossible\" parse error on " ++ show raw)

int pos raw = case reads raw of
    [(i,"")] -> Int pos raw i
    _        -> error ("int: \"impossible\" parse error on " ++ show raw)
    
frac pos raw = case readFloat raw of
    [(f,"")] -> Frac pos raw f
    _   -> error ("frac: \"impossible\" parse error on " ++ show raw)

string pos raw = String pos (read raw)

}