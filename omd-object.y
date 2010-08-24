{
module Text.OMDT.ObjectParser
    ( omdObject, OMDScalar(..)
    ) where
import qualified Text.OMDT.Lexer as Lex

import Data.List
import Data.Object (Object(..))
import Data.Version (Version)
}

%name omdObject omd_object
%tokentype  { Lex.Token }
%error      { parseError }

%token
    '(' { Lex.OpenParen      _ }
    ')' { Lex.CloseParen     _ }
    '[' { Lex.OpenBracket    _ }
    ']' { Lex.CloseBracket   _ }
    '/' { Lex.Slash          _ }
    TRUE                { Lex.String _ $$@"TRUE"                }
    Yes                 { Lex.String _ $$@"Yes"                 }
    FALSE               { Lex.String _ $$@"FALSE"               }
    No                  { Lex.String _ $$@"No"                  }
    
    version         { Lex.Version _ $$ }
    string          { Lex.String  _ $$ }
    int             { Lex.Int   _ _ $$ }
    frac            { Lex.Frac  _ _ $$ }

%%

parens(p)   : '(' p ')'                 { $2 }
tagged(p,q) : parens(both(p,q))         { $1 }

omd_object
    : list(tagged(string, omd_object))  { Mapping $1 }
    | scalar                            { Scalar  $1 }
    | omd_object list1(omd_object)              { Sequence ($1:$2)}

scalar
    : string                    { String $1 }
    | boolean                   { Bool   $1 }
    | int                       { Int    $1 }
    | frac                      { Frac   $1 }
    | int '/' int '/' int       { Date $1 $3 $5 }
    | '[' int ']'               { Footnote $2 }
    | version                   { Version $1 }
    

boolean
    : TRUE                      { True  }
    | Yes                       { True  }
    | FALSE                     { False }
    | No                        { False }

-- Some useful meta-parsers, copied from the Happy user guide:

opt(p)          : p                   { Just $1 }
                |                     { Nothing }

rev_list1(p)    : p                   { [$1] }
                | rev_list1(p) p      { $2 : $1 }

fst(p,q)        : p q                 { $1 }
snd(p,q)        : p q                 { $2 }
both(p,q)       : p q                 { ($1,$2) }

list1(p)        : rev_list1(p)        { reverse $1 }
list(p)         : list1(p)            { $1 }
                |                     { [] }
sep1(p,q)       : p list(snd(q,p))    { $1 : $2 }

{

data OMDScalar
    = String    String
    | Bool      Bool
    | Int       Integer
    | Frac      Rational
    | Date      Integer Integer Integer
    | Footnote  Integer
    | Version   Version
    deriving (Eq, Show)

parseError tokens = error ("Parse error near " ++ show (head tokens))

}