{
module Text.OMDT.Parser where
import Text.OMDT.Lexer
import Data.List
import Data.Version (Version)
}

%name omd omd
%tokentype  { Token }
%error      { parseError }

%token
    '(' { OpenParen      _ }
    ')' { CloseParen     _ }
    '[' { OpenBracket    _ }
    ']' { CloseBracket   _ }
    '/' { Slash          _ }
    OMDT                { String _ $$@"OMDT"                }
    ObjectModel         { String _ $$@"ObjectModel"         }
    Name                { String _ $$@"Name"                }
    VersionNumber       { String _ $$@"VersionNumber"       }
    Type                { String _ $$@"Type"                }
    FOM                 { String _ $$@"FOM"                 }
    SOM                 { String _ $$@"SOM"                 }
    OTHER               { String _ $$@"OTHER"               }
    Purpose             { String _ $$@"Purpose"             }
    ApplicationDomain   { String _ $$@"ApplicationDomain"   }
    SponsorOrgName      { String _ $$@"SponsorOrgName"      }
    POCHonorificName    { String _ $$@"POCHonorificName"    }
    POCFirstName        { String _ $$@"POCFirstName"        }
    POCLastName         { String _ $$@"POCLastName"         }
    POCOrgName          { String _ $$@"POCOrgName"          }
    POCPhone            { String _ $$@"POCPhone"            }
    POCEmail            { String _ $$@"POCEmail"            }
    ModificationDate    { String _ $$@"ModificationDate"    }
    MOMVersion          { String _ $$@"MOMVersion"          }
    FEDname             { String _ $$@"FEDname"             }
    EnumeratedDataType  { String _ $$@"EnumeratedDataType"  }
    ComplexDataType     { String _ $$@"ComplexDataType"     }
    RoutingSpace        { String _ $$@"RoutingSpace"        }
    Class               { String _ $$@"Class"               }
    Interaction         { String _ $$@"Interaction"         }
    Note                { String _ $$@"Note"                }
    TRUE                { String _ $$@"TRUE"                }
    Yes                 { String _ $$@"Yes"                 }
    FALSE               { String _ $$@"FALSE"               }
    No                  { String _ $$@"No"                  }
    Cardinality         { String _ $$@"Cardinality"         }
    
    version         { Version _ $$ }
    other_string    { String  _ $$ }
    int             { Int   _ _ $$ }
    frac            { Frac  _ _ $$ }

%%

string
    : other_string              { $1 }
    | Name                      { $1 }
    | ObjectModel               { $1 }
    | OMDT                      { $1 }
    | VersionNumber             { $1 }
    | Type                      { $1 }
    | FOM                       { $1 }
    | SOM                       { $1 }
    | OTHER                     { $1 }
    | Purpose                   { $1 }
    | ApplicationDomain         { $1 }
    | SponsorOrgName            { $1 }
    | POCHonorificName          { $1 }
    | POCFirstName              { $1 }
    | POCLastName               { $1 }
    | POCOrgName                { $1 }
    | POCPhone                  { $1 }
    | POCEmail                  { $1 }
    | ModificationDate          { $1 }
    | MOMVersion                { $1 }
    | FEDname                   { $1 }
    | EnumeratedDataType        { $1 }
    | ComplexDataType           { $1 }
    | RoutingSpace              { $1 }
    | Class                     { $1 }
    | Interaction               { $1 }
    | Note                      { $1 }
    | TRUE                      { $1 }
    | Yes                       { $1 }
    | FALSE                     { $1 }
    | No                        { $1 }
    | Cardinality               { $1 }

date : int '/' int '/' int      { Date $1 $3 $5 }
boolean
    : TRUE                      { True  }
    | Yes                       { True  }
    | FALSE                     { False }
    | No                        { False }

footnote    : '[' int ']'               { $2 }

cardinality
    : tagged(Cardinality, string)       { $1 }

parens(p)   : '(' p ')'                 { $2 }
tagged(p,q) : parens(snd(p,q))          { $1 }

omd :   object_model   { $1 }

object_model : 
    tagged(OMDT, version)
    tagged(ObjectModel, both(object_model_header, list(data_type)))
        { ObjectModel $1 (fst $2) (snd $2) }

object_model_header : 
    name
    version_number
    type
    purpose
    application_domain
    sponsor_org_name
    poc
    modification_date
    mom_version
    fed_name
       { ObjectModelHeader $1 $2 $3 $4 $5 $6 $7 $8 $9 $10 }

name                : tagged(Name,              string)     { $1 }
version_number      : tagged(VersionNumber,     string)     { $1 }
type                : tagged(Type,              om_type)    { $1 }
purpose             : tagged(Purpose,           string)     { $1 }
application_domain  : tagged(ApplicationDomain, string)     { $1 }
sponsor_org_name    : tagged(SponsorOrgName,    string)     { $1 }

om_type
    : FOM                                                   { FOM   }
    | SOM                                                   { SOM   }
    | OTHER                                                 { OTHER }

poc :
    poc_honorific_name
    poc_first_name
    poc_last_name
    poc_org_name
    poc_phone
    poc_email
        { POC $1 $2 $3 $4 $5 $6 }

poc_honorific_name  : tagged(POCHonorificName,  string)     { $1 }
poc_first_name      : tagged(POCFirstName,      string)     { $1 }
poc_last_name       : tagged(POCLastName,       string)     { $1 }
poc_org_name        : tagged(POCOrgName,        string)     { $1 }
poc_phone           : tagged(POCPhone,          string)     { $1 }
poc_email           : tagged(POCEmail,          string)     { $1 }

modification_date   : tagged(ModificationDate,  date)       { $1 }
mom_version         : tagged(MOMVersion,        string)     { $1 }
fed_name            : tagged(FEDname,           string)     { $1 }


data_type
    : tagged(EnumeratedDataType, enumerated_data_type) { EnumeratedDataType $1 }
    | tagged(ComplexDataType,    complex_data_type)    { ComplexDataType    $1 }
    | tagged(RoutingSpace,       routing_space)        { RoutingSpace       $1 }
    | tagged(Class,              class)                { Class              $1 }
    | tagged(Interaction,        interaction)          { Interaction        $1 }
    | tagged(Note,               note)                 { Note               $1 }

enumerated_data_type : unparsed { $1 }
complex_data_type    : unparsed { $1 }
routing_space        : unparsed { $1 }
class                : unparsed { $1 }
interaction          : unparsed { $1 }
note                 : unparsed { $1 }


unparsed : list(sexpr)                       { $1 }

sexpr
    : atom                      { Atom $1 }
    | '(' list(sexpr) ')'       { List $2 }

atom 
    : string                    { $1 }
    | int                       { show $1 }
    | frac                      { show $1 }
    | date                      { dateToString $1 }
    | '[' atom ']'              { Bracketed $2 }

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

data ObjectModel = ObjectModel
    { omdtVersion           :: Version
    , description           :: ObjectModelHeader
    , dataTypes             :: [Component]
    } deriving (Show)

data Date = Date !Integer !Integer !Integer
    deriving Show

dateToString (Date x y z) = intercalate "/" (map show [x,y,z])

data Type
    = FOM
    | SOM
    | OTHER
    deriving (Eq, Ord, Enum, Bounded, Show)

data ObjectModelHeader = ObjectModelHeader
    { name              :: String
    , versionNumber     :: String
    , omType            :: Type
    , purpose           :: String
    , applicationDomain :: String
    , sponsorOrgName    :: String
    , poc               :: POC
    , modificationDate  :: Date
    , momVersion        :: String
    , fedName           :: String
    } deriving Show

data POC = POC
    { honorificName     :: String
    , firstName         :: String
    , lastName          :: String
    , orgName           :: String
    , phone             :: String
    , email             :: String
    } deriving Show

data Component 
    = EnumeratedDataType [SExpr]
    | ComplexDataType    [SExpr]
    | RoutingSpace       [SExpr]
    | Class              [SExpr]
    | Interaction        [SExpr]
    | Note               [SExpr]
    deriving Show

data SExpr 
    = Atom String
    | List [SExpr]
    | Bracketed String
    deriving (Eq, Show)

parseError tokens = error ("Parse error near " ++ show (head tokens))

}