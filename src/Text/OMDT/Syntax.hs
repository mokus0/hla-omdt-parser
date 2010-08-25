{-# LANGUAGE TemplateHaskell #-}
module Text.OMDT.Syntax where

import Data.Record.Label
import qualified Data.IntMap as I
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Time (Day)
import Data.Version (Version)

data SExpr
    = List [SExpr]
    | Atom Atom
    deriving (Eq, Show)

data Atom
    = S String
    | V Version
    | D Day
    | I Integer
    | F Rational
    | N Integer
    deriving (Eq, Show)


data ObjectModel = ObjectModel
    { omdtVersion           :: Version
    , header                :: ObjectModelHeader
    , classes               :: I.IntMap     ()
    , complexDataTypes      :: M.Map String (FootNoted ComplexDataType)
    , enumeratedDataTypes   :: M.Map String (FootNoted EnumeratedDataType)
    , interactions          :: I.IntMap     ()
    , notes                 :: I.IntMap     String
    , routingSpaces         :: M.Map String ()
    } deriving (Eq, Show)

emptyObjectModel v = ObjectModel
    { omdtVersion           = v
    , header                = emptyHeader
    , classes               = I.empty
    , complexDataTypes      = M.empty
    , enumeratedDataTypes   = M.empty
    , interactions          = I.empty
    , notes                 = I.empty
    , routingSpaces         = M.empty
    }

data Type
    = FOM
    | SOM
    | OTHER
    deriving (Eq, Ord, Enum, Bounded, Show)

data ObjectModelHeader = ObjectModelHeader
    { objectModelName   :: Maybe String
    , versionNumber     :: Maybe String
    , omType            :: Maybe Type
    , purpose           :: Maybe String
    , applicationDomain :: Maybe String
    , sponsorOrgName    :: Maybe String
    , poc               :: POC
    , modificationDate  :: Maybe Day
    , momVersion        :: Maybe String
    , fedName           :: Maybe String
    } deriving (Eq, Show)

emptyHeader = ObjectModelHeader
    { objectModelName   = Nothing
    , versionNumber     = Nothing
    , omType            = Nothing
    , purpose           = Nothing
    , applicationDomain = Nothing
    , sponsorOrgName    = Nothing
    , poc               = emptyPOC
    , modificationDate  = Nothing
    , momVersion        = Nothing
    , fedName           = Nothing
    }

data POC = POC
    { honorificName     :: Maybe String
    , firstName         :: Maybe String
    , lastName          :: Maybe String
    , orgName           :: Maybe String
    , phone             :: Maybe String
    , email             :: Maybe String
    } deriving (Eq, Show)

emptyPOC = POC
    { honorificName     = Nothing
    , firstName         = Nothing
    , lastName          = Nothing
    , orgName           = Nothing
    , phone             = Nothing
    , email             = Nothing
    }

data FootNoted a = FootNoted
    { footNote  :: Maybe Int
    , footNoted :: a
    } deriving (Eq, Show)

data EnumeratedDataType = EnumeratedDataType
    { edtDescription        :: Maybe String
    , edtAutoSequence       :: Maybe Bool
    , edtStartValue         :: Maybe Integer
    , edtIsMOMType          :: Maybe Bool
    , edtEnumeration        :: M.Map String Enumerator
    , edtUnparsedComponents :: M.Map String [[SExpr]]
    } deriving (Eq, Show)

emptyEDT = EnumeratedDataType
    { edtDescription        = Nothing
    , edtAutoSequence       = Nothing
    , edtStartValue         = Nothing
    , edtIsMOMType          = Nothing
    , edtEnumeration        = M.empty
    , edtUnparsedComponents = M.empty
    }

data Enumerator = Enumerator
    { enumRepresentation    :: Maybe Int
    , enumDescription       :: Maybe String
    } deriving (Eq, Show)

emptyEnumerator = Enumerator
    { enumRepresentation    = Nothing
    , enumDescription       = Nothing
    }

data ComplexDataType = ComplexDataType
    { cdtDescription        :: Maybe String
    , cdtUnparsedComponents :: [SExpr]
    } deriving (Eq, Show)

emptyCDT = ComplexDataType
    { cdtDescription        = Nothing
    , cdtUnparsedComponents = []
    }

