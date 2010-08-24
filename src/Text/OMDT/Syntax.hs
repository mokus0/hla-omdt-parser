{-# LANGUAGE TemplateHaskell #-}
module Text.OMDT.Syntax where

import Data.Record.Label
import qualified Data.IntMap as I
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Time (Day)
import Data.Version (Version)

data ObjectModel a b = ObjectModel
    { _omdtVersion           :: Version
    , _header                :: ObjectModelHeader
    , _classes               :: a
    , _complexDataTypes      :: a
    , _enumeratedDataTypes   :: a
    , _interactions          :: a
    , _notes                 :: I.IntMap String
    , _routingSpaces         :: b
    } deriving (Eq, Show)

data Type
    = FOM
    | SOM
    | OTHER
    deriving (Eq, Ord, Enum, Bounded, Show)

data ObjectModelHeader = ObjectModelHeader
    { _objectModelName   :: Maybe String
    , _versionNumber     :: Maybe String
    , _omType            :: Maybe Type
    , _purpose           :: Maybe String
    , _applicationDomain :: Maybe String
    , _sponsorOrgName    :: Maybe String
    , _poc               :: POC
    , _modificationDate  :: Maybe Day
    , _momVersion        :: Maybe String
    , _fedName           :: Maybe String
    } deriving (Eq, Show)

emptyHeader = ObjectModelHeader
    { _objectModelName   = Nothing
    , _versionNumber     = Nothing
    , _omType            = Nothing
    , _purpose           = Nothing
    , _applicationDomain = Nothing
    , _sponsorOrgName    = Nothing
    , _poc               = emptyPOC
    , _modificationDate  = Nothing
    , _momVersion        = Nothing
    , _fedName           = Nothing
    }

data POC = POC
    { _honorificName     :: Maybe String
    , _firstName         :: Maybe String
    , _lastName          :: Maybe String
    , _orgName           :: Maybe String
    , _phone             :: Maybe String
    , _email             :: Maybe String
    } deriving (Eq, Show)

emptyPOC = POC
    { _honorificName     = Nothing
    , _firstName         = Nothing
    , _lastName          = Nothing
    , _orgName           = Nothing
    , _phone             = Nothing
    , _email             = Nothing
    }


$(mkLabels [''ObjectModel, ''ObjectModelHeader, ''POC])