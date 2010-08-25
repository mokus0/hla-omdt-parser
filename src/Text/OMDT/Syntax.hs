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
    , classes               :: I.IntMap     Class
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
    , cdtIsMOMType          :: Maybe Bool
    , cdtComponents         :: [ComplexComponent]
    , cdtUnparsedComponents :: M.Map String [[SExpr]]
    } deriving (Eq, Show)

emptyCDT = ComplexDataType
    { cdtDescription        = Nothing
    , cdtIsMOMType          = Nothing
    , cdtComponents         = []
    , cdtUnparsedComponents = M.empty
    }

data ComplexComponent = ComplexComponent
    { ccName                :: String
    , ccDescription         :: Maybe String
    , ccDataType            :: Maybe String
    , ccAccuracy            :: Maybe Accuracy
    , ccAccuracyCondition   :: Maybe AccuracyCondition
    , ccResolution          :: Maybe String
    , ccUnits               :: Maybe String
    , ccCardinality         :: Maybe Cardinality
    , ccUnparsedComponents  :: M.Map String [[SExpr]]
    } deriving (Eq, Show)

emptyComplexComponent name = ComplexComponent
    { ccName                = name
    , ccDescription         = Nothing
    , ccDataType            = Nothing
    , ccAccuracy            = Nothing
    , ccAccuracyCondition   = Nothing
    , ccResolution          = Nothing
    , ccUnits               = Nothing
    , ccCardinality         = Nothing
    , ccUnparsedComponents  = M.empty
    }

data Accuracy
    = Perfect
    deriving (Eq, Ord, Enum, Bounded, Read, Show)

data AccuracyCondition
    = Always
    | PerfectCondition
    deriving (Eq, Ord, Enum, Bounded, Read, Show)

newtype Cardinality = Cardinality String
    deriving (Eq, Show)

data Class = Class
    { className                 :: Maybe String
    , classSuperClassID         :: Maybe Int
    , classDescription          :: Maybe String
    , classPSCapabilities       :: Maybe PSCapabilities
    , classIsMOMType            :: Maybe Bool
    , classAttributes           :: [Attribute]
    , classUnparsedComponents   :: M.Map String [[SExpr]]
    } deriving (Eq, Show)

emptyClass = Class
    { className                 = Nothing
    , classSuperClassID         = Nothing
    , classDescription          = Nothing
    , classPSCapabilities       = Nothing
    , classIsMOMType            = Nothing
    , classAttributes           = []
    , classUnparsedComponents   = M.empty
    }

data PSCapabilities = PSCapabilities
    { pubCapability             :: Bool
    , subCapability             :: Bool
    } deriving (Eq, Show)

data Attribute = Attribute
    { attributeName                 :: Maybe String
    , attributeDescription          :: Maybe String
    , attributeDataType             :: Maybe String
    , attributeAccuracy             :: Maybe Accuracy
    , attributeAccuracyCondition    :: Maybe AccuracyCondition
    , attributeCardinality          :: Maybe Cardinality
    , attributeResolution           :: Maybe String
    , attributeUnits                :: Maybe String
    , attributeDelivery             :: Maybe Delivery
    , attributeOrdering             :: Maybe MsgOrdering
    , attributeTransferAccept       :: Maybe TransferAccept
    , attributeRoutingSpace         :: Maybe String
    , attributeUpdateReflect        :: Maybe UpdateReflect
    , attributeUpdateType           :: Maybe UpdateType
    , attributeUpdateCondition      :: Maybe String
    , attributeUnparsedComponents   :: M.Map String [[SExpr]]
    } deriving (Eq, Show)

emptyAttribute = Attribute
    { attributeName                 = Nothing
    , attributeDescription          = Nothing
    , attributeDataType             = Nothing
    , attributeAccuracy             = Nothing
    , attributeAccuracyCondition    = Nothing
    , attributeCardinality          = Nothing
    , attributeResolution           = Nothing
    , attributeUnits                = Nothing
    , attributeDelivery             = Nothing
    , attributeOrdering             = Nothing
    , attributeTransferAccept       = Nothing
    , attributeUpdateType           = Nothing
    , attributeUpdateCondition      = Nothing
    , attributeRoutingSpace         = Nothing
    , attributeUpdateReflect        = Nothing
    , attributeUnparsedComponents   = M.empty
    }

data Delivery
    = Reliable
    | BestEffort
    deriving (Eq, Ord, Enum, Bounded, Read, Show)

data MsgOrdering
    = Receive
    deriving (Eq, Ord, Enum, Bounded, Read, Show)

data TransferAccept = TransferAccept
    { taTransfer        :: Bool
    , taAccept          :: Bool
    } deriving (Eq, Show)

data UpdateReflect = UpdateReflect
    { urUpdate          :: Bool
    , urReflect         :: Bool
    } deriving (Eq, Show)

data UpdateType
    = Static
    | Conditional
    | Periodic
    deriving (Eq, Ord, Enum, Bounded, Read, Show)