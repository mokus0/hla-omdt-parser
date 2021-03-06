{-# LANGUAGE TemplateHaskell #-}
module Text.OMDT.Syntax where

import Data.Record.Label
import qualified Data.IntMap as I
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Time (Day)
import Data.Version (Version)
import Language.Haskell.TH.Lift

data SExpr
    = List [SExpr]
    | Atom Atom
    deriving (Eq, Show)

data Atom
    = S_ String
    | V_ Version
    | D_ Day
    | I_ Integer
    | F_ Rational
    | N_ Integer
    deriving (Eq, Show)


data ObjectModel = ObjectModel
    { omdtVersion           :: Version
    , header                :: ObjectModelHeader
    , classes               :: I.IntMap     Class
    , complexDataTypes      :: M.Map String (FootNoted ComplexDataType)
    , enumeratedDataTypes   :: M.Map String (FootNoted EnumeratedDataType)
    , interactions          :: I.IntMap     Interaction
    , notes                 :: I.IntMap     String
    , routingSpaces         :: M.Map String RoutingSpace
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
    , ccDataType            :: Maybe (FootNoted String)
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

data Cardinality = Cardinality
    { minCount  :: Int
    , maxCount  :: Maybe Int
    } deriving (Eq, Show)

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
    , attributeDataType             :: Maybe (FootNoted String)
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

data UpdateReflect
    = Update
    | Reflect
    | UpdateReflect
    deriving (Eq, Ord, Enum, Bounded, Read, Show)

data UpdateType
    = Static
    | Conditional
    | Periodic
    deriving (Eq, Ord, Enum, Bounded, Read, Show)

data Interaction = Interaction
    { interactionName               :: Maybe String
    , interactionSuperInteractionID :: Maybe Int
    , interactionIsMOMType          :: Maybe Bool
    , interactionDescription        :: Maybe String
    , interactionDelivery           :: Maybe Delivery
    , interactionOrdering           :: Maybe MsgOrdering
    , interactionISRType            :: Maybe ISRType
    , interactionRoutingSpace       :: Maybe String
    , interactionParameters         :: [Parameter]
    , interactionUnparsedComponents :: M.Map String [[SExpr]]
    } deriving (Eq, Show)

emptyInteraction = Interaction
    { interactionName               = Nothing
    , interactionSuperInteractionID = Nothing
    , interactionIsMOMType          = Nothing
    , interactionDescription        = Nothing
    , interactionDelivery           = Nothing
    , interactionOrdering           = Nothing
    , interactionISRType            = Nothing
    , interactionRoutingSpace       = Nothing
    , interactionParameters         = []
    , interactionUnparsedComponents = M.empty
    }

data ISRType
    = I
    | S
    | R
    | IS
    | IR
    | N
    deriving (Eq, Ord, Enum, Bounded, Read, Show)

data Parameter = Parameter
    { parameterName                 :: Maybe String
    , parameterDescription          :: Maybe String
    , parameterDataType             :: Maybe (FootNoted String)
    , parameterAccuracy             :: Maybe Accuracy
    , parameterAccuracyCondition    :: Maybe AccuracyCondition
    , parameterResolution           :: Maybe String
    , parameterUnits                :: Maybe String
    , parameterCardinality          :: Maybe Cardinality
    , parameterUnparsedComponents   :: M.Map String [[SExpr]]
    } deriving (Eq, Show)

emptyParameter = Parameter
    { parameterName                 = Nothing
    , parameterDescription          = Nothing
    , parameterDataType             = Nothing
    , parameterAccuracy             = Nothing
    , parameterAccuracyCondition    = Nothing
    , parameterResolution           = Nothing
    , parameterUnits                = Nothing
    , parameterCardinality          = Nothing
    , parameterUnparsedComponents   = M.empty
    }

data RoutingSpace = RoutingSpace
    { rSpaceDescription             :: Maybe String
    , rSpaceDimensions              :: [Dimension]
    , rSpaceUnparsedComponents      :: M.Map String [[SExpr]]
    } deriving (Eq, Show)

emptyRoutingSpace = RoutingSpace
    { rSpaceDescription             = Nothing
    , rSpaceDimensions              = []
    , rSpaceUnparsedComponents      = M.empty
    }

data Dimension = Dimension
    { dimensionName                 :: Maybe String
    , dimensionType                 :: Maybe (FootNoted String)
    , dimensionIntervalType         :: Maybe IntervalType
    , dimensionMaximum              :: Maybe Integer
    , dimensionMinimum              :: Maybe Integer
    , dimensionNormalization        :: Maybe NormalizationFunction
    , dimensionRangeSetUnits        :: Maybe String
    , dimensionSet                  :: Maybe [String]
    , dimensionUnparsedComponents   :: M.Map String [[SExpr]]
    } deriving (Eq, Show)

emptyDimension = Dimension
    { dimensionName                 = Nothing
    , dimensionType                 = Nothing
    , dimensionIntervalType         = Nothing
    , dimensionMaximum              = Nothing
    , dimensionMinimum              = Nothing
    , dimensionNormalization        = Nothing
    , dimensionRangeSetUnits        = Nothing
    , dimensionSet                  = Nothing
    , dimensionUnparsedComponents   = M.empty
    }

data IntervalType
    = Closed
    deriving (Eq, Ord, Enum, Bounded, Read, Show)

data NormalizationFunction
    = Linear
    deriving (Eq, Ord, Enum, Bounded, Read, Show)

$( fmap concat $ mapM deriveLift
    [ ''SExpr, ''Atom
    , ''ObjectModel, ''Type, ''ObjectModelHeader, ''POC
    , ''FootNoted
    
    , ''EnumeratedDataType, ''Enumerator
    
    , ''ComplexDataType, ''ComplexComponent
    , ''Accuracy, ''AccuracyCondition
    , ''Cardinality
    
    , ''Class, ''Attribute
    , ''PSCapabilities, ''Delivery, ''MsgOrdering
    , ''TransferAccept, ''UpdateReflect, ''UpdateType
    
    , ''Interaction, ''Parameter
    , ''ISRType
    
    , ''RoutingSpace, ''Dimension
    , ''IntervalType, ''NormalizationFunction
    
    -- Orphans!
    , ''Day, ''Version
    , ''M.Map, ''I.IntMap
    ])