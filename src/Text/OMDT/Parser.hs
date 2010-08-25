{-# LANGUAGE NoMonomorphismRestriction, RecordWildCards #-}
-- |This is an incomplete parser derived from a few specific instances of the
-- data files.  I don't have access to the spec for the format, assuming it
-- even exists.
module Text.OMDT.Parser where

import Text.OMDT.Parser.Prim

import Prelude hiding (id, (.))
import Control.Category

import Text.OMDT.Syntax
    ( emptyObjectModel, Type(..)
    , FootNoted(FootNoted)
    , emptyEDT, emptyEnumerator
    , emptyCDT, emptyComplexComponent
    , Accuracy(..), AccuracyCondition(..)
    , Cardinality(..)
    , emptyClass, emptyAttribute
    , PSCapabilities(..), Delivery(..), MsgOrdering(..)
    , TransferAccept(..), UpdateReflect(..), UpdateType(..)
    )
import Text.OMDT.Syntax.Labels

import Text.Parsec hiding (string)
import Text.Parsec.Prim (ParsecT(..))

import Data.Record.Label
import qualified Data.IntMap as I
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Time (Day)
import Data.Version (Version)

omdt = do
    version <- tagged "OMDT" version <?> "OMDT element"
    objectModel version <?> "ObjectModel element"

objectModel version = tagged "ObjectModel" $ do
    (hdr, _) <- localState (emptyObjectModel version) (many objectModelElements)
    return hdr

objectModelElements = choice 
    [ headerElements
    , enumeratedDataType    <?> "EnumeratedDataType element"
    , complexDataType       <?> "ComplexDataType element"
    , objectClass           <?> "Class element"
    , interactionClass      <?> "Interaction element"
    , note                  <?> "Note element"
    , routingSpace          <?> "RoutingSpace element"
    ]

-- * The header parts

headerElements = focus lHeader $ do
    many1 $ choice 
        [ element "Name"                lObjectModelName    anyString
        , element "VersionNumber"       lVersionNumber      anyString
        , element "Type"                lOmType             omType
        , element "Purpose"             lPurpose            anyString
        , element "ApplicationDomain"   lApplicationDomain  anyString
        , element "SponsorOrgName"      lSponsorOrgName     anyString
        , element "MOMVersion"          lMomVersion         anyString
        , element "ModificationDate"    lModificationDate   date
        , element "FEDname"             lFedName            anyString
        , pocElements
        ]
    return ()

omType = choice
    [ string "FOM"   >> return FOM
    , string "SOM"   >> return SOM
    , string "OTHER" >> return OTHER
    ]

pocElements = focus lPoc $ do
    many1 $ choice 
        [ element "POCHonorificName" lHonorificName anyString
        , element "POCFirstName"     lFirstName     anyString
        , element "POCLastName"      lLastName      anyString
        , element "POCOrgName"       lOrgName       anyString
        , element "POCPhone"         lPhone         anyString
        , element "POCEmail"         lEmail         anyString
        ]
    return ()

-- * Enumerated Data Types

enumeratedDataType = tagged "EnumeratedDataType" $ do
    (edt, FootNoted mbNote name) <- localState emptyEDT $ do
        name <- tagged "Name" (footNoted anyString)
        many enumeratedDataTypeComponent
        return name
    
    modifyP lEnumeratedDataTypes (M.insert name (FootNoted mbNote edt))

enumeratedDataTypeComponent = choice
    [ element "Description"           lEdtDescription   anyString
    , element "AutoSequence"          lEdtAutoSequence  boolean
    , element "StartValue"            lEdtStartValue    int
    , element "MOMEnumeratedDataType" lEdtIsMOMType     boolean
    , edtEnumeration <?> "Enumeration element"
    , unparsed lEdtUnparsedComponents
    ]

edtEnumeration  = tagged "Enumeration" $ do
    name <- tagged "Enumerator" anyString
    (enum, _) <- localState emptyEnumerator $ do
        many enumeratorComponent
    
    modifyP lEdtEnumeration (M.insert name enum)

enumeratorComponent = choice 
    [ element "Description"     lEnumDescription    anyString
    , element "Representation"  lEnumRepresentation int
    ]

-- * Complex Data Types

complexDataType = tagged "ComplexDataType" $ do
    (cdt, FootNoted mbNote name) <- localState emptyCDT $ do
        name <- tagged "Name" (footNoted anyString)
        many complexDataTypeComponent
        modifyP lCdtComponents reverse
        return name
    
    modifyP lComplexDataTypes (M.insert name (FootNoted mbNote cdt))

complexDataTypeComponent = choice
    [ element "Description"         lCdtDescription anyString
    , element "MOMComplexDataType"  lCdtIsMOMType   boolean
    , cdtComplexComponent <?> "ComplexComponent element"
    , unparsed lCdtUnparsedComponents
    ]

cdtComplexComponent = do
    component <- tagged "ComplexComponent" $ do
        name <- tagged "FieldName" anyString
        (cc, _) <- localState (emptyComplexComponent name) $ do
            many complexComponentComponent
        return cc
    
    modifyP lCdtComponents (component:)

complexComponentComponent = choice 
    [ element "Description"         lCcDescription          anyString
    , element "DataType"            lCcDataType             anyString
    , element "Accuracy"            lCcAccuracy             accuracy
    , element "AccuracyCondition"   lCcAccuracyCondition    accuracyCondition
    , element "Cardinality"         lCcCardinality          cardinality
    , element "Resolution"          lCcResolution           anyString
    , element "Units"               lCcUnits                anyString
    , unparsed lCcUnparsedComponents
    ]

accuracy = choice
    [ string "perfect" >> return Perfect]

accuracyCondition = choice
    [ string "always"  >> return Always
    , string "perfect" >> return PerfectCondition -- ???
    ]

cardinality = fmap Cardinality anyString

-- * Classes

objectClass = tagged "Class" $ do
    classId <- tagged "ID" int
    (cls, _) <- localState emptyClass $ do
        many objectClassComponents
        modifyP lClassAttributes reverse
    
    modifyP lClasses (I.insert classId cls)

objectClassComponents = choice
    [ element "Name"            lClassName           anyString
    , element "SuperClass"      lClassSuperClassID   int
    , element "Description"     lClassDescription    anyString
    , element "PSCapabilities"  lClassPSCapabilities psCapabilities
    , element "MOMClass"        lClassIsMOMType      boolean
    , classAttribute <?> "Attribute element"
    , unparsed lClassUnparsedComponents
    ]

psCapabilities = choice
    [ string "PS" >> return (PSCapabilities True  True )
    , string "P"  >> return (PSCapabilities True  False)
    , string "S"  >> return (PSCapabilities False True )
    , string "N"  >> return (PSCapabilities False False)
    ]

classAttribute = tagged "Attribute" $ do
    (attribute, _) <- localState emptyAttribute $ do
        many attributeComponents
    
    modifyP lClassAttributes (attribute:)

attributeComponents = choice
    [ element "Name"                lAttributeName              anyString
    , element "Description"         lAttributeDescription       anyString
    , element "DataType"            lAttributeDataType          anyString
    , element "Accuracy"            lAttributeAccuracy          accuracy
    , element "AccuracyCondition"   lAttributeAccuracyCondition accuracyCondition
    , element "Cardinality"         lAttributeCardinality       cardinality
    , element "Resolution"          lAttributeResolution        anyString
    , element "Units"               lAttributeUnits             anyString
    , element "DeliveryCategory"    lAttributeDelivery          delivery
    , element "MessageOrdering"     lAttributeOrdering          ordering
    , element "TransferAccept"      lAttributeTransferAccept    transferAccept
    , element "RoutingSpace"        lAttributeRoutingSpace      anyString
    , element "UpdateReflect"       lAttributeUpdateReflect     updateReflect
    , element "UpdateCondition"     lAttributeUpdateCondition   anyString
    , element "UpdateType"          lAttributeUpdateType        updateType
    , unparsed lAttributeUnparsedComponents
    ]

delivery = choice
    [ string "reliable"     >> return Reliable
    , string "best_effort"  >> return BestEffort
    ]

ordering = choice
    [ string "receive"      >> return Receive
    ]

transferAccept = choice
    [ string "TA"   >> return (TransferAccept True  True )
    , string "T"    >> return (TransferAccept True  False)
    , string "A"    >> return (TransferAccept False True )
    , string "N"    >> return (TransferAccept False False)
    ]

updateReflect = choice
    [ string "UR"   >> return (UpdateReflect True  True )
    , string "U"    >> return (UpdateReflect True  False)
    , string "R"    >> return (UpdateReflect False True )
    , string "N"    >> return (UpdateReflect False False)
    ]

updateType = choice
    [ string "Static"       >> return Static
    , string "Conditional"  >> return Conditional
    , string "Periodic"     >> return Periodic
    ]

-- * Interactions

interactionClass = tagged "Interaction" (many sexpr) >> return ()

-- * Notes

note = tagged "Note" $ do
    num  <- tagged "NoteNumber" int
    note <- tagged "NoteText"   anyString
    
    modifyP lNotes (I.insert num note)

-- * Routing Spaces

routingSpace = tagged "RoutingSpace" (many sexpr) >> return ()
