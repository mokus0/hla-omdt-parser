{-# LANGUAGE NoMonomorphismRestriction, RecordWildCards #-}
module Text.OMDT.Parser where

import Text.OMDT.Parser.Prim

import Prelude hiding (id, (.))
import Control.Category

import Text.OMDT.Syntax
    ( emptyObjectModel, Type(..)
    , SExpr(..), Atom(..)
    , FootNoted(FootNoted)
    , emptyEDT, emptyEnumerator
    , emptyCDT, emptyComplexComponent
    , Accuracy(..), AccuracyCondition(..)
    , Cardinality(..)
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
    -- parse as many as possible in a row to avoid redundant refocusing
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
    , unparsedEDTComponent
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

unparsedEDTComponent = do
    openParen
    tag <- anyString
    things <- many sexpr
    closeParen
    
    modifyP lEdtUnparsedComponents (M.insertWith (++) tag [things])

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
    , cdtUnparsedComponent
    ]

cdtComplexComponent = do
    component <- tagged "ComplexComponent" $ do
        name <- tagged "FieldName" anyString
        (cc, _) <- localState (emptyComplexComponent name) $ do
            many complexComponentComponent
        return cc
    
    modifyP lCdtComponents (component:)

cdtUnparsedComponent = do
    openParen
    tag <- anyString
    things <- many sexpr
    closeParen
    
    modifyP lCdtUnparsedComponents (M.insertWith (++) tag [things])

complexComponentComponent = choice 
    [ element "Description"         lCcDescription          anyString
    , element "DataType"            lCcDataType             anyString
    , element "Accuracy"            lCcAccuracy             accuracy
    , element "AccuracyCondition"   lCcAccuracyCondition    accuracyCondition
    , element "Cardinality"         lCcCardinality          cardinality
    , element "Resolution"          lCcResolution           anyString
    , element "Units"               lCcUnits                anyString
    , ccUnparsedComponent
    ]

accuracy = choice
    [ string "perfect" >> return Perfect]

accuracyCondition = choice
    [ string "always"  >> return Always
    , string "perfect" >> return PerfectCondition -- ???
    ]

cardinality = fmap Cardinality anyString

ccUnparsedComponent = do
    openParen
    tag <- anyString
    things <- many sexpr
    closeParen
    
    modifyP lCcUnparsedComponents (M.insertWith (++) tag [things])

-- * Classes

objectClass = tagged "Class" (many sexpr) >> return ()

-- * Interactions

interactionClass = tagged "Interaction" (many sexpr) >> return ()

-- * Notes

note = tagged "Note" (many sexpr) >> return ()

-- * Routing Spaces

routingSpace = tagged "RoutingSpace" (many sexpr) >> return ()

-- * Unparsed S-Expression

sexpr = list <|> atom

list = fmap List $ parens (many sexpr)
atom = fmap Atom $ choice
    [ fmap S anyString, fmap V version, fmap D date, fmap I int, fmap F frac, fmap N footnote]
