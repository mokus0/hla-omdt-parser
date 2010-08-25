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
    , ComplexDataType(..)
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
    (hdr, _) <- localState (emptyObjectModel version) (many objectModelElement)
    return hdr

objectModelElement = choice 
    [ headerElement
    , enumeratedDataType    <?> "EnumeratedDataType element"
    , complexDataType       <?> "ComplexDataType element"
    , objectClass           <?> "Class element"
    , interactionClass      <?> "Interaction element"
    , note                  <?> "Note element"
    , routingSpace          <?> "RoutingSpace element"
    ]

-- * The header parts

headerElement = focus lHeader $ do
    many1 $ choice 
        [ omName        <?> "Name element"
        , omVersion     <?> "VersionNumber element"
        , omType        <?> "Type element"
        , omPurpose     <?> "Purpose element"
        , appDomain     <?> "ApplicationDomain element"
        , omSponsOrg    <?> "SponsorOrgName element"
        , pocElement
        , momVers       <?> "MOMVersion element"
        , modDate       <?> "ModificationDate element"
        , omFEDname     <?> "FEDname element"
        ]
    return ()

omName          = setP lObjectModelName   . Just =<< tagged "Name"              anyString
omVersion       = setP lVersionNumber     . Just =<< tagged "VersionNumber"     anyString
omType          = setP lOmType            . Just =<< tagged "Type" (choice
    [ string "FOM"   >> return FOM
    , string "SOM"   >> return SOM
    , string "OTHER" >> return OTHER
    ])
omPurpose       = setP lPurpose           . Just =<< tagged "Purpose"           anyString
appDomain       = setP lApplicationDomain . Just =<< tagged "ApplicationDomain" anyString
omSponsOrg      = setP lSponsorOrgName    . Just =<< tagged "SponsorOrgName"    anyString

pocElement = focus lPoc $ do
    -- parse as many as possible in a row to avoid redundant refocusing
    many1 $ choice 
        [ pocHonorificName  <?> "POCHonorificName element"
        , pocFirstName      <?> "POCFirstName element"
        , pocLastName       <?> "POCLastName element"
        , pocOrgName        <?> "POCOrgName element"
        , pocPhone          <?> "POCPhone element"
        , pocEmail          <?> "POCEmail element"
        ]
    return ()

pocHonorificName = setP lHonorificName . Just =<< tagged "POCHonorificName" anyString
pocFirstName     = setP lFirstName     . Just =<< tagged "POCFirstName"     anyString
pocLastName      = setP lLastName      . Just =<< tagged "POCLastName"      anyString
pocOrgName       = setP lOrgName       . Just =<< tagged "POCOrgName"       anyString
pocPhone         = setP lPhone         . Just =<< tagged "POCPhone"         anyString
pocEmail         = setP lEmail         . Just =<< tagged "POCEmail"         anyString

momVers         = setP lMomVersion        . Just =<< tagged "MOMVersion"        anyString
modDate         = setP lModificationDate  . Just =<< tagged "ModificationDate"  date
omFEDname       = setP lFedName           . Just =<< tagged "FEDname"           anyString

-- * Enumerated Data Types

enumeratedDataType = tagged "EnumeratedDataType" $ do
    (edt, FootNoted mbNote name) <- localState emptyEDT $ do
        name <- tagged "Name" (footNoted anyString)
        many enumeratedDataTypeComponent
        return name
    
    modifyP lEnumeratedDataTypes (M.insert name (FootNoted mbNote edt))

enumeratedDataTypeComponent = choice
    [ edtDescription        <?> "Description element"
    , edtAutoSequence       <?> "AutoSequence element"
    , edtStartValue         <?> "StartValue element"
    , edtIsMOMType          <?> "MOMEnumeratedDataType element"
    , edtEnumeration        <?> "Enumeration element"
    , unparsedEDTComponent
    ]

edtDescription  = setP lEdtDescription  . Just =<< tagged "Description"           anyString
edtAutoSequence = setP lEdtAutoSequence . Just =<< tagged "AutoSequence"          boolean
edtStartValue   = setP lEdtStartValue   . Just =<< tagged "StartValue"            int
edtIsMOMType    = setP lEdtIsMOMType    . Just =<< tagged "MOMEnumeratedDataType" boolean
edtEnumeration  = tagged "Enumeration" $ do
    name <- tagged "Enumerator" anyString
    (enum, _) <- localState emptyEnumerator $ do
        many enumeratorComponent
    
    modifyP lEdtEnumeration (M.insert name enum)

enumeratorComponent = choice [enumeratorDescription, enumeratorRepresentation]

enumeratorDescription    = setP lEnumDescription    . Just =<< tagged "Description"    anyString
enumeratorRepresentation = setP lEnumRepresentation . Just =<< tagged "Representation" int

unparsedEDTComponent = do
    openParen
    tag <- anyString
    things <- many sexpr
    closeParen
    
    modifyP lEdtUnparsedComponents (M.insertWith (++) tag [things])

-- * Complex Data Types

complexDataType = tagged "ComplexDataType" $ do
    FootNoted mbNote name <- tagged "Name" (footNoted anyString)
    
    content <- many sexpr
    modifyP lComplexDataTypes (M.insert name (FootNoted mbNote (ComplexDataType Nothing content)))

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
