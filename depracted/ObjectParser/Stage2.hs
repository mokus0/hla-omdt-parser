module Text.OMDT.ObjectParser.Stage2
    ( omdtObjectToObjectModel
    , OMDT(..), ObjectModel(..)
    , ObjectModelHeader(..)
    
    , Named(..), lookupNamed, namedElems
    ) where

import Text.OMDT.ObjectParser
import qualified Data.IntMap as I
import Data.List
import qualified Data.Map as M
import Data.Object
import qualified Data.Set as S
import Data.Time
import Data.Version (Version)

toMultiMap :: Ord k => [(k,v)] -> M.Map k [v]
toMultiMap = M.fromListWith (++) . map (fmap (:[]))

lookupNamed name = fmap (Named name) . M.lookup name

namedElems namedMap = map (uncurry Named) (M.toList namedMap)

toNamedMap readThing entries = M.fromList
    [ (name, thing)
    | Mapping entry <- entries
    , let Named name thing = readNamed readThing entry
    ]

getString (String s) = Just s
getString _ = Nothing

getDate (Date y m d) = Just (fromGregorian y (fromInteger m) (fromInteger d))
getDate _ = Nothing

data Named thing = Named
    { name      :: String
    , thing     :: thing
    } deriving (Eq, Show)

readNamed readThing raw = case partition isName raw of
    ([(_, Scalar (String name))], rest) -> Named name (readThing rest)
    ([(_, Sequence [Scalar (String name), Scalar (Footnote _)])], rest) -> Named name (readThing rest)
    where isName ("Name", _) = True; isName _ = False

data OMDT a b = OMDT
    { version       :: Version
    , objectModel   :: ObjectModel a b
    } deriving (Eq, Show)

data ObjectModel a b = ObjectModel
    { header                :: ObjectModelHeader
    , classes               :: a
    , complexDataTypes      :: a
    , enumeratedDataTypes   :: a
    , interactions          :: a
    , notes                 :: I.IntMap String
    , routingSpaces         :: b
    } deriving (Eq, Show)

omdtObjectToObjectModel (Mapping 
    [ ("OMDT", Scalar (Version version))
    , ("ObjectModel", objectModel)
    ]) = OMDT version (readObjectModel objectModel)

readObjectModel (Mapping entries)
    | S.null unknownKeys    = ObjectModel
        { header                = readHeader headerThings
        , classes               = maybe M.empty (toNamedMap id) (M.lookup "Class"              components)
        , complexDataTypes      = maybe M.empty (toNamedMap id) (M.lookup "ComplexDataType"    components)
        , enumeratedDataTypes   = maybe M.empty (toNamedMap id) (M.lookup "EnumeratedDataType" components)
        , interactions          = maybe M.empty (toNamedMap id) (M.lookup "Interaction"        components)
        , notes                 = readNotes (M.lookup "Note" components)
        , routingSpaces         = readRoutingSpaces (M.lookup "RoutingSpace"       components)
        }
    | otherwise             = error ("unknown keys in ObjectModel: " ++ show (S.toList unknownKeys))
    where
        unknownKeys = S.difference (M.keysSet entriesByType) (S.union headerKeys componentKeys)
        
        (headerThings, components) = M.partitionWithKey (\k v -> S.member k headerKeys) entriesByType
        entriesByType = toMultiMap entries

data Type
    = FOM
    | SOM
    | OTHER
    deriving (Eq, Ord, Enum, Bounded, Show)

getType (String string) = case string of
    "FOM"   -> Just FOM
    "SOM"   -> Just SOM
    "OTHER" -> Just OTHER
    unknown -> Just (error ("Unknown object model type: " ++ show unknown))
getType _ = Nothing
    

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

data POC = POC
    { honorificName     :: Maybe String
    , firstName         :: Maybe String
    , lastName          :: Maybe String
    , orgName           :: Maybe String
    , phone             :: Maybe String
    , email             :: Maybe String
    } deriving (Eq, Show)

readHeader entries = ObjectModelHeader
    { objectModelName   = getString =<< getScalar "Name"
    , versionNumber     = getString =<< getScalar "VersionNumber"
    , omType            = getType   =<< getScalar "Type"
    , purpose           = getString =<< getScalar "Purpose"
    , applicationDomain = getString =<< getScalar "ApplicationDomain"
    , sponsorOrgName    = getString =<< getScalar "SponsorOrgName"
    , poc               = readPOC entries
    , modificationDate  = getDate   =<< getScalar "ModificationDate"
    , momVersion        = getString =<< getScalar "MOMVersion"
    , fedName           = getString =<< getScalar "FEDname"
    } where
        getScalar key = do
            [Scalar s] <- M.lookup key entries
            return s

readPOC entries = POC
    { honorificName     = getString =<< getScalar "POCHonorificName"
    , firstName         = getString =<< getScalar "POCFirstName"
    , lastName          = getString =<< getScalar "POCLastName"
    , orgName           = getString =<< getScalar "POCOrgName"
    , phone             = getString =<< getScalar "POCPhone"
    , email             = getString =<< getScalar "POCEmail"
    } where
        getScalar key = do
            [Scalar s] <- M.lookup key entries
            return s

readNotes Nothing = I.empty
readNotes (Just notes) = I.fromList
    [ (fromInteger num, note)
    | Mapping [ ("NoteNumber", Scalar (Int num))
              , ("NoteText", Scalar (String note))
              ] <- notes
    ]

data Class = Class
    { classID           :: Int
    , className         :: String
    , psCapabilities    :: PSCapabilities
    , classDescription  :: Maybe String
    , superClass        :: Maybe Int
    , attributes        :: M.Map String Attribute
    , momClass          :: Bool
    } deriving (Eq, Show)

data Attribute a = Attribute
    { unparsedAttributeBits :: a
    } deriving (Eq, Show)

data RoutingSpace a = RoutingSpace
    { spaceDescription  :: Maybe String
    , dimensions        :: M.Map String (Dimension a)
    } deriving (Eq, Show)

readRoutingSpaces Nothing = M.empty
readRoutingSpaces (Just spaces) = M.fromList
    [ (name, space)
    | Mapping raw <- spaces
    , let Named name space = readNamed readRoutingSpace raw
    ]
    
readRoutingSpace space
    | null unknownKeys      = RoutingSpace
        { spaceDescription  = getString =<< getScalar "Description"
        , dimensions        = readDimensions dimensions
        }
    | otherwise             = error ("unknown key in RoutingSpace: " ++ show unknownKeys)
    where
        unknownKeys = nub (map fst space) \\
            [ "Description"
            , "Dimension"
            ]
        dimensions = [desc | ("Dimension", desc) <- space]
        
        getScalar key = do
            Scalar s <- lookup key space
            return s

data Dimension a = Dimension a
    deriving (Eq, Show)

readDimensions dims = M.fromList
    [ (name, dimension)
    | Mapping dim <- dims
    , let Named name dimension = readNamed readDimension dim
    ]

readDimension dimension
    | null unknownKeys  = Dimension dimension
    | otherwise = error ("unknown keys in Dimension: " ++ show unknownKeys)
    where
        unknownKeys = nub (map fst dimension) \\
            [ "DimensionSet"
            , "DimensionType"
            , "DimensionMinimum"
            , "DimensionMaximum"
            , "IntervalType"
            , "RangeSetUnits"
            , "NormalizationFunction"
            ]

headerKeys = S.fromList
    [ "ApplicationDomain"
    , "FEDname"
    , "MOMVersion"
    , "ModificationDate"
    , "Name"
    , "POCEmail"
    , "POCFirstName"
    , "POCHonorificName"
    , "POCLastName"
    , "POCOrgName"
    , "POCPhone"
    , "Purpose"
    , "SponsorOrgName"
    , "Type"
    , "VersionNumber"
    ]
componentKeys = S.fromList
    [ "Class"
    , "ComplexDataType"
    , "EnumeratedDataType"
    , "Interaction"
    , "Note"
    , "RoutingSpace"
    ]
