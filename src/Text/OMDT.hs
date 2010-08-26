module Text.OMDT
    ( ObjectModel(..)
    , ObjectModelHeader(..)
    , Type(..)
    , POC(..)
    
    , Class(..)
    , PSCapabilities(..)
    , Attribute(..)
    , TransferAccept(..)
    , UpdateReflect(..)
    , UpdateType(..)
    
    , ComplexDataType(..)
    , ComplexComponent(..)
    
    , EnumeratedDataType(..)
    , Enumerator(..)
    
    , Interaction(..)
    , ISRType(..)
    , Parameter(..)
    
    , RoutingSpace(..)
    , Dimension(..)
    , IntervalType(..)
    , NormalizationFunction(..)
    
    , Accuracy(..)
    , AccuracyCondition(..)
    , Cardinality(..)
    , Delivery(..)
    , MsgOrdering(..)
    
    , FootNoted(..)
    , SExpr(..)
    , Atom(..)
    
    , readOMD, readOMDFromFile
    ) where

import Text.OMDT.Lexer (alexScanTokens)
import Text.OMDT.Parser (omdt)
import Text.OMDT.Syntax

import Text.Parsec

readOMD :: String -> Either ParseError ObjectModel
readOMD src = runParser omdt () srcName tokens
    where 
        srcName = "readOMD"
        tokens = alexScanTokens src

readOMDFromFile :: FilePath -> IO (Either ParseError ObjectModel)
readOMDFromFile filePath = do
    src <- readFile filePath
    let tokens = alexScanTokens src
    return (runParser omdt () filePath tokens)
    