module Main where

import Text.OMDT.Lexer
import Text.OMDT.ObjectParser
import Text.OMDT.ObjectParser.Stage2

import qualified Data.Map as M

main = do
    src <- readFile "fom_v4.2_17NOV2008.omd"
    let tokens = alexScanTokens src
        obj = omdObject tokens
        omd = omdtObjectToObjectModel obj

    mapM_ print (namedElems (enumeratedDataTypes (objectModel omd)))
