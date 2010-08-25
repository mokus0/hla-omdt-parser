module Main where

import Text.OMDT.Lexer (alexScanTokens)
import Text.OMDT.Parser (omdt)
import Text.OMDT.Syntax
import Text.Parsec

import qualified Data.Map as M
import qualified Data.IntMap as I

srcName = "fom_v4.2_17NOV2008.omd"

main = do
    src <- readFile srcName
    let tokens = alexScanTokens src
    
    case runParser omdt () srcName tokens of
        Left err -> do
            -- mapM_ print tokens
            print err
        Right objectModel -> do
            sequence_
                [ do
                    putStr (show num ++ ": ")
                    print thing
                    putStrLn ""
                | (num, thing) <- I.assocs (classes objectModel)
                ]
