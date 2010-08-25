module Main where

import Text.OMDT.Lexer (alexScanTokens)
import Text.OMDT.Parser (omdt)
import Text.OMDT.Syntax
import Text.Parsec

import qualified Data.Map as M

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
                    putStr name
                    maybe (putStrLn "") (\note -> putStrLn $ concat [" [" ++ show note ++ "]"]) (footNote edt)
                    print (footNoted edt)
                    putStrLn ""
                | (name, edt) <- M.assocs (enumeratedDataTypes objectModel)
                ]
