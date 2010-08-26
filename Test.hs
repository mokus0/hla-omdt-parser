module Main where

import Text.OMDT.Lexer (alexScanTokens)
import Text.OMDT.Parser (omdt)
import Text.OMDT.Syntax
import Text.Parsec

import qualified Data.Map as M
import qualified Data.IntMap as I
import System.Environment

main = do
    args <- getArgs
    sequence_
        [ do
            src <- readFile srcName
            let tokens = alexScanTokens src
    
            case runParser omdt () srcName tokens of
                Left err -> do
                    -- mapM_ print tokens
                    print err
                Right objectModel -> do
                    let section heading = do
                            putStrLn ""
                            putStrLn ("    " ++ heading)
                            putStrLn ("    " ++ map (const '-') heading)
                            putStrLn ""
                        dumpMap showKey kvs = sequence_
                            [ do
                                putStr (showKey key ++ ": ")
                                print val
                                putStrLn ""
                            | (key, val) <- kvs
                            ]
            
                    section "Object Model Header"
                    print (header objectModel)
            
                    section "Enumerated Data Types"
                    dumpMap id (M.assocs (enumeratedDataTypes objectModel))
            
                    section "Complex Data Types"
                    dumpMap id (M.assocs (complexDataTypes objectModel))
            
                    section "Classes"
                    dumpMap show (I.assocs (classes objectModel))
            
                    section "Interactions"
                    dumpMap show (I.assocs (interactions objectModel))
            
                    section "Routing Spaces"
                    dumpMap id (M.assocs (routingSpaces objectModel))
            
                    section "Notes"
                    dumpMap show (I.assocs (notes objectModel))
        | srcName <- args
        ]