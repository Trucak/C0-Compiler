module Main where

import Lexer
import Parser
import AST
import Typecheck
import IntermCode
import FinalCode
import Control.Monad.State
import qualified Data.Map as Map

main :: IO ()
main = do
    txt <- getContents

    -- Lexer
    putStrLn "Lexer:"
    let tokens = alexScanTokens txt
    print (tokens)
    putStrLn ""

    -- Parser
    putStrLn "Parser:"
    let prog = parse $ tokens
    print (prog)
    putStrLn ""

    -- Type checker (doesnt work for variable assignments with function calls)
    {-putStrLn "Type checker:"
    let checker = checkProg Map.empty prog
    print (checker)
    putStrLn ""-}

    -- Intermediate code
    putStrLn "Intermediate code:"
    let intermediate = runState (transProg prog Map.empty) (0,0)
    print (intermediate)
    putStrLn ""

    -- Final code
    putStrLn "Final code:"
    let final = finalTransProg (fst intermediate)
    putStrLn final
    putStrLn ""