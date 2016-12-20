module Main where

import Util
import Lexer
import Parser
import SugarSyntax
import Types
import qualified DeBruijnSyntax as P
import TypeCheck
import Interpreter
import CodeGen

import System.Exit
import System.Environment (getArgs)

main :: IO ExitCode
main = do
    args <- getArgs
    case args of
        [fileName] -> do
            inputText <- readFile fileName
            case runPipeline inputText of
                Success t -> do
                    putStrLn $ show t
                    exitSuccess
                Error m -> do
                    putStrLn $ "Error: " ++ m
                    exitFailure
        _ -> do
            putStrLn "Please supply a single filename."
            exitFailure

runPipeline :: String -> Result P.Term
runPipeline progText = do
    let tokens = alexScanTokens progText
    sugarAST <- parse tokens
    ty       <- check sugarAST
    if ty /= TTree then
        Error $ "Program has type '" ++ show ty ++ "'. Valid programs have "
        ++ "type '" ++ show TTree ++ "'."
    else do
        pureAST  <- desugar sugarAST
        eval pureAST
