module Main where

import Util
import Lexer
import Parser
import SugarSyntax
import qualified Syntax as P
import TypeCheck
import Interpreter

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
    pureAST  <- desugar sugarAST
    ty       <- check pureAST
    if ty /= TTree then
        Error $ "Program has type '" ++ show ty ++ "'. Valid programs have "
        ++ "type '" ++ show TTree ++ "'."
    else
        eval pureAST
