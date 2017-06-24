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
import Control.Monad.Except
import Control.Monad.State
import Data.Map (empty)

main :: IO ExitCode
main = do
    args <- getArgs
    case args of
        [fileName] -> do
            inputText <- readFile fileName
            case runExcept (runPipeline inputText) of
                Right t -> do
                    putStrLn $ show t
                    exitSuccess
                Left m -> do
                    putStrLn $ "Error: " ++ m
                    exitFailure
        _ -> do
            putStrLn "Please supply a single filename."
            exitFailure

runPipeline :: String -> Except String P.Term
runPipeline progText = case runExcept $ scan progText of
    Left m -> throwError $ m empty
    Right (names, nextName, tokens) -> withExcept (\m -> m (swap names)) $ do
        sugarAST <- parse tokens
        (ty, _) <- runStateT (check sugarAST) nextName
        if ty /= TTree then
            throwBasic
                $ "Program has type '" ++ show ty ++ "'. Valid programs have "
                ++ "type '" ++ show TTree ++ "'."
        else do
            pureAST <- desugar sugarAST
            eval pureAST
