module Main where

import TestUtil
import qualified TestParser as P
import qualified TestTypeCheck as TC

import System.Exit

main :: IO ExitCode
main = do
    putStr "\n"
    result <- runTests tests
    if result then do
        putStrLn "All tests passed."
        exitSuccess
    else
        exitFailure

tests :: [Test]
tests = TC.tests ++ P.tests
