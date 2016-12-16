module Main where

import GenData
import Types

import System.Exit
import Test.QuickCheck
import Data.Map (empty)

main :: IO ExitCode
main = do
    tm <- generate $ genTypeSafeTerm 5 empty TTree
    putStrLn $ show tm
    exitFailure
