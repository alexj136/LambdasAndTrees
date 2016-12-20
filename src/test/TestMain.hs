module Main where

import GenData
import Types
import qualified TestTypeCheck as TC
import qualified TestParser    as P

import Test.QuickCheck
import Test.Framework (defaultMain, Test)

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [TC.tests, P.tests]
