module TestParser where

import TestUtil
import GenData
import Util
import Lexer
import Parser
import SugarSyntax

import qualified Data.Map as M
import Test.QuickCheck
import Test.Framework (testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Debug.Trace

tests = testGroup "Parser tests"
    [
    ]
