module TestParser where

import TestUtil
import GenData
import Lexer
import Parser

import Test.QuickCheck
import Test.Framework (testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

tests = testGroup "Parser tests"
    [ testProperty "unparseAndParseIsSame" unparseAndParseIsSame
    ]

unparseAndParseIsSame :: Property
unparseAndParseIsSame = forAll arbitraryTerm
    (\tm -> (parse . alexScanTokens . show) tm == return tm)
