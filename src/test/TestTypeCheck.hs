module TestTypeCheck where

import TestUtil
import GenData
import Types
import TypeCheck

import Test.QuickCheck
import Test.Framework (testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

tests = testGroup "TypeCheck tests"
    [ testProperty "propWellTypedTermsCheck" propWellTypedTermsCheck
    ]

propWellTypedTermsCheck :: Property
propWellTypedTermsCheck = forAll (typeSafeTerm TTree) check
