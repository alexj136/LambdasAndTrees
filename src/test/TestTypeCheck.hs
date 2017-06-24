module TestTypeCheck where

import Util
import SugarSyntax
import Types
import TypeCheck

import TestUtil
import GenData

import qualified Data.Set as S
import Control.Monad.State (runStateT)

import Test.QuickCheck
import Test.Framework (testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

tests = testGroup "TypeCheck tests"
    [ testProperty "propWellTypedTermsCheck" propWellTypedTermsCheck
    ]

propWellTypedTermsCheck :: Property
propWellTypedTermsCheck = forAll (typeSafeTerm TTree) $ \tm -> do
    let fv = freeVars tm
    let nm = if S.null fv then (Name 0) else after (S.findMax fv)
    (ty, _) <- runStateT (check tm) nm
    return (ty == TTree)
