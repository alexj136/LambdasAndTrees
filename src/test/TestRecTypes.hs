module TestRecTypes where

import Util
import RecTypes

import TestUtil
import GenData

import qualified Control.Monad.State as SM

import Test.HUnit
import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit

tests = testGroup "RecTypes tests"
    [ testCase "Equality of folded and unfolded list type" (assert unfoldListEqual)
    , testCase "Simple type equalities" (assert simpleEqualities)
    ]

testStateName :: SM.State Name Bool -> Bool
testStateName p = fst $ SM.runState p (Name 1000)

unitList :: Type
unitList = Mu (Name 0) (Sum (Pair Unit (TVar (Name 0))) Unit)

unfoldListEqual :: Bool
unfoldListEqual = testStateName $ do
    unfoldedUnitList <- unsafeFold unitList
    t1 <- equiv unitList unfoldedUnitList
    t2 <- equiv unfoldedUnitList unitList
    return $ t1 && t2

simpleEqualities :: Bool
simpleEqualities = and
    [ testStateName (equiv    (TVar (Name 0)) (TVar (Name 0)))
    , testStateName (notEquiv (TVar (Name 1)) (TVar (Name 0)))
    ]
