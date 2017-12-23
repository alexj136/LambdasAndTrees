module TestRecTypes where

import Util
import RecTypes

import TestUtil
import GenData

import qualified Data.Set as S
import qualified Control.Monad.State as SM
import Control.Monad (liftM, liftM2, liftM3, liftM4, liftM5)
import Prelude hiding ((+))

import Test.HUnit
import Test.QuickCheck
import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)

tests = testGroup "RecTypes tests"
    [ testProperty "Equality of folded types"    propFoldEqual
    , testCase     "Simple type equalities"      (assert simpleEqualities)
    , testProperty "Generated types contractive" propContractive
    , testProperty "Generated types closed"      propClosed
    ]

propContractive :: Property
propContractive = forAll genType contractive

propClosed :: Property
propClosed = forAll genType closed

testStateName :: SM.State Name Bool -> Bool
testStateName p = fst $ SM.runState p (Name 1000)

propFoldEqual :: Property
propFoldEqual = forAll genMuType $ \ty -> testStateName $ do
    unfoldTy <- unsafeFold ty
    t1 <- equiv ty unfoldTy
    t2 <- equiv unfoldTy ty
    return $ t1 && t2

simpleEqualities :: Bool
simpleEqualities = and
    [ testStateName (equiv    (TVar (Name 0)) (TVar (Name 0)))
    , testStateName (notEquiv (TVar (Name 1)) (TVar (Name 0)))
    ]

genMuType :: Gen Type
genMuType = do
    n <- genVarName
    t <- doGenType (S.singleton n) (S.singleton n)
    return $ Mu n t

genType :: Gen Type
genType = doGenType S.empty S.empty

-- Generate a closed, contractive type
doGenType ::
    S.Set Name -> -- The TVars in scope
    S.Set Name -> -- The TVars that are not allowed here for contractivity
    Gen Type
doGenType inScope nonCtve = frequency
    [ (3, do n <- genVarName ;
                liftM (Mu n) (doGenType (inScope + n) (nonCtve + n)))
    , (1, liftM2 Pair ty ty  )
    , (1, liftM2 Sum  ty ty  )
    , (1, liftM2 Fun  ty ty  )
    , (p, liftM  TVar genTVar)
    , (2, return Unit        )
    ]
    where
    ty :: Gen Type
    ty = doGenType inScope S.empty

    p :: Int
    p = if S.size (inScope S.\\ nonCtve) == 0 then 0 else 8
    
    (+) :: Ord a => S.Set a -> a -> S.Set a
    (+) s e = S.insert e s

    genTVar :: Gen Name
    genTVar = elements $ S.toList $ inScope S.\\ nonCtve 
