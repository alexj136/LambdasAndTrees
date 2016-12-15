module Main where

import SugarSyntax
import Types

import Control.Monad (liftM, liftM2, liftM3)
import System.Exit
import Test.QuickCheck
import qualified Data.Map as M

instance Arbitrary Type where
    arbitrary = frequency
        [ ( 5, return TTree                     )
        , ( 2, liftM2 TFunc arbitrary arbitrary )
        ]

arbitraryType :: Gen Type
arbitraryType = arbitrary

genVarName :: Gen String
genVarName = listOf $ elements $ ['a'..'z'] ++ ['A'..'Z']

instance Arbitrary Term where
    arbitrary = frequency
        [ ( 2, liftM3 Lam genVarName arbitrary arbitrary )
        , ( 9, liftM Var genVarName                      )
        , ( 2, liftM2 App arbitrary arbitrary            )
        , ( 4, liftM Fix arbitrary                       )
        , ( 1, liftM3 Cond arbitrary arbitrary arbitrary )
        , ( 2, liftM2 Cons arbitrary arbitrary           )
        , ( 4, liftM Hd arbitrary                        )
        , ( 4, liftM Tl arbitrary                        )
        , ( 9, return Nil                                )
        ]

arbitraryTerm :: Gen Term
arbitraryTerm = arbitrary

genTypeSafeTerm :: M.Map String Type -> Type -> Gen Term
genTypeSafeTerm env ty = frequency $ case ty of
    TTree     | null (namesOfType TTree) -> tTreeRest
    TTree     | otherwise                ->
        (9, liftM Var (oneof $ map return $ namesOfType TTree)) : tTreeRest
    TFunc a r | null (namesOfType ty) -> tFuncRest a r
    TFunc a r | otherwise             ->
        (20, liftM Var (oneof $ map return $ namesOfType ty)) : tFuncRest a r

    where

    namesOfType :: Type -> [String]
    namesOfType ty = M.keys $ M.filter (== ty) env

    tTreeRest :: [(Int, Gen Term)]
    tTreeRest =
        [ ( 2 , application TTree                                        )
        --, ( 4, liftM Fix (genTypeSafeTerm env TTree)                   )
        , ( 1 , liftM3 Cond (genTypeSafeTerm env TTree)
            (genTypeSafeTerm env TTree) (genTypeSafeTerm env TTree)      )
        , ( 2 , liftM2 Cons (genTypeSafeTerm env TTree)
                (genTypeSafeTerm env TTree)                              )
        , ( 4 , liftM Tl (genTypeSafeTerm env TTree)                     )
        , ( 4 , liftM Hd (genTypeSafeTerm env TTree)                     )
        , ( 20, return Nil                                               )
        ]

    tFuncRest :: Type -> Type -> [(Int, Gen Term)]
    tFuncRest argTy retTy =
        [ ( 4, lambda argTy retTy                        )
        , ( 2, application ty                            )
        --, ( 4, liftM Fix arbitrary                     )
        , ( 1, liftM3 Cond (genTypeSafeTerm env TTree) (genTypeSafeTerm env ty)
                (genTypeSafeTerm env ty)                 )
        ]

    lambda :: Type -> Type -> Gen Term
    lambda argTy retTy = do
        x <- genVarName
        liftM3 Lam (return x) (return $ Just argTy)
            (genTypeSafeTerm (M.insert x argTy env) retTy)

    application :: Type -> Gen Term
    application retTy = do
        argTy <- customArbType
        liftM2 App (genTypeSafeTerm env (TFunc argTy retTy))
            (genTypeSafeTerm env argTy)

    customArbType :: Gen Type
    customArbType = frequency
        [ ( 4, return TTree                             )
        , ( 1, liftM2 TFunc customArbType customArbType )
        ]

main :: IO ExitCode
main = do
    tm <- generate $ genTypeSafeTerm M.empty TTree
    putStrLn $ show tm
    exitFailure
