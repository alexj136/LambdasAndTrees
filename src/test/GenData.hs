module GenData where

import SugarSyntax
import Types

import Control.Monad (liftM, liftM2, liftM3)
import Test.QuickCheck
import qualified Data.Map as M

instance Arbitrary Type where
    arbitrary = frequency
        [ ( 5, return TTree                     )
        , ( 2, liftM2 TFunc arbitrary arbitrary )
        ]

arbitraryType :: Gen Type
arbitraryType = arbitrary

-- Does not generate all possible variable names (i.e. does not agree with the
-- lexer's RE) but useful nonetheless.
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

-- Generate a well-typed Term. Does not generate all possible well-typed terms,
-- but is nonetheless very useful for testing the type checker.
--
--     Takes an integer to stop the algorithm blowing up.
--     Takes an environment so bound variables of the correct type can be used.
--     Takes a Type - this is the 'goal-type' for the generated term.
--
--  WARNING! The given integer must be used carefully. The size of the generated
--  term will be exponentially related to the given integer. Small values
--  (e.g. 5) are perfectly sufficient for generating interesting programs.
--  A value like 20 would be far too high and would take a VERY long time to
--  terminate.
genTypeSafeTerm :: Integer -> M.Map String Type -> Type -> Gen Term
genTypeSafeTerm depth env ty = frequency $ case ty of
    -- If the goal type is TTree and there are no bound of this type, pick
    -- a term from tTreeRest (see below).
    TTree     | null (namesOfType TTree) -> tTreeRest

    -- If the goal type is TTree and there are bound of this type, pick
    -- either a term from tTreeRest (see below) or one of those variables.
    TTree     | otherwise                ->
        (cn, liftM Var (oneof $ map return $ namesOfType TTree)) : tTreeRest

    -- A similar logic here - if there are no variables of the goal type, don't
    -- generate a variable, but if there are, perhaps choose one.
    TFunc a r | null (namesOfType ty) -> tFuncRest a r
    TFunc a r | otherwise             ->
        (cn, liftM Var (oneof $ map return $ namesOfType ty)) : tFuncRest a r

    where

    ex, eq, cn :: Int
    -- probability of generating something to make the term grow
    ex = if depth <= 1 then 0 else 4
    -- probability of generating something to keep the term about the same size
    eq = if depth <= 1 then 0 else 2
    -- probability of generating something to make the term shrink
    cn = 4

    -- Make a recursive call for a given type with the same environment
    recurse :: Type -> Gen Term
    recurse = genTypeSafeTerm (depth - 1) env

    -- Make a recursive call but add a new name to the environment for the
    -- recursive call
    recurseWith :: String -> Type -> Type -> Gen Term
    recurseWith newName typeOfNewName =
        genTypeSafeTerm (depth - 1) (M.insert newName typeOfNewName env)

    -- Get all names of a given type from an environment
    namesOfType :: Type -> [String]
    namesOfType ty = M.keys $ M.filter (== ty) env

    -- Possible TTree type expressions to generate, without variables
    tTreeRest :: [(Int, Gen Term)]
    tTreeRest =
        [ ( ex, application TTree                                           )
        --, ( 4, liftM Fix (genTypeSafeTerm env TTree)                       )
        , ( ex, liftM3 Cond (recurse TTree) (recurse TTree) (recurse TTree) )
        , ( ex, liftM2 Cons (recurse TTree) (recurse TTree)                 )
        , ( eq, liftM  Tl   (recurse TTree)                                 )
        , ( eq, liftM  Hd   (recurse TTree)                                 )
        , ( cn, return Nil                                                  )
        ]

    -- Possible TFunc type expressions to generate, without variables
    tFuncRest :: Type -> Type -> [(Int, Gen Term)]
    tFuncRest argTy retTy =
        [ ( cn, lambda argTy retTy                                    )
        , ( ex, application ty                                        )
        --, ( 4, liftM Fix arbitrary                                   )
        , ( ex, liftM3 Cond (recurse TTree) (recurse ty) (recurse ty) )
        ]

    -- Generate a lambda expression with the given argument and return type
    lambda :: Type -> Type -> Gen Term
    lambda argTy retTy = do
        x <- genVarName
        liftM3 Lam (return x) (return $ Just argTy) (recurseWith x argTy retTy)

    -- Generate an application expression with a given type. The argument type
    -- is randomly chosen.
    application :: Type -> Gen Term
    application retTy = do
        argTy <- if depth <= 1 then return TTree else arbitraryType
        liftM2 App (recurse (TFunc argTy retTy)) (recurse argTy)
