module RecTypes where

import Util
import Control.Monad.State
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe (isJust)

----------------------------------
-- Datatype for recursive types --
----------------------------------

data Type
    = Mu   Name Type
    | Pair Type Type
    | Sum  Type Type
    | Fun  Type Type
    | TVar Name
    | Unit
    deriving (Eq, Ord)

instance Show Type where
    show t = case t of
        Mu   n t -> "(μ" ++ show n ++ "."    ++ show t ++ ")"
        Pair t u -> "("  ++ show t ++ ", "   ++ show u ++ ")"
        Sum  t u -> "("  ++ show t ++ " + "  ++ show u ++ ")"
        Fun  t u -> "("  ++ show t ++ " -> " ++ show u ++ ")"
        TVar n   -> show n
        Unit     -> "Unit"

-----------------------
-- Useful predicates --
-----------------------

free :: Type -> S.Set Name
free ty = case ty of
    Mu   n t -> S.delete n $ free t
    Pair t u -> S.union (free t) (free u)
    Sum  t u -> S.union (free t) (free u)
    Fun  t u -> S.union (free t) (free u)
    TVar n   -> S.singleton n
    Unit     -> S.empty

closed :: Type -> Bool
closed = S.null . free

contractive :: Type -> Bool
contractive = ck S.empty
    where
    ck :: S.Set Name -> Type -> Bool
    ck s t = case t of
        Mu   n t -> ck (S.insert n s) t
        Pair t u -> ck S.empty t && ck S.empty u
        Sum  t u -> ck S.empty t && ck S.empty u
        Fun  t u -> ck S.empty t && ck S.empty u
        TVar n   -> S.notMember n s
        Unit     -> True

notEquiv :: Type -> Type -> State Name Bool
notEquiv t = liftM not . (equiv t)

----------------------------------
-- Substitution and equivalence --
----------------------------------

subst :: Name -> Type -> Type -> State Name Type
subst n t u = case u of
    Mu   m v -> if n == m then return u
        else if S.member m (free t) then do
            m'  <- fresh
            v'  <- subst m (TVar m') v
            v'' <- subst n t v'
            return $ Mu m' v''
        else liftM (Mu m) (subst n t v)
    Pair v w -> liftM2 Pair (subst n t v) (subst n t w)
    Sum  v w -> liftM2 Sum  (subst n t v) (subst n t w)
    Fun  v w -> liftM2 Fun  (subst n t v) (subst n t w)
    TVar m   -> return $ if n == m then t else u
    Unit     -> return Unit

equiv :: Type -> Type -> State Name Bool
equiv t u = case (t, u) of
    (Mu   n v, Mu   m w) -> return $ αEquiv t u
    (Pair v w, Pair x y) -> liftM2 (&&) (equiv v x) (equiv w y)
    (Sum  v w, Sum  x y) -> liftM2 (&&) (equiv v x) (equiv w y)
    (Fun  v w, Fun  x y) -> liftM2 (&&) (equiv v x) (equiv w y)
    (TVar m  , TVar o  ) -> return $ m == o
    (Unit    , Unit    ) -> return True
    (Mu   n v, w       ) -> muEquiv n v w
    (v       , Mu n w  ) -> muEquiv n w v
    (_       , _       ) -> return False
    where
    muEquiv :: Name -> Type -> Type -> State Name Bool
    muEquiv n v w = liftM2 (||)
        (do v' <- fold n v    ; equiv v' w)
        (do v' <- subst n w v ; equiv v' w)
    

unsafeFold :: Type -> State Name Type
unsafeFold (Mu n t) = fold n t

fold :: Name -> Type -> State Name Type
fold n t = subst n (Mu n t) t

αEquiv :: Type -> Type -> Bool
αEquiv t = isJust . αEquivMap t

type Αmap = Maybe (M.Map Name Name)

αEquivMap :: Type -> Type -> Αmap
αEquivMap t u = case (t, u) of
    (Mu   n v, Mu   m w) -> αEnsure (αEquivMap v w) n m
    (Pair v w, Pair x y) -> αCombine (αEquivMap v x) (αEquivMap w y)
    (Sum  v w, Sum  x y) -> αCombine (αEquivMap v x) (αEquivMap w y)
    (Fun  v w, Fun  x y) -> αCombine (αEquivMap v x) (αEquivMap w y)
    (TVar m  , TVar o  ) -> Just $ M.singleton m o
    (Unit    , Unit    ) -> Just M.empty
    (_       , _       ) -> Nothing
    
αCombine :: Αmap -> Αmap -> Αmap
αCombine aMap bMap = do
    mapA <- aMap
    mapB <- bMap
    if and (S.map (\n -> mapA M.! n == mapB M.! n)
            (S.intersection (M.keysSet mapA) (M.keysSet mapB))) then
        return (M.union mapA mapB)
    else Nothing

αEnsure :: Αmap -> Name -> Name -> Αmap
αEnsure aMap n m = do
    map <- aMap
    if (M.notMember n map && notElem m (M.elems map))
        || (M.member n map && map M.! n == m) then aMap
    else Nothing
