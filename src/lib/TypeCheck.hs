module TypeCheck where

import Util
import Types
import SugarSyntax

import Data.Maybe (fromMaybe)
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Writer.Lazy
import qualified Data.Map as M
import qualified Data.Set as S

type Env = M.Map String Type

type Constraints = S.Set Constraint

-- A constraint also carries a Term, which is the source origin of the
-- constraint, used for error messages
newtype Constraint = Constraint (Type, Type, Term) deriving (Show, Ord)

instance Eq Constraint where
    Constraint (a, b, _) == Constraint (c, d, _) = (a, b) == (c, d)

check :: Term -> Result Type
check tm = do
    (_, ty, constrs) <- constraints M.empty 0 tm
    unifyFn <- unify constrs
    return $ unifyFn ty

unify :: Constraints -> Result (Type -> Type)
unify constrs = if null constrs then return id else
    let (next, rest) = S.deleteFindMin constrs in case next of

        Constraint (ty1, ty2, _) | ty1 == ty2 -> unify rest

        Constraint (TVar x, ty, _) | not $ occurs x ty -> handleVar x ty rest

        Constraint (ty, TVar x, _) | not $ occurs x ty -> handleVar x ty rest

        Constraint (TFunc a1 b1, TFunc a2 b2, tm) -> unify $
            S.insert (Constraint (a1, a2, tm)) $
            S.insert (Constraint (b1, b2, tm)) $ rest

        Constraint (ty1, ty2, tm) -> Error $ "Unsatisfiable type constraint at "
            ++ (fromMaybe "[NO POSITION DATA]" (fmap show (getPos tm)))
            ++ "\nin expression:\n" ++ show tm

    where

    handleVar :: Integer -> Type -> Constraints -> Result (Type -> Type)
    handleVar x ty rest = do
        unifyRest <- unify (S.map (constrSub x ty) rest)
        return $ unifyRest . (tySub x ty)

    constrSub :: Integer -> Type -> Constraint -> Constraint
    constrSub x ty (Constraint (t1, t2, tm)) =
        Constraint (tySub x ty t1, tySub x ty t2, tm)

    tySub :: Integer -> Type -> Type -> Type
    tySub from to within = case within of
        TVar  x   -> if x == from then to else TVar x
        TFunc a b -> TFunc (tySub from to a) (tySub from to b)
        TTree     -> TTree

    occurs :: Integer -> Type -> Bool
    occurs x (TVar  y  ) = x == y
    occurs x (TFunc a b) = occurs x a || occurs x b
    occurs x  TTree      = False

constraints :: Env -> Integer -> Term -> Result (Integer, Type, Constraints)
constraints env n tm =
    case runExcept $ runWriterT $ runStateT (constraintGen env tm) n of
        Left s             -> Error s
        Right ((t, i), cs) -> return (i, t, cs)

constraintGen :: Env -> Term ->
    StateT Integer (WriterT Constraints (Except String)) Type
constraintGen env tm = case tm of
    Lam _ x Nothing body -> do
        n <- fresh
        let newEnv = M.insert x (TVar n) env
        tyBody <- constraintGen newEnv body
        return $ TFunc (TVar n) tyBody
    Lam _ x (Just tyX) body -> do
        let newEnv = M.insert x tyX env
        tyBody <- constraintGen newEnv body
        return $ TFunc tyX tyBody
    Var _ x -> case M.lookup x env of
        Just ty -> return ty
        Nothing -> throwError $ "Unbound variable '" ++ x ++ "' found."
    App _ func arg -> do
        n <- fresh
        tyFunc <- constraintGen env func
        tyArg  <- constraintGen env arg
        constrain tyFunc (TFunc tyArg (TVar n)) tm
        return $ TVar n
    Fix _ f -> do
        n <- fresh
        tyF <- constraintGen env f
        constrain tyF (TFunc (TVar n) (TVar n)) f
        return $ TVar n
    Cond _ gd tbr fbr -> do
        tyGd  <- constraintGen env gd
        tyTbr <- constraintGen env tbr
        tyFbr <- constraintGen env fbr
        constrain tyGd TTree gd
        constrain tyTbr tyFbr tm
        return tyTbr
    Cons _ l r -> do
        tyL <- constraintGen env l
        tyR <- constraintGen env r
        constrain tyL TTree l
        constrain tyR TTree r
        return TTree
    Hd _ e -> do
        tyE <- constraintGen env e
        constrain tyE TTree e
        return TTree
    Tl _ e -> do
        tyE <- constraintGen env e
        constrain tyE TTree e
        return TTree
    Nil _ -> return TTree

    where

    fresh :: Monad m => StateT Integer m Integer
    fresh = state $ \n -> (n, n + 1)

    constrain :: MonadTrans m => Monad n => Type -> Type -> Term ->
        m (WriterT Constraints n) ()
    constrain t1 t2 tm = lift $ tell $ S.singleton (Constraint (t1, t2, tm))
