module TypeCheck where

import Util
import Types
import SugarSyntax

import Data.Maybe (fromMaybe, fromJust, isJust)
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Writer.Lazy
import qualified Data.Map as M
import qualified Data.Set as S

import Debug.Trace

--------------------------------------------------------------------------------
-- TYPE SCHEME DATATYPE
--------------------------------------------------------------------------------

data Scheme = Quant Name Scheme | Base Type deriving (Eq, Ord)

instance Show Scheme where
    show s = case s of
        Quant n s -> "∀" ++ show n ++ "." ++ show s
        Base ty   -> show ty

--------------------------------------------------------------------------------
-- ENVIRONMENT DATATYPE
--------------------------------------------------------------------------------

newtype Env = Env (M.Map Name Scheme)

extend :: Env -> Name -> Scheme -> Env
extend (Env env) n s = Env $ M.insert n s env

query :: Name -> Env -> Maybe Scheme
query n (Env env) = M.lookup n env

emptyEnv :: Env
emptyEnv = Env M.empty

--------------------------------------------------------------------------------
-- SUBSTITUTION DATATYPE (represented with a Data.Map)
--------------------------------------------------------------------------------

type Subst = M.Map Name Type

idSubst :: Subst
idSubst = M.empty

compose :: Subst -> Subst -> Subst
compose s1 s2 = M.map (apply s1) s2 `M.union` s1

class Substitutable a where
    apply :: Subst -> a -> a
    freeTVars :: a -> S.Set Name

occurs :: Substitutable a => Name -> a -> Bool
occurs n = (S.member n) . freeTVars

instance Substitutable Type where
    apply subst ty = case ty of
        TTree     -> TTree
        TVar n    -> M.findWithDefault (TVar n) n subst
        TFunc t u -> TFunc (apply subst t) (apply subst u)
    freeTVars ty = case ty of
        TTree       -> S.empty
        TFunc t1 t2 -> freeTVars t1 `S.union` freeTVars t2
        TVar  n     -> S.singleton n

instance Substitutable Scheme where
    apply subst sc = case sc of
        Quant n s -> Quant n (apply (M.delete n subst) s)
        Base ty   -> Base (apply subst ty)
    freeTVars sc = case sc of
        Quant n s -> S.delete n (freeTVars s)
        Base ty   -> freeTVars ty

instance Substitutable Env where
    apply subst (Env env) = Env $ M.map (apply subst) env
    freeTVars (Env env) = S.unions $ map freeTVars $ M.elems env

instance Substitutable a => Substitutable [a] where
    apply = map . apply
    freeTVars = S.unions . map freeTVars

instance Substitutable Constraint where
    apply subst (Constraint (ty1, ty2, originTm)) =
        Constraint (apply subst ty1, apply subst ty2, originTm)
    freeTVars (Constraint (ty1, ty2, _)) = freeTVars ty1 `S.union` freeTVars ty2

--------------------------------------------------------------------------------
-- CONSTRAINT GENERATION
--------------------------------------------------------------------------------

newtype Constraint = Constraint (Type, Type, Term)

instance Show Constraint where
    show (Constraint (t1, t2, tm)) = show t1 ++ " /==/ " ++ show t2 ++ "       from       " ++ concat (lines (show tm))

constraints :: Env -> Term -> WriterT [Constraint] (StateT Name Result) Type
constraints env tm = case tm of
    Lam _ x mTy body -> do
        n <- lift fresh
        maybeConstrain mTy (TVar n) tm
        let newEnv = extend env x (Base (TVar n))
        tyBody <- constraints newEnv body
        let inferredTy = TFunc (TVar n) tyBody
        return inferredTy
    Var i x -> case query x env of
        Just sc -> do
            ty <- lift $ instantiate sc
            return ty
        Nothing -> throwError $
            \m -> "Unbound name '" ++ m !? x ++ "': " ++ show i
    App _ func arg -> do
        tyFunc <- constraints env func
        tyArg  <- constraints env arg
        n      <- lift fresh
        constrain tyFunc (TFunc tyArg (TVar n)) tm
        return $ TVar n
    Let _ False x mTy d b -> do
        tyD <- constraints env d
        maybeConstrain mTy tyD tm
        let scD    = generalize env tyD
        let newEnv = extend env x scD
        tyB <- constraints newEnv b
        return tyB
    Let i True x t d b -> constraints env (unLetR i x t d b)
    Fix _ -> do
        n <- lift fresh
        return $ ((TVar n) `TFunc` (TVar n)) `TFunc` (TVar n)
    Cond _ gd tbr fbr -> do
        tyGd  <- constraints env gd
        tyTbr <- constraints env tbr
        tyFbr <- constraints env fbr
        constrain tyGd TTree gd
        constrain tyTbr tyFbr tm
        return tyTbr
    Cons _ l r -> do
        tyL <- constraints env l
        tyR <- constraints env r
        constrain tyL TTree l
        constrain tyR TTree r
        return TTree
    Hd _ e -> do
        tyE <- constraints env e
        constrain tyE TTree e
        return TTree
    Tl _ e -> do
        tyE <- constraints env e
        constrain tyE TTree e
        return TTree
    Nil _ -> return TTree

    where

    constrain :: Monad m => Type -> Type -> Term -> WriterT [Constraint] m ()
    constrain t1 t2 tm = tell [traceShowId $ Constraint (t1, t2, tm)]

    maybeConstrain :: Monad m => Maybe Type -> Type -> Term ->
        WriterT [Constraint] m ()
    maybeConstrain mTy1 ty2 tm = case mTy1 of
        Nothing  -> return ()
        Just ty1 -> constrain ty1 ty2 tm

-- Generalize a type by quantifying over all free type variables it contains. Do
-- not generalize any type variables that exist in the current scope as they
-- have a 'broader meaning'.
generalize :: Env -> Type -> Scheme
generalize env ty = foldr Quant (Base ty)
    (S.toList (freeTVars ty `S.difference` freeTVars env))

-- Replace implicitly quantified type variables in a given type with fresh names
instantiate :: Monad m => Scheme -> StateT Name m Type
instantiate sc = case sc of
    Base ty   -> return ty
    Quant x s -> do
        y <- fresh
        t <- instantiate s
        return $ apply (M.singleton x (TVar y)) t

--------------------------------------------------------------------------------
-- UNIFICATION
--------------------------------------------------------------------------------

unifyMany :: [Constraint] -> Result Subst
unifyMany [] = return idSubst
unifyMany (constr:rest) = do
    s1 <- unify constr
    s2 <- unifyMany (apply s1 rest)
    return $ compose s2 s1

unify :: Constraint -> Result Subst
unify (Constraint (ty1, ty2, originTm)) = case (ty1, ty2) of
    (a        , α        ) | a == α -> return idSubst
    (TVar x   , t        ) -> handleVar x t originTm
    (t        , TVar x   ) -> handleVar x t originTm
    (TFunc a b, TFunc α β) ->
        unifyMany [Constraint (a, α, originTm), Constraint (b, β, originTm)]
    (a        , α        ) -> throwError $ \m ->
        "Could not match type\n"
        ++ "    " ++ prettyPrint m a ++ "\n"
        ++ "with\n"
        ++ "    " ++ prettyPrint m α ++ "\n"
        ++ "at " ++ showMPos (getPos originTm) ++ "\n"
        ++ "in expression:\n"
        ++ prettyPrint m originTm

    where

    handleVar :: Name -> Type -> Term -> Result Subst
    handleVar n ty tm
        | ty == TVar n = return idSubst
        | occurs n ty  = throwError $ \m ->
            "Illegal infinite type at " ++ showMPos (getPos tm)
            ++ "\nin expression:\n" ++ prettyPrint m originTm
        | otherwise    = return $ M.singleton n ty

--------------------------------------------------------------------------------
-- EXTERNAL INTERFACE
--------------------------------------------------------------------------------

check :: Term -> StateT Name Result Type
check tm = do
    (ty, constrs) <- runWriterT $ constraints emptyEnv tm
    unifier       <- lift $ unifyMany constrs
    return $ apply unifier ty
