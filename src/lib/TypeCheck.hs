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

type Env = M.Map Name Type

type Constraints = S.Set Constraint

-- A constraint also carries a Term, which is the source origin of the
-- constraint, used for error messages
newtype Constraint = Constraint (Type, Type, Term) deriving (Show, Ord)

instance Eq Constraint where
    Constraint (a, b, _) == Constraint (c, d, _) = (a, b) == (c, d)

check :: Term -> StateT Name Result Type
check tm = do
    (ty, constrs) <- runWriterT $ constraints M.empty tm
    unifier       <- lift $ unify constrs
    return $ unifier ty

unify :: Constraints -> Result (Type -> Type)
unify constrs = if null constrs then return id else
    let (next, rest) = S.deleteFindMin constrs in case next of

        Constraint (ty1, ty2, _) | ty1 == ty2 -> unify rest

        Constraint (TVar x, ty, _) | not $ occurs x ty -> handleVar x ty rest

        Constraint (ty, TVar x, _) | not $ occurs x ty -> handleVar x ty rest

        Constraint (TFunc a1 b1, TFunc a2 b2, tm) -> unify $
            S.insert (Constraint (a1, a2, tm)) $
            S.insert (Constraint (b1, b2, tm)) $ rest

        Constraint (ty1, ty2, tm) -> throwError $ \m ->
            "Unsatisfiable type constraint at " ++ showMPos (getPos tm)
            ++ "\nin expression:\n" ++ prettyPrint m tm

    where

    handleVar :: Name -> Type -> Constraints -> Result (Type -> Type)
    handleVar x ty rest = do
        unifyRest <- unify (S.map (constrSub x ty) rest)
        return $ unifyRest . (tySub x ty)

    constrSub :: Name -> Type -> Constraint -> Constraint
    constrSub x ty (Constraint (t1, t2, tm)) =
        Constraint (tySub x ty t1, tySub x ty t2, tm)

    tySub :: Name -> Type -> Type -> Type
    tySub from to within = case within of
        TVar  x   -> if x == from then to else TVar x
        TFunc a b -> TFunc (tySub from to a) (tySub from to b)
        TTree     -> TTree

    occurs :: Name -> Type -> Bool
    occurs x (TVar  y  ) = x == y
    occurs x (TFunc a b) = occurs x a || occurs x b
    occurs x  TTree      = False

constraints :: Env -> Term -> WriterT Constraints (StateT Name Result) Type
constraints env tm = case tm of
    Lam _ x Nothing body -> do
        n <- lift fresh
        let newEnv = M.insert x (TVar n) env
        tyBody <- constraints newEnv body
        return $ TFunc (TVar n) tyBody
    Lam _ x (Just tyX) body -> do
        let newEnv = M.insert x tyX env
        tyBody <- constraints newEnv body
        return $ TFunc tyX tyBody
    Var i x -> case M.lookup x env of
        Just ty -> return ty
        Nothing -> throwError $
            \m -> "Unbound name '" ++ m !? x ++ "': " ++ show i
    App _ func arg -> do
        n <- lift fresh
        tyFunc <- constraints env func
        tyArg  <- constraints env arg
        constrain tyFunc (TFunc tyArg (TVar n)) tm
        return $ TVar n
    Let _ x d b -> do
        b' <- lift $ subst d x b
        tyB' <- constraints env b'
        return tyB'
    LetR i x d b -> constraints env (unLetR i x d b)
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

    fresh :: Monad m => StateT Name m Name
    fresh = state $ \n -> (n, after n)

    constrain :: Monad m => Type -> Type -> Term -> WriterT Constraints m ()
    constrain t1 t2 tm = tell $ S.singleton (Constraint (t1, t2, tm))

    subst :: Monad m => Term -> Name -> Term -> StateT Name m Term
    subst arg x body = case body of
        Lam i y t b ->
            if y == x then
                return body
            else if y `S.member` freeVars arg then do
                n <- fresh
                b' <- subst (Var NoInfo n) y b
                liftM (Lam i n t) (subst arg x b')
            else
                liftM (Lam i y t) (subst arg x b)
        Let i y d b ->
            if y == x then
                return body
            else if y `S.member` freeVars arg then do
                n <- fresh
                b' <- subst (Var NoInfo n) y b
                liftM2 (Let i n) (subst arg x d) (subst arg x b')
            else
                liftM2 (Let i y) (subst arg x d) (subst arg x b)
        LetR i y d b ->
            if y == x then
                return body
            else if y `S.member` freeVars arg then do
                n <- fresh
                b' <- subst (Var NoInfo n) y b
                liftM2 (Let i n) (subst arg x d) (subst arg x b')
            else
                liftM2 (Let i y) (subst arg x d) (subst arg x b)
        Var i y -> return $ if x == y then arg else body
        App i f a -> liftM2 (App i) (subst arg x f) (subst arg x a)
        Fix i -> return $ Fix i
        Cond i gd tbr fbr ->
            liftM3 (Cond i) (subst arg x gd) (subst arg x tbr) (subst arg x fbr)
        Cons i l r -> liftM2 (Cons i) (subst arg x l) (subst arg x r)
        Hd i e -> liftM (Hd i) (subst arg x e)
        Tl i e -> liftM (Tl i) (subst arg x e)
        Nil i -> return $ Nil i
