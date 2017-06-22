module TypeCheck where

import Util
import Types
import SugarSyntax

import Data.Maybe (fromMaybe)
import Control.Monad.State
import Control.Monad.Except (throwError)
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

check :: Term -> Result Type
check tm = do
    ((ty, _), constrs) <- runWriterT $ runStateT (constraints M.empty tm) 0
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

        Constraint (ty1, ty2, tm) -> throwError
            $ "Unsatisfiable type constraint at " ++ showMPos (getPos tm)
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

constraints :: Env -> Term ->
    StateT Integer (WriterT Constraints Result) Type
constraints env tm = case tm of
    Lam _ x Nothing body -> do
        n <- fresh
        let newEnv = M.insert x (TVar n) env
        tyBody <- constraints newEnv body
        return $ TFunc (TVar n) tyBody
    Lam _ x (Just tyX) body -> do
        let newEnv = M.insert x tyX env
        tyBody <- constraints newEnv body
        return $ TFunc tyX tyBody
    Var i x -> case M.lookup x env of
        Just ty -> return ty
        Nothing -> throwError $ "Unbound name '" ++ show x ++ "': " ++ show i
    App _ func arg -> do
        n <- fresh
        tyFunc <- constraints env func
        tyArg  <- constraints env arg
        constrain tyFunc (TFunc tyArg (TVar n)) tm
        return $ TVar n
    Let _ x d b -> do
        b' <- subst d x b
        tyB' <- constraints env b'
        return tyB'
    Fix _ -> do
        n <- fresh
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

    fresh :: Monad m => StateT Integer m Integer
    fresh = state $ \n -> (n, n + 1)

    constrain :: MonadTrans m => Monad n => Type -> Type -> Term ->
        m (WriterT Constraints n) ()
    constrain t1 t2 tm = lift $ tell $ S.singleton (Constraint (t1, t2, tm))

    subst :: Monad m => Term -> Name -> Term -> StateT Integer m Term
    subst arg x body = case body of
        Lam _ y _ _ | y == x -> return body
        Lam i y ty b | y `S.member` free arg -> do
            n <- fresh
            b' <- subst (Var NoInfo (Name n)) y b
            liftM (Lam i (Name n) ty) (subst arg x b')
        Lam i y ty b | otherwise -> liftM (Lam i y ty) (subst arg x b)
        Var i y | x == y -> return arg
        Var i y | x /= y -> return body
        App i f a -> liftM2 (App i) (subst arg x f) (subst arg x a)
        Let _ y _ _ | y == x -> return body
        Let i y d b | y `S.member` free arg -> do
            n <- fresh
            b' <- subst (Var NoInfo (Name n)) y b
            liftM2 (Let i (Name n)) (subst arg x d) (subst arg x b')
        Let i y d b | otherwise ->
            liftM2 (Let i y) (subst arg x d) (subst arg x b)
        Fix i -> return $ Fix i
        Cond i gd tbr fbr ->
            liftM3 (Cond i) (subst arg x gd) (subst arg x tbr) (subst arg x fbr)
        Cons i l r -> liftM2 (Cons i) (subst arg x l) (subst arg x r)
        Hd i e -> liftM (Hd i) (subst arg x e)
        Tl i e -> liftM (Tl i) (subst arg x e)
        Nil i -> return $ Nil i

    free :: Term -> S.Set Name
    free tm = case tm of
        Lam _ x _ body    -> S.delete x (free body)
        Var _ x           -> S.singleton x
        App _ func arg    -> free func `S.union` free arg
        Let _ x d b       -> free d `S.union` (S.delete x (free b))
        Fix _             -> S.empty
        Cond _ gd tbr fbr -> free gd `S.union` free tbr `S.union` free fbr
        Cons _ l r        -> free l `S.union` free r
        Hd _ e            -> free e
        Tl _ e            -> free e
        Nil _             -> S.empty
