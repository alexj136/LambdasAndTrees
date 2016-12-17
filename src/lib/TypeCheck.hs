module TypeCheck where

import Util
import Types
import SugarSyntax
import qualified Data.Map as M
import qualified Data.Set as S

type Env = M.Map String Type
type Constraints = S.Set (Type, Type)

check :: Term -> Result Type
check tm = do
    (_, ty, constrs) <- constraints M.empty 0 tm
    unifyFn <- unify constrs
    return $ unifyFn ty

unify :: Constraints -> Result (Type -> Type)
unify constrs = if null constrs then return id else
    let (next, rest) = S.deleteFindMin constrs in case next of

        (ty1, ty2) | ty1 == ty2 -> unify rest

        (TVar x, ty) | not $ occurs x ty -> handleVar x ty rest

        (ty, TVar x) | not $ occurs x ty -> handleVar x ty rest

        (TFunc a1 b1, TFunc a2 b2) -> unify $
            S.insert (a1, a2) $ S.insert (b1, b2) $ rest

        _ -> Error "Type error"

    where

        handleVar :: Integer -> Type -> Constraints -> Result (Type -> Type)
        handleVar x ty rest = do
            let tySubFn = tySub x ty
            unifyRest <- unify (S.map (\(a, b) -> (tySubFn a, tySubFn b)) rest)
            return $ unifyRest . tySubFn

        occurs :: Integer -> Type -> Bool
        occurs x (TVar  y  ) = x == y
        occurs x (TFunc a b) = occurs x a || occurs x b
        occurs x  TTree      = False

        tySub :: Integer -> Type -> Type -> Type
        tySub from to within = case within of
            TVar  x   -> if x == from then to else TVar x
            TFunc a b -> TFunc (tySub from to a) (tySub from to b)
            TTree     -> TTree

constraints :: Env -> Integer -> Term -> Result (Integer, Type, Constraints)
constraints env fresh tm = case tm of

    Lam _ x Nothing body -> do
        let tyArg = TVar fresh
        let newEnv = M.insert x tyArg env
        (fresh', tyBody, constrBody) <- constraints newEnv (fresh + 1) body
        return (fresh', TFunc tyArg tyBody, constrBody)

    Lam _ x (Just tyX) body -> do
        let newEnv = M.insert x tyX env
        (fresh', tyBody, constrBody) <- constraints newEnv fresh body
        return (fresh', TFunc tyX tyBody, constrBody)

    Var _ x -> case M.lookup x env of
        Just ty -> return (fresh, ty, S.empty)
        Nothing -> Error $ "Unbound variable '" ++ x ++ "' found."

    App _ func arg -> do
        let tyRet = TVar fresh
        (fresh'  , tyFunc, constrFunc) <- constraints env (fresh + 1) func
        (fresh'' , tyArg , constrArg ) <- constraints env  fresh'     arg
        let allConstrs = S.insert (tyFunc, TFunc tyArg tyRet)
                $ S.union constrFunc constrArg
        return (fresh'', tyRet, allConstrs)

    Fix _ f -> do
        let tyFixF = TVar fresh
        (fresh', tyF, constrF) <- constraints env (fresh + 1) f
        let allConstrs = S.insert (tyF, TFunc tyFixF tyFixF) constrF
        return (fresh', tyFixF, allConstrs)

    Cond _ gd tbr fbr -> do
        (fresh'  , tyGd , constrGd ) <- constraints env fresh   gd
        (fresh'' , tyTbr, constrTbr) <- constraints env fresh'  tbr
        (fresh''', tyFbr, constrFbr) <- constraints env fresh'' fbr
        let allConstrs = S.insert (tyGd , TTree)
                $ S.insert (tyTbr, tyFbr)
                $ S.unions [constrGd, constrTbr, constrFbr]
        return (fresh''', tyTbr, allConstrs)

    Cons _ l r -> do
        (fresh' , tyL, constrL) <- constraints env fresh  l
        (fresh'', tyR, constrR) <- constraints env fresh' r
        let allConstrs = S.insert (tyL, TTree)
                $ S.insert (tyR, TTree)
                $ S.union constrL constrR
        return (fresh'', TTree, allConstrs)

    Hd _ e -> do
        (fresh', tyE, constrE) <- constraints env fresh e
        return (fresh', TTree, S.insert (tyE, TTree) constrE)

    Tl _ e -> do
        (fresh', tyE, constrE) <- constraints env fresh e
        return (fresh', TTree, S.insert (tyE, TTree) constrE)

    Nil _ -> return (fresh, TTree, S.empty)
