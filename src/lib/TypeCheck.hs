module TypeCheck where

import Util
import Types
import SugarSyntax

import Data.Maybe (fromMaybe)
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
        let allConstrs = S.insert (Constraint (tyFunc, TFunc tyArg tyRet, tm))
                $ S.union constrFunc constrArg
        return (fresh'', tyRet, allConstrs)

    Fix _ f -> do
        let tyFixF = TVar fresh
        (fresh', tyF, constrF) <- constraints env (fresh + 1) f
        let allConstrs = S.insert (Constraint (tyF, TFunc tyFixF tyFixF, f)) constrF
        return (fresh', tyFixF, allConstrs)

    Cond _ gd tbr fbr -> do
        (fresh'  , tyGd , constrGd ) <- constraints env fresh   gd
        (fresh'' , tyTbr, constrTbr) <- constraints env fresh'  tbr
        (fresh''', tyFbr, constrFbr) <- constraints env fresh'' fbr
        let allConstrs = S.insert (Constraint (tyGd , TTree, gd))
                $ S.insert (Constraint (tyTbr, tyFbr, tm))
                $ S.unions [constrGd, constrTbr, constrFbr]
        return (fresh''', tyTbr, allConstrs)

    Cons _ l r -> do
        (fresh' , tyL, constrL) <- constraints env fresh  l
        (fresh'', tyR, constrR) <- constraints env fresh' r
        let allConstrs = S.insert (Constraint (tyL, TTree, l))
                $ S.insert (Constraint (tyR, TTree, r))
                $ S.union constrL constrR
        return (fresh'', TTree, allConstrs)

    Hd _ e -> do
        (fresh', tyE, constrE) <- constraints env fresh e
        return (fresh', TTree, S.insert (Constraint (tyE, TTree, e)) constrE)

    Tl _ e -> do
        (fresh', tyE, constrE) <- constraints env fresh e
        return (fresh', TTree, S.insert (Constraint (tyE, TTree, e)) constrE)

    Nil _ -> return (fresh, TTree, S.empty)
