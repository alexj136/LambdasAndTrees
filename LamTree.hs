module LamTree where

import qualified Data.Map as M
import qualified Data.Set as S

data Term
    = Lam Term
    | Var Integer
    | App Term Term
    | Cond Term Term Term
    | Cons Term Term
    | Hd Term
    | Tl Term
    | Nil
    deriving (Show, Eq, Ord)

data ETree = ECons ETree ETree | ENil deriving (Show, Eq, Ord)

eval :: Term -> ETree
eval tm = case tm of
    Lam _ -> error "unapplied Lam"
    Var _ -> error "free Var"
    App (Lam body) (arg) -> eval (shift (-1) 0 (sub 0 (shift 1 0 arg) body))
    Cond gd tbr fbr -> eval (if eval gd == ENil then fbr else tbr)
    Cons h t        -> ECons (eval h) (eval t)
    Hd e            -> case eval e of { ECons h _ -> h ; _ -> error "bad Hd" }
    Tl e            -> case eval e of { ECons _ t -> t ; _ -> error "bad Tl" }
    Nil             -> ENil

sub :: Integer -> Term -> Term -> Term
sub x arg body = let subAgain = sub x arg in case body of
    Lam body        -> Lam (sub (x + 1) (shift 1 0 arg) body)
    Var y           -> if x == y then arg else Var y
    App func arg    -> App  (subAgain func) (subAgain arg)
    Cond gd tbr fbr -> Cond (subAgain gd  ) (subAgain tbr) (subAgain fbr)
    Cons l r        -> Cons (subAgain l   ) (subAgain r  )
    Hd e            -> Hd   (subAgain e   )
    Tl e            -> Tl   (subAgain e   )
    Nil             -> Nil

shift :: Integer -> Integer -> Term -> Term
shift places cutoff tm = let shiftAgain = shift places cutoff in case tm of
    Lam body        -> Lam  (shift places (cutoff + 1) body)
    Var x           -> Var  (if x < cutoff then x else x + places)
    App func arg    -> App  (shiftAgain func) (shiftAgain arg)
    Cond gd tbr fbr -> Cond (shiftAgain gd  ) (shiftAgain tbr) (shiftAgain fbr)
    Cons l r        -> Cons (shiftAgain l   ) (shiftAgain r  )
    Hd e            -> Hd   (shiftAgain e   )
    Tl e            -> Tl   (shiftAgain e   )
    Nil             -> Nil

data Type = TTree | TFunc Type Type | TVar Integer deriving (Eq, Ord)

instance Show Type where
    show t = case t of
        TTree               -> "@"
        TVar   x            -> show x
        TFunc  TTree   tR   -> "@ -> " ++ show tR
        TFunc (TVar x) tR   -> show x ++ " -> " ++ show tR
        TFunc tL    TTree   -> "(" ++ show tL ++ ") -> @"
        TFunc tL   (TVar x) -> "(" ++ show tL ++ ") -> " ++ show x
        TFunc tL    tR      -> "(" ++ show tL ++ ") -> (" ++ show tR ++ ")"

type Env = M.Map Integer Type
type Constraints = S.Set (Type, Type)

check :: Term -> Maybe Type
check tm = do
    let (_, ty, constrs) = constraints M.empty 0 tm
    unifyFn <- unify constrs
    return $ unifyFn ty

unify :: Constraints -> Maybe (Type -> Type)
unify constrs = if null constrs then return id else
    let (next, rest) = S.deleteFindMin constrs in case next of

        (ty1, ty2) | ty1 == ty2 -> unify rest

        (TVar x, ty) | not $ occurs x ty -> handleVar x ty rest

        (ty, TVar x) | not $ occurs x ty -> handleVar x ty rest

        (TFunc a1 b1, TFunc a2 b2) -> unify $
            S.insert (a1, a2) $ S.insert (b1, b2) $ rest

        _ -> Nothing

    where

        handleVar :: Integer -> Type -> Constraints -> Maybe (Type -> Type)
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

constraints :: Env -> Integer -> Term -> (Integer, Type, Constraints)
constraints env fresh tm = case tm of

    Lam body -> (fresh', TFunc tyArg tyBody, constrBody)
        where tyArg = TVar fresh
              newEnv = M.insert 0 tyArg $ M.mapKeys (+1) env
              (fresh', tyBody, constrBody) = constraints newEnv (fresh + 1) body

    Var x -> (fresh, env M.! x, S.empty)

    App func arg -> (fresh'', tyRet, allConstrs)
        where tyRet = TVar fresh
              (fresh'  , tyFunc, constrFunc) = constraints env (fresh + 1) func
              (fresh'' , tyArg , constrArg ) = constraints env  fresh'     arg
              allConstrs = S.insert (tyFunc, TFunc tyArg tyRet)
                         $ S.union constrFunc constrArg

    Cond gd tbr fbr -> (fresh''', tyTbr, allConstrs)
        where (fresh'  , tyGd , constrGd ) = constraints env fresh   gd
              (fresh'' , tyTbr, constrTbr) = constraints env fresh'  tbr
              (fresh''', tyFbr, constrFbr) = constraints env fresh'' fbr
              allConstrs = S.insert (tyGd , TTree)
                         $ S.insert (tyTbr, tyFbr)
                         $ S.unions [constrGd, constrTbr, constrFbr]

    Cons l r -> (fresh'', TTree, allConstrs)
        where (fresh' , tyL, constrL) = constraints env fresh  l
              (fresh'', tyR, constrR) = constraints env fresh' r
              allConstrs = S.insert (tyL, TTree)
                         $ S.insert (tyR, TTree)
                         $ S.union constrL constrR

    Hd e -> (fresh', TTree, S.insert (tyE, TTree) constrE)
        where (fresh', tyE, constrE) = constraints env fresh e

    Tl e -> (fresh', TTree, S.insert (tyE, TTree) constrE)
        where (fresh', tyE, constrE) = constraints env fresh e

    Nil -> (fresh, TTree, S.empty)
