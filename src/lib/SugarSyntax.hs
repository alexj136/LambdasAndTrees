module SugarSyntax where

import Util
import Types
import qualified DeBruijnSyntax as P

import Control.Monad (liftM, liftM2, liftM3)
import Control.Monad.Except (throwError)
import qualified Data.Map as M

data Term
    = Lam  Info String (Maybe Type) Term
    | Var  Info String
    | App  Info Term Term
    | Let  Info String Term Term
    | Cond Info Term Term Term
    | Cons Info Term Term
    | Hd   Info Term
    | Tl   Info Term
    | Nil  Info
    deriving (Eq, Ord)

instance Show Term where
    show = prettyPrint

instance Positionable Term where
    getPos tm = case tm of
        Lam  (PosInfo pos) _ _ _ -> Just pos
        Var  (PosInfo pos) _     -> Just pos
        App  (PosInfo pos) _ _   -> Just pos
        Let  (PosInfo pos) _ _ _ -> Just pos
        Cond (PosInfo pos) _ _ _ -> Just pos
        Cons (PosInfo pos) _ _   -> Just pos
        Hd   (PosInfo pos) _     -> Just pos
        Tl   (PosInfo pos) _     -> Just pos
        Nil  (PosInfo pos)       -> Just pos
        _ -> Nothing

data Info = PosInfo Pos | NoInfo deriving Ord

instance Show Info where
    show NoInfo      = "-"
    show (PosInfo i) = show i

instance Eq Info where
    i1 == i2 = True

infoFromPositionable :: Positionable a => a -> Info
infoFromPositionable = maybe NoInfo PosInfo . getPos

-- Convert the indentation-level to a string
tab :: Integer -> String
tab 0 = ""
tab n = "  " ++ tab (n - 1)

prettyPrint :: Term -> String
prettyPrint = pp 0 where
    pp :: Integer -> Term -> String
    pp indent term = case term of
        Lam  _ x Nothing  m ->
            "(| " ++ x ++ " . \n"
            ++ tab (indent + 1) ++ (pp (indent + 1) m) ++ "\n"
            ++ tab indent ++ ")"
        Lam  _ x (Just t) m ->
            "(| " ++ x ++ ": " ++ show t ++ " . \n"
            ++ tab (indent + 1) ++ (pp (indent + 1) m) ++ "\n"
            ++ tab indent ++ ")"
        Var  _ x            -> x
        App  _ m n          ->
            "(" ++ (pp (indent + 1) m) ++ " " ++ (pp (indent + 1) n) ++ ")"
        Let  _ x d b        ->
            "(let " ++ x ++ " = " ++ (pp (indent + 1) d) ++ " in \n"
            ++ tab (indent + 1) ++ (pp (indent + 1) b)
            ++ tab  indent      ++ ")"
        Cond _ g t f        ->
            "if " ++ show g ++ " then\n"
            ++ tab (indent + 1) ++ (pp (indent + 1) t) ++ "\n"
            ++ tab  indent      ++ "else \n"
            ++ tab (indent + 1) ++ (pp (indent + 1) f) ++ "\n"
            ++ tab  indent      ++ "end"
        Cons _ l r          ->
            "(" ++ (pp (indent + 1) l) ++ "." ++ (pp (indent + 1) r) ++ ")"
        Hd   _ t            -> "(< " ++ (pp (indent + 1) t) ++ ")"
        Tl   _ t            -> "(> " ++ (pp (indent + 1) t) ++ ")"
        Nil  _              -> "nil"

debugPrint :: Term -> String
debugPrint = pp 0 where
    pp :: Integer -> Term -> String
    pp indent term = case term of
        Lam  i x Nothing  m ->
            "\n" ++ tab indent ++ "| LAMBDA " ++ show i
            ++ "\n" ++ tab (indent + 1) ++ "| BINDER = " ++ x
            ++ "\n" ++ tab (indent + 1) ++ "| BODY   = "
            ++ pp (indent + 2) m
        Lam  i x (Just t) m ->
            "\n" ++ tab indent ++ "| LAMBDA " ++ show i
            ++ "\n" ++ tab (indent + 1) ++ "| BINDER = " ++ x
            ++ "\n" ++ tab (indent + 1) ++ "| TYPE   = " ++ show t
            ++ "\n" ++ tab (indent + 1) ++ "| BODY   = "
            ++ pp (indent + 2) m
        Var  i x            ->
            "\n" ++ tab indent ++ "| VARIABLE " ++ x ++ " " ++ show i
        App  i m n          ->
            "\n" ++ tab indent ++ "| APPLICATION " ++ show i
            ++ "\n" ++ tab (indent + 1) ++ "| FUNCTION = "
            ++ pp (indent + 2) m
            ++ "\n" ++ tab (indent + 1) ++ "| ARGUMENT = "
            ++ pp (indent + 2) n
        Let  i x d b        ->
            "\n" ++ tab indent ++ "| LET " ++ show i
            ++ "\n" ++ tab (indent + 1) ++ "| BINDER = " ++ x
            ++ "\n" ++ tab (indent + 1) ++ "| DEFINITION = "
            ++ pp (indent + 2) d
            ++ "\n" ++ tab (indent + 1) ++ "| SCOPE = "
            ++ pp (indent + 2) b
        Cond i g t f        ->
            "\n" ++ tab indent ++ "| CONDITIONAL " ++ show i
            ++ "\n" ++ tab (indent + 1) ++ "| GUARD       = "
            ++ pp (indent + 2) g
            ++ "\n" ++ tab (indent + 1) ++ "| TRUEBRANCH  = "
            ++ pp (indent + 2) t
            ++ "\n" ++ tab (indent + 1) ++ "| FALSEBRANCH = "
            ++ pp (indent + 2) f
        Cons i l r          ->
            "\n" ++ tab indent ++ "| CONS " ++ show i
            ++ "\n" ++ tab (indent + 1) ++ "| LEFT  = "
            ++ pp (indent + 2) l
            ++ "\n" ++ tab (indent + 1) ++ "| RIGHT = "
            ++ pp (indent + 2) r
        Hd   i t             ->
            "\n" ++ tab indent ++ "| HEAD " ++ show i
            ++ "\n" ++ tab (indent + 1) ++ "| ARGUMENT = "
            ++ pp (indent + 2) t
        Tl   i t             ->
            "\n" ++ tab indent ++ "| TAIL " ++ show i
            ++ "\n" ++ tab (indent + 1) ++ "| ARGUMENT = "
            ++ pp (indent + 2) t
        Nil  i               ->
            "\n" ++ tab indent ++ "| NIL " ++ show i

-- Desugar a Term, i.e. convert it to De Bruijn syntax. Wrapper around a worker
-- that passes a map with name bindings.
desugar :: Term -> Result P.Term
desugar = let
    desug :: M.Map String Integer -> Term -> Result P.Term
    desug ns t = case t of
        Lam  _ n _ b -> liftM P.Lam (desug (M.insert n 0 (M.map (+1) ns)) b)
        Var  _ n     -> case M.lookup n ns of
            Just x  -> return $ P.Var x
            Nothing -> throwError $ "Unbound variable '" ++ n ++ "'."
        App  _ f a   -> liftM2 P.App  (desug ns f) (desug ns a)
        Let  i x d b ->
            liftM2 P.App (liftM P.Fix (desug ns (Lam i x Nothing b))) (desug ns d)
        Cond _ g t f -> liftM3 P.Cond (desug ns g) (desug ns t) (desug ns f)
        Cons _ l r   -> liftM2 P.Cons (desug ns l) (desug ns r)
        Hd   _ t     -> liftM  P.Hd   (desug ns t)
        Tl   _ t     -> liftM  P.Tl   (desug ns t)
        Nil  _       -> return P.Nil
    in desug M.empty
