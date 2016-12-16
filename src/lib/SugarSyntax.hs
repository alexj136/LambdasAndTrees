module SugarSyntax where

import Util
import Types
import qualified Syntax as P

import Control.Monad (liftM, liftM2, liftM3)
import qualified Data.Map as M

data Term
    = Lam String (Maybe Type) Term
    | Var String
    | App Term Term
    | Fix Term
    | Cond Term Term Term
    | Cons Term Term
    | Hd Term
    | Tl Term
    | Nil
    deriving (Eq, Ord)

instance Show Term where
    show = prettyPrint

-- Pretty-print
prettyPrint :: Term -> String
prettyPrint = pp 0 where

    -- Convert the indentation-level to a string
    tab :: Integer -> String
    tab 0 = ""
    tab n = "    " ++ tab (n - 1)

    -- pretty-print a term at a specified indentaton level
    pp :: Integer -> Term -> String
    pp indent term = case term of
        Lam  x Nothing  m ->
            "(| " ++ x ++ " . \n"
            ++ tab (indent + 1) ++ (pp (indent + 1) m) ++ "\n"
            ++ tab indent ++ ")"
        Lam  x (Just t) m ->
            "(| " ++ x ++ ": " ++ show t ++ " . \n"
            ++ tab (indent + 1) ++ (pp (indent + 1) m) ++ "\n"
            ++ tab indent ++ ")"
        Var  x            -> x
        App  m n          ->
            "(" ++ (pp (indent + 1) m) ++ " " ++ (pp (indent + 1) n) ++ ")"
        Fix  f            -> "(Y " ++ (pp (indent + 1) f) ++ ")"
        Cond g t f        ->
            "if " ++ show g ++ " then\n"
            ++ tab (indent + 1) ++ (pp (indent + 1) t) ++ "\n"
            ++ tab  indent      ++ "else \n"
            ++ tab (indent + 1) ++ (pp (indent + 1) f) ++ "\n"
            ++ tab  indent      ++ "end"
        Cons l r          ->
            "(" ++ (pp (indent + 1) l) ++ "." ++ (pp (indent + 1) r) ++ ")"
        Hd t              -> "< " ++ (pp (indent + 1) t)
        Tl t              -> "> " ++ (pp (indent + 1) t)
        Nil               -> "nil"

-- Desugar a Term. Wrapper around a worker that passes a map with name bindings.
desugar :: Term -> Result P.Term
desugar = let
    desug :: M.Map String Integer -> Term -> Result P.Term
    desug ns t = case t of
        Lam  n _ b -> liftM P.Lam (desug (M.insert n 0 (M.map (+1) ns)) b)
        Var  n     -> case M.lookup n ns of
            Just x  -> return $ P.Var x
            Nothing -> Error $ "Unbound variable '" ++ n ++ "'."
        App  f a   -> liftM2 P.App  (desug ns f) (desug ns a)
        Fix  t     -> liftM  P.Fix  (desug ns t)
        Cond g t f -> liftM3 P.Cond (desug ns g) (desug ns t) (desug ns f)
        Cons l r   -> liftM2 P.Cons (desug ns l) (desug ns r)
        Hd   t     -> liftM  P.Hd   (desug ns t)
        Tl   t     -> liftM  P.Tl   (desug ns t)
        Nil        -> return P.Nil
    in desug M.empty
