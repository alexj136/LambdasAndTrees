module SugarSyntax where

import Util
import Types
import qualified Syntax as P

import Control.Monad (liftM, liftM2, liftM3)
import qualified Data.Map as M

data Term
    = Lam  Info String (Maybe Type) Term
    | Var  Info String
    | App  Info Term Term
    | Fix  Info Term
    | Cond Info Term Term Term
    | Cons Info Term Term
    | Hd   Info Term
    | Tl   Info Term
    | Nil  Info
    deriving (Eq, Ord)

data Info = Pos PosInfo | NoInfo deriving (Show, Eq, Ord)
data PosInfo = PosInfo
    { startRow :: Int
    , startCol :: Int
    , endRow   :: Int
    , endCol   :: Int
    } deriving (Show, Eq, Ord)

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
        Fix  _ f            -> "(Y " ++ (pp (indent + 1) f) ++ ")"
        Cond _ g t f        ->
            "if " ++ show g ++ " then\n"
            ++ tab (indent + 1) ++ (pp (indent + 1) t) ++ "\n"
            ++ tab  indent      ++ "else \n"
            ++ tab (indent + 1) ++ (pp (indent + 1) f) ++ "\n"
            ++ tab  indent      ++ "end"
        Cons _ l r          ->
            "(" ++ (pp (indent + 1) l) ++ "." ++ (pp (indent + 1) r) ++ ")"
        Hd   _ t             -> "(< " ++ (pp (indent + 1) t) ++ ")"
        Tl   _ t             -> "(> " ++ (pp (indent + 1) t) ++ ")"
        Nil  _               -> "nil"

-- Desugar a Term. Wrapper around a worker that passes a map with name bindings.
desugar :: Term -> Result P.Term
desugar = let
    desug :: M.Map String Integer -> Term -> Result P.Term
    desug ns t = case t of
        Lam  _ n _ b -> liftM P.Lam (desug (M.insert n 0 (M.map (+1) ns)) b)
        Var  _ n     -> case M.lookup n ns of
            Just x  -> return $ P.Var x
            Nothing -> Error $ "Unbound variable '" ++ n ++ "'."
        App  _ f a   -> liftM2 P.App  (desug ns f) (desug ns a)
        Fix  _ t     -> liftM  P.Fix  (desug ns t)
        Cond _ g t f -> liftM3 P.Cond (desug ns g) (desug ns t) (desug ns f)
        Cons _ l r   -> liftM2 P.Cons (desug ns l) (desug ns r)
        Hd   _ t     -> liftM  P.Hd   (desug ns t)
        Tl   _ t     -> liftM  P.Tl   (desug ns t)
        Nil  _       -> return P.Nil
    in desug M.empty
