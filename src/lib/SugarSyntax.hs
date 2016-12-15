module SugarSyntax where

import Util
import Types
import qualified Syntax as P

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
    show (Lam  x Nothing  m) = "(| " ++ x ++ " . " ++ show m ++ ")"
    show (Lam  x (Just t) m) = "(| " ++ x ++ ": " ++ show t ++ " . " ++ show m
        ++ ")"
    show (Var  x           ) = x
    show (App  m n         ) = "(" ++ show m ++ " " ++ show n ++ ")"
    show (Fix  f           ) = "(Y " ++ show f ++ ")"
    show (Cond g t f       ) = "if " ++ show g ++ " then " ++ show t ++ " else "
        ++ show f ++ " end"
    show (Cons l r         ) = "(" ++ show l ++ "." ++ show r ++ ")"
    show (Hd t             ) = "< " ++ show t
    show (Tl t             ) = "> " ++ show t
    show  Nil                = "nil"

desugar :: Term -> Result P.Term
desugar = let
    doDesugar :: M.Map String Integer -> Term -> Result P.Term
    doDesugar ns t = case t of
        Lam name ty body -> do
            pBody <- doDesugar (M.insert name 0 (M.map (+1) ns)) body
            return $ P.Lam pBody
        Var name -> case M.lookup name ns of
            Just x  -> return $ P.Var x
            Nothing -> Error $ "Unbound variable '" ++ name ++ "'."
        App func arg -> do
            pFunc <- doDesugar ns func
            pArg  <- doDesugar ns arg
            return $ P.App pFunc pArg
        Fix f -> do
            pF <- doDesugar ns f
            return $ P.Fix pF
        Cond gd tbr fbr -> do
            pGd  <- doDesugar ns gd
            pTbr <- doDesugar ns tbr
            pFbr <- doDesugar ns fbr
            return $ P.Cond pGd pTbr pFbr
        Cons l r -> do
            pL <- doDesugar ns l
            pR <- doDesugar ns r
            return $ P.Cons pL pR
        Hd t -> do
            pT <- doDesugar ns t
            return $ P.Hd pT
        Tl t -> do
            pT <- doDesugar ns t
            return $ P.Tl pT
        Nil -> return P.Nil
    in doDesugar M.empty
