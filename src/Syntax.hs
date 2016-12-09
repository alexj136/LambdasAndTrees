module Syntax where

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
