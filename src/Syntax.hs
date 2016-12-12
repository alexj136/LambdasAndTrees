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
    deriving (Eq, Ord)

instance Show Term where
    show (Lam  m    ) = "(| -> " ++ show m ++ ")"
    show (Var  x    ) = "v" ++ show x
    show (App  m n  ) = "(" ++ show m ++ " " ++ show n ++ ")"
    show (Cond g t f) = "if " ++ show g ++ " then " ++ show t ++ " else "
        ++ show f ++ " end"
    show (Cons l r  ) = "(" ++ show l ++ "." ++ show r ++ ")"
    show (Hd t      ) = "< " ++ show t
    show (Tl t      ) = "> " ++ show t
    show  Nil         = "nil"
