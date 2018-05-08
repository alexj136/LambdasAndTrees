module DeBruijnSyntax where

data Term
    = Lam Term
    | Var Integer
    | App Term Term
    | Fix
    | Cond Term Term Term
    | Cons Term Term
    | Hd
    | Tl
    | Nil
    deriving (Eq, Ord)

instance Show Term where
    show (Lam  m    ) = "(| . " ++ show m ++ ")"
    show (Var  x    ) = "v" ++ show x
    show (App  m n  ) = "(" ++ show m ++ " " ++ show n ++ ")"
    show  Fix         = "fix"
    show (Cond g t f) = "if " ++ show g ++ " then " ++ show t ++ " else "
        ++ show f ++ " end"
    show (Cons l r  ) = "(" ++ show l ++ "." ++ show r ++ ")"
    show  Hd          = "<"
    show  Tl          = ">"
    show  Nil         = "nil"
