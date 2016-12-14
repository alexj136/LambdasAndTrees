module Types where

data Type = TTree | TFunc Type Type | TVar Integer deriving (Eq, Ord)

instance Show Type where
    show t = case t of
        TTree               -> "@"
        TVar   x            -> show x
        TFunc  TTree   tR   -> "@ => " ++ show tR
        TFunc (TVar x) tR   -> show x ++ " => " ++ show tR
        TFunc tL    TTree   -> "(" ++ show tL ++ ") => @"
        TFunc tL   (TVar x) -> "(" ++ show tL ++ ") => " ++ show x
        TFunc tL    tR      -> "(" ++ show tL ++ ") => (" ++ show tR ++ ")"
