module Interpreter where

import Syntax

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
