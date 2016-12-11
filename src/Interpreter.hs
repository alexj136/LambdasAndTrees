module Interpreter where

import Util
import Syntax

eval :: Term -> Result Term
eval tm = case tm of

    Lam _ -> Error "Implementation error - unapplied Lam reached eval"

    Var _ -> Error "Implementation error - free Var reached eval"

    App (Lam body) (arg) -> eval (shift (-1) 0 (sub 0 (shift 1 0 arg) body))

    App nonLam arg -> do
        evNonLam <- eval nonLam
        eval $ App evNonLam arg

    Cond gd tbr fbr -> do
        evGd <- eval gd
        eval $ if evGd /= Nil then tbr else fbr

    Cons h t -> do
        evH <- eval h
        evT <- eval t
        return $ Cons evH evT

    Hd e -> do
        evE <- eval e
        case evE of
            Cons h _ -> return h
            Nil      -> Error "< nil"

    Tl e -> do
        evE <- eval e
        case evE of
            Cons _ t -> return t
            Nil      -> Error "< nil"

    Nil -> return Nil

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
