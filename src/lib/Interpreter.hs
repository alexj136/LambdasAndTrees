module Interpreter where

import Util
import DeBruijnSyntax

import Control.Monad.Except (throwError)

eval :: Term -> Result Term
eval tm = case tm of

    Lam _ -> return tm

    Var _ -> throwBasic "Implementation error - free Var reached eval"

    App (Lam body) (arg) -> eval (shift (-1) 0 (sub 0 (shift 1 0 arg) body))

    App Fix (Lam body) -> eval (shift (-1) 0 (sub 0 (App Fix (Lam body)) body))

    App Fix body -> do
        evalBody <- eval body
        eval $ App Fix evalBody

    App Hd e -> do
        evE <- eval e
        case evE of
            Cons h _ -> return h
            Nil      -> throwBasic "< nil"

    App Tl e -> do
        evE <- eval e
        case evE of
            Cons _ t -> return t
            Nil      -> throwBasic "> nil"

    App m n -> do
        eM <- eval m
        eN <- eval n
        eval $ App eM eN

    Cond gd tbr fbr -> do
        evGd <- eval gd
        eval $ if evGd /= Nil then tbr else fbr

    Cons h t -> do
        evH <- eval h
        evT <- eval t
        return $ Cons evH evT

    Nil -> return Nil

sub :: Integer -> Term -> Term -> Term
sub x arg body = let subAgain = sub x arg in case body of
    Lam body        -> Lam (sub (x + 1) (shift 1 0 arg) body)
    Var y           -> if x == y then arg else Var y
    App func arg    -> App  (subAgain func) (subAgain arg)
    Fix             -> Fix
    Cond gd tbr fbr -> Cond (subAgain gd  ) (subAgain tbr) (subAgain fbr)
    Cons l r        -> Cons (subAgain l   ) (subAgain r  )
    Hd              -> Hd
    Tl              -> Tl
    Nil             -> Nil

shift :: Integer -> Integer -> Term -> Term
shift places cutoff tm = let shiftAgain = shift places cutoff in case tm of
    Lam body        -> Lam  (shift places (cutoff + 1) body)
    Var x           -> Var  (if x < cutoff then x else x + places)
    App func arg    -> App  (shiftAgain func) (shiftAgain arg)
    Fix             -> Fix
    Cond gd tbr fbr -> Cond (shiftAgain gd  ) (shiftAgain tbr) (shiftAgain fbr)
    Cons l r        -> Cons (shiftAgain l   ) (shiftAgain r  )
    Hd              -> Hd
    Tl              -> Tl
    Nil             -> Nil
