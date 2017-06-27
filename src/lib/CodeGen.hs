module CodeGen where

-- Code in this module assumes that given terms are well typed. Run them through
-- TypeCheck.check before invoking code here.
--
-- In order to generate code, we transform the SugarSyntax Term in several
-- phases:
--     1) Elimination of free variables under all lambdas (NoYTm -> NoFrTm).
--        NoFrTms are not allowed to have any free variables under a given
--        lambda. Lambdas can have multiple arguments here.
--     2) Lambda lifting from a NoFrTm to an LTerm, where all functions are
--        named global functions, not lambdas.

import qualified SugarSyntax as Z
import Util

import qualified Data.Map as M
import qualified Data.Set as S

-------------------------------------------
-- NoFrTm definition                     --
-------------------------------------------

data NoFrTm
    = NoFrLam   [Name]  NoFrTm
    | NoFrVar    Name
    | NoFrApp   NoFrTm [NoFrTm]
    | NoFrLet   [Name]  NoFrTm NoFrTm
    | NoFrCond  NoFrTm  NoFrTm NoFrTm
    | NoFrCons  NoFrTm  NoFrTm
    | NoFrHd    NoFrTm
    | NoFrTl    NoFrTm
    | NoFrNil
    deriving (Show, Eq, Ord)

-- Determine the free variables of a NoFrTm
frees :: NoFrTm -> S.Set Name
frees tm = case tm of
    NoFrLam  xs m   -> foldr S.delete (frees m) xs
    NoFrVar  x      -> S.singleton x
    NoFrApp  f as   -> frees f `S.union` S.unions (map frees as)
    NoFrLet  xs d b -> undefined
    NoFrCond g t f  -> frees g `S.union` frees t `S.union` frees f
    NoFrCons l r    -> frees l `S.union` frees r
    NoFrHd   t      -> frees t
    NoFrTl   t      -> frees t
    NoFrNil         -> S.empty

elimFrees :: Z.Term -> NoFrTm
elimFrees tm = case tm of
    Z.Var  _ x         -> NoFrVar   x
    Z.App  _ m n       -> NoFrApp  (elimFrees m) [elimFrees n]
    Z.Cond _ g t f     -> NoFrCond (elimFrees g) (elimFrees t) (elimFrees f)
    Z.Cons _ l r       -> NoFrCons (elimFrees l) (elimFrees r)
    Z.Hd   _ t         -> NoFrHd $  elimFrees t
    Z.Tl   _ t         -> NoFrTl $  elimFrees t
    Z.Nil  _           -> NoFrNil
    Z.Let  _ _ _ _ _ _ -> undefined
    Z.Lam  _ x _ m     ->
        let noFrM   = elimFrees m
            newArgs = S.toList $ frees noFrM
        in
            NoFrApp (NoFrLam (newArgs ++ [x]) noFrM) (map NoFrVar newArgs)

-------------------------------------------
-- LProg/LTerm definition                --
-------------------------------------------

-- An LProg is a map from global function names to argument lists with function
-- bodies, also with an additional expression which is the result, and refers to
-- the function definitions.
newtype LProg = LProg (M.Map Integer ([String], LTerm), LTerm)
    deriving (Show, Eq, Ord)

-- LTerms are 'lambda-lifted' i.e. contain only global functions, no lambdas
data LTerm
    = LFnApp Integer [LTerm]
    | LVar   Name
    | LCond  LTerm    LTerm  LTerm
    | LCons  LTerm    LTerm
    | LHd    LTerm
    | LTl    LTerm
    | LNil
    deriving (Show, Eq, Ord)

liftLams :: Integer -> (M.Map Integer ([String], LTerm)) -> NoFrTm ->
    (Integer, LProg)
liftLams fresh fs tm = case tm of
    NoFrLam  xs m  -> undefined
    NoFrVar  x     -> undefined
    NoFrApp  f as  -> undefined
    NoFrCond g t f -> undefined
    NoFrCons l r   -> undefined--LCons nL nR where
    NoFrHd   t     -> undefined--LHd $ liftLams fresh t
    NoFrTl   t     -> undefined--LTl $ liftLams fresh t
    NoFrNil        -> (fresh, LProg (fs, LNil))
