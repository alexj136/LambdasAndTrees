module CodeGen where

-- Code in this module assumes that given terms are well typed. Run them through
-- TypeCheck.check before invoking code here.
--
-- In order to generate code, we transform the SugarSyntax Term in several
-- phases:
--     1) Removal of the Y combinator from a SugarSyntax.Term, resulting in a
--        NoYTm. At this phase we also drop any source position information and
--        type labels.
--     2) Elimination of free variables under all lambdas (NoYTm -> NoFrTm).
--        NoFrTms are not allowed to have any free variables under a given
--        lambda. Lambdas can have multiple arguments here.
--     3) Lambda lifting from a NoFrTm to an LTerm, where all functions are
--        named global functions, not lambdas.

import qualified SugarSyntax as Z

import qualified Data.Map as M
import qualified Data.Set as S

-------------------------------------------
-- NoYTm definition                    --
-------------------------------------------

data NoYTm
    = NoYLam  String NoYTm
    | NoYVar  String
    | NoYApp  NoYTm  NoYTm
    | NoYCond NoYTm  NoYTm NoYTm
    | NoYCons NoYTm  NoYTm
    | NoYHd   NoYTm
    | NoYTl   NoYTm
    | NoYNil
    deriving (Show, Eq, Ord)

-- Remove all occurences of the Y-combinator from a SugarSyntax Term. Replace
-- them with a pure-lambda version of the Y-combinator. This is done after type
-- checking as the replacement term is untypable.
unY :: Z.Term -> NoYTm
unY tm = case tm of
    Z.Lam  _ x _ m -> NoYLam  x (unY m)
    Z.Var  _ x     -> NoYVar  x
    Z.App  _ m n   -> NoYApp  (unY m) (unY n)
    Z.Fix  _ f     -> NoYApp  noYFix (unY f)
    Z.Cond _ g t f -> NoYCond (unY g) (unY t) (unY f)
    Z.Cons _ l r   -> NoYCons (unY l) (unY r)
    Z.Hd   _ t     -> NoYHd   (unY t)
    Z.Tl   _ t     -> NoYTl   (unY t)
    Z.Nil  _       -> NoYNil  

-- The Y-Combinator in pure lambdas
noYFix :: NoYTm
noYFix =
    NoYLam "f" (
        NoYApp (
            NoYLam "x" (NoYApp (NoYVar "f") (NoYApp (NoYVar "x") (NoYVar "x")))
        ) (
            NoYLam "x" (NoYApp (NoYVar "f") (NoYApp (NoYVar "x") (NoYVar "x")))
        )
    )

-------------------------------------------
-- NoFrTm definition                     --
-------------------------------------------

data NoFrTm
    = NoFrLam  [String] NoFrTm
    | NoFrVar   String
    | NoFrApp   NoFrTm [NoFrTm]
    | NoFrCond  NoFrTm  NoFrTm NoFrTm
    | NoFrCons  NoFrTm  NoFrTm
    | NoFrHd    NoFrTm
    | NoFrTl    NoFrTm
    | NoFrNil
    deriving (Show, Eq, Ord)

-- Determine the free variables of a NoFrTm
frees :: NoFrTm -> S.Set String
frees tm = case tm of
    NoFrLam  xs m  -> foldr S.delete (frees m) xs
    NoFrVar  x     -> S.singleton x
    NoFrApp  f as  -> frees f `S.union` S.unions (map frees as)
    NoFrCond g t f -> frees g `S.union` frees t `S.union` frees f
    NoFrCons l r   -> frees l `S.union` frees r
    NoFrHd   t     -> frees t
    NoFrTl   t     -> frees t
    NoFrNil        -> S.empty

elimFrees :: NoYTm -> NoFrTm
elimFrees tm = case tm of
    NoYVar  x     -> NoFrVar   x
    NoYApp  m n   -> NoFrApp  (elimFrees m) [elimFrees n]
    NoYCond g t f -> NoFrCond (elimFrees g) (elimFrees t) (elimFrees f)
    NoYCons l r   -> NoFrCons (elimFrees l) (elimFrees r)
    NoYHd   t     -> NoFrHd $  elimFrees t
    NoYTl   t     -> NoFrTl $  elimFrees t
    NoYNil        -> NoFrNil
    NoYLam  x m   ->
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
    | LVar   String
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
