{
module Parser where

import Util
import Lexer
import SugarSyntax
import Types

import Prelude hiding (span)
import qualified Data.Map as M
import Control.Monad (liftM, ap)
}

%monad { Result }
%name parseTS TS
%name parseTy TY

%left app Name var Hd Tl Fix
%right cons Dot Arrow At
%nonassoc Bar Nil cond If Then Else End LParen RParen ulam llam Def

%tokentype { Token      }
%error     { parseError }

%token
    Bar     { Token ( TK_Bar      , _ , _ ) }
    Arrow   { Token ( TK_Arrow    , _ , _ ) }
    If      { Token ( TK_If       , _ , _ ) }
    Then    { Token ( TK_Then     , _ , _ ) }
    Else    { Token ( TK_Else     , _ , _ ) }
    End     { Token ( TK_End      , _ , _ ) }
    LParen  { Token ( TK_LParen   , _ , _ ) }
    RParen  { Token ( TK_RParen   , _ , _ ) }
    Dot     { Token ( TK_Dot      , _ , _ ) }
    Nil     { Token ( TK_Nil      , _ , _ ) }
    Hd      { Token ( TK_Hd       , _ , _ ) }
    Tl      { Token ( TK_Tl       , _ , _ ) }
    Name    { Token ( TK_Name     , _ , _ ) }
    At      { Token ( TK_At       , _ , _ ) }
    Colon   { Token ( TK_Colon    , _ , _ ) }
    Fix     { Token ( TK_Fix      , _ , _ ) }
    Error   { Token ( TK_Error    , _ , _ ) }
%%

TERM :: { Term }
TERM
    : Bar Name          Dot TS  %prec ulam { lamU   $1 $2 $3 $4          }
    | Bar Name Colon TY Dot TS  %prec llam { lamT   $1 $2 $3 $4 $5 $6    }
    | Name                      %prec var  { var    $1                   }
    | If TS Then TS Else TS End %prec cond { cond   $1 $2 $3 $4 $5 $6 $7 }
    | LParen TS Dot TS RParen   %prec cons { cons   $1 $2 $3 $4 $5       }
    | Hd TS                                { hd     $1 $2                }
    | Tl TS                                { tl     $1 $2                }
    | Nil                                  { nil    $1                   }
    | LParen TS RParen                     { parens $1 $2 $3             }
    | Fix TS                               { fix    $1 $2                }

TS :: { [Term] }
TS : TERM TS { $1 : $2 } | TERM { [$1] }

TY :: { Type }
TY
    : TY Arrow TY      { TFunc $1 $3 }
    | At               { TTree       }
    | LParen TY RParen { $2          }

{
parse :: [Token] -> Result Term
parse = (fmap tsToT) . parseTS

parseError :: [Token] -> Result a
parseError tokens = Error $ case tokens of
    []                          -> "Reached end of file while parsing"
    (Token (_, AlexPn _ r c, s):rest) ->
        "Parse error on line " ++ show r ++ ", column " ++ show c ++ ": "
        ++ "'" ++ s ++ "'"

--------------------------------------------------------------------
--  TERM Production Handlers - thread position info through terms --
--------------------------------------------------------------------

lamU :: Token -> Token -> Token -> [Term] -> Term
lamU barTk (Token (_, _, name)) dotTk body =
    Lam (maybe NoInfo PosInfo (barTk `span` body)) name Nothing (tsToT body)

lamT :: Token -> Token -> Token -> Type -> Token -> [Term] -> Term
lamT barTk (Token (_ ,_ ,name)) colonTk ty dotTk body =
    Lam (maybe NoInfo PosInfo (barTk `span` body)) name (Just ty) (tsToT body)

var :: Token -> Term
var varTk@(Token (_, (AlexPn _ r c), name)) =
    Var (PosInfo (Pos r c r (c + length name))) name

cond :: Token -> [Term] -> Token -> [Term] -> Token -> [Term] -> Token -> Term
cond ifTk gd thenTk tbr elseTk fbr endTk =
    Cond (maybe NoInfo PosInfo (ifTk `span` endTk))
        (tsToT gd) (tsToT tbr) (tsToT fbr)

cons :: Token -> [Term] -> Token -> [Term] -> Token -> Term
cons lpTk l dotTk r rpTk =
    Cons (maybe NoInfo PosInfo (lpTk `span` rpTk)) (tsToT l) (tsToT r)

hd :: Token -> [Term] -> Term
hd hdTk body = Hd (maybe NoInfo PosInfo (hdTk `span` body)) (tsToT body)

tl :: Token -> [Term] -> Term
tl tlTk body = Tl (maybe NoInfo PosInfo (tlTk `span` body)) (tsToT body)

nil :: Token -> Term
nil (Token (_, AlexPn _ r c, s)) = Nil $ PosInfo $ Pos r c r (c + length s)

parens :: Token -> [Term] -> Token -> Term
parens lpTk body rpTk = tsToT body

fix :: Token -> [Term] -> Term
fix fixTk body = Fix (maybe NoInfo PosInfo (fixTk `span` body)) (tsToT body)

tsToT :: [Term] -> Term
tsToT [ ] = error "tsToT of []"
tsToT [t] = t
tsToT ts  = App (maybe NoInfo PosInfo (getPos ts)) (tsToT (init ts)) (last ts)
}
