{
module Parser where

import Util
import Lexer
import SugarSyntax
import Types

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
    Bar     { ( TK_Bar      , _ , _ ) }
    Arrow   { ( TK_Arrow    , _ , _ ) }
    If      { ( TK_If       , _ , _ ) }
    Then    { ( TK_Then     , _ , _ ) }
    Else    { ( TK_Else     , _ , _ ) }
    End     { ( TK_End      , _ , _ ) }
    LParen  { ( TK_LParen   , _ , _ ) }
    RParen  { ( TK_RParen   , _ , _ ) }
    Dot     { ( TK_Dot      , _ , _ ) }
    Nil     { ( TK_Nil      , _ , _ ) }
    Hd      { ( TK_Hd       , _ , _ ) }
    Tl      { ( TK_Tl       , _ , _ ) }
    Name    { ( TK_Name     , _ , _ ) }
    At      { ( TK_At       , _ , _ ) }
    Colon   { ( TK_Colon    , _ , _ ) }
    Fix     { ( TK_Fix      , _ , _ ) }
    Error   { ( TK_Error    , _ , _ ) }
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

tsToT :: [Term] -> Term
tsToT [ ] = error "tsToT of []"
tsToT [t] = t
tsToT ts  = App undefined (tsToT (init ts)) (last ts)

parseError :: [Token] -> Result a
parseError tokens = Error $ case tokens of
    []                          -> "Reached end of file while parsing"
    ((_, AlexPn _ y x, s):rest) ->
        "Parse error on line " ++ show y ++ ", column " ++ show x ++ ": "
        ++ "'" ++ s ++ "'"

--------------------------------
--  TERM Production Handlers  --
--------------------------------

lamU :: Token -> Token -> Token -> [Term] -> Term
lamU barTk (_, _, name) dotTk body = Lam undefined name Nothing (tsToT body)

lamT :: Token -> Token -> Token -> Type -> Token -> [Term] -> Term
lamT barTk (_ ,_ ,name) colonTk ty dotTk body =
    Lam undefined name (Just ty) (tsToT body)

var :: Token -> Term
var (_, _, name) = Var undefined name

cond :: Token -> [Term] -> Token -> [Term] -> Token -> [Term] -> Token -> Term
cond ifTk gd thenTk tbr elseTk fbr endTk =
    Cond undefined (tsToT gd) (tsToT tbr) (tsToT fbr)

cons :: Token -> [Term] -> Token -> [Term] -> Token -> Term
cons lpTk l dotTk r rpTk = Cons undefined (tsToT l) (tsToT r)

hd :: Token -> [Term] -> Term
hd hdTk body = Hd undefined (tsToT body)

tl :: Token -> [Term] -> Term
tl tlTk body = Tl undefined (tsToT body)

nil :: Token -> Term
nil nilTk = Nil undefined

parens :: Token -> [Term] -> Token -> Term
parens lpTk body rpTk = tsToT body

fix :: Token -> [Term] -> Term
fix fixTk body = Fix undefined (tsToT body)
}
