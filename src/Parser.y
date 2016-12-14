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

%left app Name var Hd Tl
%right cons Dot
%nonassoc Bar Nil cond If Then Else End LParen RParen ulam llam Def Arrow

%tokentype { (Token, AlexPosn, String) }
%error     { parseError                }

%token
    Bar     { ( TK_Bar      , _ , _  ) }
    Arrow   { ( TK_Arrow    , _ , _  ) }
    If      { ( TK_If       , _ , _  ) }
    Then    { ( TK_Then     , _ , _  ) }
    Else    { ( TK_Else     , _ , _  ) }
    End     { ( TK_End      , _ , _  ) }
    LParen  { ( TK_LParen   , _ , _  ) }
    RParen  { ( TK_RParen   , _ , _  ) }
    Dot     { ( TK_Dot      , _ , _  ) }
    Nil     { ( TK_Nil      , _ , _  ) }
    Hd      { ( TK_Hd       , _ , _  ) }
    Tl      { ( TK_Tl       , _ , _  ) }
    Name    { ( TK_Name     , _ , $$ ) }
    At      { ( TK_At       , _ , _  ) }
    Colon   { ( TK_Colon    , _ , _  ) }
    Fix     { ( TK_Fix      , _ , _  ) }
    Error   { ( TK_Error    , _ , _  ) }
%%

TERM :: { Term }
TERM
    : Bar Name          Dot TS  %prec ulam { Lam $2 Nothing   (tsToT $4) }
    | Bar Name Colon TY Dot TS  %prec llam { Lam $2 (Just $4) (tsToT $6) }
    | Name                      %prec var  { Var $1                      }
    | If TS Then TS Else TS End %prec cond { Cond (tsToT $2) (tsToT $4) (tsToT $6) }
    | LParen TS Dot TS RParen   %prec cons { Cons (tsToT $2) (tsToT $4)  }
    | Hd TS                                { Hd $ tsToT $2               }
    | Tl TS                                { Tl $ tsToT $2               }
    | Nil                                  { Nil                         }
    | LParen TS RParen                     { tsToT $2                    }
    | Fix TS                               { Fix $ tsToT $2              }

TS :: { [Term] }
TS : TERM TS { $1 : $2 } | TERM { [$1] }

TY :: { Type }
TY
    : TY Arrow TY      { TFunc $1 $3 }
    | At               { TTree       }
    | LParen TY RParen { $2          }

{
parse :: [(Token, AlexPosn, String)] -> Result Term
parse = (fmap tsToT) . parseTS

tsToT :: [Term] -> Term
tsToT [ ] = error "tsToT of []"
tsToT [t] = t
tsToT ts  = App (tsToT (init ts)) (last ts)

parseError :: [(Token, AlexPosn, String)] -> Result a
parseError tokens = Error $ case tokens of
    []                          -> "Reached end of file while parsing"
    ((_, AlexPn _ y x, s):rest) ->
        "Parse error on line " ++ show y ++ ", column " ++ show x ++ ": "
        ++ "'" ++ s ++ "'"
}
