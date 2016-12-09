{
module Parser where

import Lexer
import Syntax
import Interpreter (shift)

import qualified Data.Map as M
}

%name parse TERM

%tokentype { (Token, AlexPosn) }
%error     { parseError        }

%token
    def     { ( TK_Def    , _ ) }
    arrow   { ( TK_Arrow  , _ ) }
    end     { ( TK_End    , _ ) }
    if_     { ( TK_If     , _ ) }
    then_   { ( TK_Then   , _ ) }
    else_   { ( TK_Else   , _ ) }
    nil     { ( TK_Nil    , _ ) }
    dot     { ( TK_Dot    , _ ) }
    hd      { ( TK_Hd     , _ ) }
    tl      { ( TK_Tl     , _ ) }
    name    { ( TK_Name $$, _ ) }
%%

TERM :: { Term }
TERM
    : name arrow TERM                    { Lam $3        }
    | name                               { Var 0         }
    | TERM TERM                          { App $1 $2     }
    | if_ TERM then_ TERM else_ TERM end { Cond $2 $4 $6 }
    | TERM dot TERM                      { Cons $1 $3    }
    | hd TERM                            { Hd $1         }
    | tl TERM                            { Tl $1         }
    | nil                                { Nil           }

{
parseError :: [Token] -> a
parseError []                       = error "Reached end of file while parsing"
parseError ((_, AlexPn _ y x):rest) =
    error $ concat ["Parse error on line ", show y, ", column ", show x,"."]
}
