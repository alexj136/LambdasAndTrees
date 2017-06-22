{
module Parser where

import Util
import Lexer
import SugarSyntax
import Types

import Prelude hiding (span)
import qualified Data.Map as M
import Control.Monad (liftM, ap)
import Control.Monad.Except (throwError)
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
    Bar     { Token ( _ , TK_Bar    ) }
    Arrow   { Token ( _ , TK_Arrow  ) }
    If      { Token ( _ , TK_If     ) }
    Then    { Token ( _ , TK_Then   ) }
    Else    { Token ( _ , TK_Else   ) }
    End     { Token ( _ , TK_End    ) }
    Let     { Token ( _ , TK_Let    ) }
    Eq      { Token ( _ , TK_Eq     ) }
    In      { Token ( _ , TK_In     ) }
    Fix     { Token ( _ , TK_Fix    ) }
    LParen  { Token ( _ , TK_LParen ) }
    RParen  { Token ( _ , TK_RParen ) }
    Dot     { Token ( _ , TK_Dot    ) }
    Nil     { Token ( _ , TK_Nil    ) }
    Hd      { Token ( _ , TK_Hd     ) }
    Tl      { Token ( _ , TK_Tl     ) }
    Name    { Token ( _ , TK_Name _ ) }
    At      { Token ( _ , TK_At     ) }
    Colon   { Token ( _ , TK_Colon  ) }
    Error   { Token ( _ , TK_Error  ) }
%%

TERM :: { Term }
TERM
    : Bar Name          Dot TS  %prec ulam { lamU   $1 $2 $3 $4          }
    | Bar Name Colon TY Dot TS  %prec llam { lamT   $1 $2 $3 $4 $5 $6    }
    | Name                      %prec var  { var    $1                   }
    | If TS Then TS Else TS End %prec cond { cond   $1 $2 $3 $4 $5 $6 $7 }
    | Let Name Eq TS In TS                 { lt     $1 $2 $3 $4 $5 $6    }
    | Fix                                  { fix    $1                   }
    | LParen TS Dot TS RParen   %prec cons { cons   $1 $2 $3 $4 $5       }
    | Hd TS                                { hd     $1 $2                }
    | Tl TS                                { tl     $1 $2                }
    | Nil                                  { nil    $1                   }
    | LParen TS RParen                     { parens $1 $2 $3             }

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
parseError tokens = throwError $ case tokens of
    []                     -> "Reached end of file while parsing"
    (Token (pos, tk):rest) ->
        "Parse error: " ++ showMPos pos

------------------------------------------------------------------
--  TM Production Handlers - thread position info through terms --
------------------------------------------------------------------

lamU :: Token -> Token -> Token -> [Term] -> Term
lamU barTk (Token (_, TK_Name name)) dotTk body =
    Lam (maybe NoInfo PosInfo (barTk `span` body)) name Nothing (tsToT body)

lamT :: Token -> Token -> Token -> Type -> Token -> [Term] -> Term
lamT barTk (Token (_, TK_Name name)) colonTk ty dotTk body =
    Lam (maybe NoInfo PosInfo (barTk `span` body)) name (Just ty) (tsToT body)

var :: Token -> Term
var varTk@(Token (pos, TK_Name name)) = Var (maybe NoInfo PosInfo pos) name

lt :: Token -> Token -> Token -> [Term] -> Token -> [Term] -> Term
lt letTk (Token (_, TK_Name name)) eqTk def inTk body =
    Let (maybe NoInfo PosInfo (letTk `span` body)) name (tsToT def) (tsToT body)

fix :: Token -> Term
fix (Token (pos, TK_Fix)) = Fix (maybe NoInfo PosInfo pos)

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
nil (Token (pos, TK_Nil)) = Nil $ maybe NoInfo PosInfo pos

parens :: Token -> [Term] -> Token -> Term
parens lpTk body rpTk = tsToT body

tsToT :: [Term] -> Term
tsToT [ ] = error "tsToT of []"
tsToT [t] = t
tsToT ts  = App (maybe NoInfo PosInfo (getPos ts)) (tsToT (init ts)) (last ts)
}
