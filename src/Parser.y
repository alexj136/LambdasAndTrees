{
module Parser where

import Lexer
import Syntax
import Interpreter (shift)

import qualified Data.Map as M
import Control.Monad (liftM, ap)
}

%monad { ParseResult }
%name parseTS TS

%left app Name var Hd Tl
%right cons
%nonassoc def Eq Bar Nil Dot cond If Then Else End LParen RParen lam Def Arrow

%tokentype { (Token, AlexPosn, String) }
%error     { parseError                }

%token
    Def    { ( TK_Def      , _ , _  ) }
    Eq     { ( TK_Eq       , _ , _  ) }
    Bar    { ( TK_Bar      , _ , _  ) }
    Arrow  { ( TK_Arrow    , _ , _  ) }
    LParen { ( TK_LParen   , _ , _  ) }
    RParen { ( TK_RParen   , _ , _  ) }
    End    { ( TK_End      , _ , _  ) }
    If     { ( TK_If       , _ , _  ) }
    Then   { ( TK_Then     , _ , _  ) }
    Else   { ( TK_Else     , _ , _  ) }
    Nil    { ( TK_Nil      , _ , _  ) }
    Dot    { ( TK_Dot      , _ , _  ) }
    Hd     { ( TK_Hd       , _ , _  ) }
    Tl     { ( TK_Tl       , _ , _  ) }
    Name   { ( TK_Name     , _ , $$ ) }
    Error  { ( TK_Error    , _ , _  ) }
%%

TERM :: { Term }
TERM
    : Def NAMES Eq TS           %prec def  { undefined                  }
    | Bar Name Arrow TS         %prec lam  { Lam (tsToT $4)             }
    | Name                      %prec var  { Var 0                      }
    | If TS Then TS Else TS End %prec cond { Cond (tsToT $2) (tsToT $4) (tsToT $6) }
    | TS Dot TS                 %prec cons { Cons (tsToT $1) (tsToT $3) }
    | Hd TS                                { Hd $ tsToT $2              }
    | Tl TS                                { Tl $ tsToT $2              }
    | Nil                                  { Nil                        }
    | LParen TS RParen                     { tsToT $2                   }

NAMES :: { Integer }
NAMES
    : Name NAMES { $2 + 1 }
    | Name       { 0      }

TS :: { [Term] }
TS
    : TERM TS { $1 : $2 }
    | TERM    { [$1]    }
{
data ParseResult a = ParseError String | Parsed a

instance Functor ParseResult where
    fmap = liftM

instance Applicative ParseResult where
    pure  = return
    (<*>) = ap

instance Monad ParseResult where
    (ParseError s) >>= f = ParseError s
    (Parsed     a) >>= f = f a
    return = Parsed

instance Show (ParseResult Term) where
    show (Parsed     tm) = show tm
    show (ParseError s ) = s

parse :: [(Token, AlexPosn, String)] -> ParseResult Term
parse = (fmap tsToT) . parseTS

tsToT :: [Term] -> Term
tsToT [ ] = error "tsToT of []"
tsToT [t] = t
tsToT ts  = App (tsToT (init ts)) (last ts)

parseError :: [(Token, AlexPosn, String)] -> ParseResult a
parseError tokens = ParseError $ case tokens of
    []                       -> "Reached end of file while parsing"
    ((_, AlexPn _ y x, s):rest) ->
        "Parse error on line " ++ show y ++ ", column " ++ show x ++ ": "
        ++ "'" ++ s ++ "'"
}
