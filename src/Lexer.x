{
module Lexer where

import qualified Data.Map as M
}

%wrapper "posn"

$digit = 0-9
$lower = a-z
$upper = A-Z
$alpha = [$lower $upper]
$alnum = [$alpha $digit]

tokens :-
    $white+                ;
    \#.*\n                 ; -- Single line comments with '#'
    "def"                  { \p s -> ( TK_Def   , p ) }
    "->"                   { \p s -> ( TK_Arrow , p ) }
    "end"                  { \p s -> ( TK_End   , p ) }
    "if"                   { \p s -> ( TK_If    , p ) }
    "then"                 { \p s -> ( TK_Then  , p ) }
    "else"                 { \p s -> ( TK_Else  , p ) }
    "nil"                  { \p s -> ( TK_Nil   , p ) }
    "."                    { \p s -> ( TK_Dot   , p ) }
    "<"                    { \p s -> ( TK_Hd    , p ) }
    ">"                    { \p s -> ( TK_Tl    , p ) }
    $alpha [$alnum \_]*    { \p s -> ( TK_Name s, p ) }

{
data Token
    = TK_Def
    | TK_Arrow
    | TK_End
    | TK_If
    | TK_Then
    | TK_Else
    | TK_Nil
    | TK_Dot
    | TK_Hd
    | TK_Tl
    | TK_Name Name
    deriving (Show, Eq)
}
