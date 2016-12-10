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
    \/\/.*\n               ; -- C-style single line comments
    "def"                  { \p s -> ( TK_Def    , p , s ) }
    "="                    { \p s -> ( TK_Eq     , p , s ) }
    "|"                    { \p s -> ( TK_Bar    , p , s ) }
    "->"                   { \p s -> ( TK_Arrow  , p , s ) }
    "("                    { \p s -> ( TK_LParen , p , s ) }
    ")"                    { \p s -> ( TK_RParen , p , s ) }
    "end"                  { \p s -> ( TK_End    , p , s ) }
    "if"                   { \p s -> ( TK_If     , p , s ) }
    "then"                 { \p s -> ( TK_Then   , p , s ) }
    "else"                 { \p s -> ( TK_Else   , p , s ) }
    "nil"                  { \p s -> ( TK_Nil    , p , s ) }
    "."                    { \p s -> ( TK_Dot    , p , s ) }
    "<"                    { \p s -> ( TK_Hd     , p , s ) }
    ">"                    { \p s -> ( TK_Tl     , p , s ) }
    $alpha [$alnum \_]*    { \p s -> ( TK_Name   , p , s ) }
    .                      { \p s -> ( TK_Error  , p , s ) }

{
data Token
    = TK_Def
    | TK_Eq
    | TK_Bar
    | TK_Arrow
    | TK_LParen
    | TK_RParen
    | TK_End
    | TK_If
    | TK_Then
    | TK_Else
    | TK_Nil
    | TK_Dot
    | TK_Hd
    | TK_Tl
    | TK_Name
    | TK_Error
    deriving (Show, Eq)

alexEOF = undefined
}
