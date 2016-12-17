{
module Lexer where
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
    "|"                    { \p s -> ( TK_Bar     , p , s ) }
    "->"                   { \p s -> ( TK_Arrow   , p , s ) }
    "if"                   { \p s -> ( TK_If      , p , s ) }
    "then"                 { \p s -> ( TK_Then    , p , s ) }
    "else"                 { \p s -> ( TK_Else    , p , s ) }
    "end"                  { \p s -> ( TK_End     , p , s ) }
    "("                    { \p s -> ( TK_LParen  , p , s ) }
    ")"                    { \p s -> ( TK_RParen  , p , s ) }
    "."                    { \p s -> ( TK_Dot     , p , s ) }
    "nil"                  { \p s -> ( TK_Nil     , p , s ) }
    "<"                    { \p s -> ( TK_Hd      , p , s ) }
    ">"                    { \p s -> ( TK_Tl      , p , s ) }
    "@"                    { \p s -> ( TK_At      , p , s ) }
    ":"                    { \p s -> ( TK_Colon   , p , s ) }
    "Y"                    { \p s -> ( TK_Fix     , p , s ) }
    $alpha [$alnum \_]*    { \p s -> ( TK_Name    , p , s ) }
    .                      { \p s -> ( TK_Error   , p , s ) }

{
type Token = (TokenType, AlexPosn, String)

data TokenType
    = TK_Bar
    | TK_Arrow
    | TK_If
    | TK_Then
    | TK_Else
    | TK_End
    | TK_LParen
    | TK_RParen
    | TK_Dot
    | TK_Nil
    | TK_Hd
    | TK_Tl
    | TK_At
    | TK_Colon
    | TK_Fix
    | TK_Name
    | TK_Error
    deriving (Show, Eq)
}
