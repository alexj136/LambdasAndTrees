{
module Lexer where

import Util
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
    "|"                    { \p s -> Token ( TK_Bar     , p , s ) }
    "->"                   { \p s -> Token ( TK_Arrow   , p , s ) }
    "if"                   { \p s -> Token ( TK_If      , p , s ) }
    "then"                 { \p s -> Token ( TK_Then    , p , s ) }
    "else"                 { \p s -> Token ( TK_Else    , p , s ) }
    "end"                  { \p s -> Token ( TK_End     , p , s ) }
    "let"                  { \p s -> Token ( TK_Let     , p , s ) }
    "="                    { \p s -> Token ( TK_Eq      , p , s ) }
    "in"                   { \p s -> Token ( TK_In      , p , s ) }
    "("                    { \p s -> Token ( TK_LParen  , p , s ) }
    ")"                    { \p s -> Token ( TK_RParen  , p , s ) }
    "."                    { \p s -> Token ( TK_Dot     , p , s ) }
    "nil"                  { \p s -> Token ( TK_Nil     , p , s ) }
    "<"                    { \p s -> Token ( TK_Hd      , p , s ) }
    ">"                    { \p s -> Token ( TK_Tl      , p , s ) }
    "@"                    { \p s -> Token ( TK_At      , p , s ) }
    ":"                    { \p s -> Token ( TK_Colon   , p , s ) }
    $alpha [$alnum \_]*    { \p s -> Token ( TK_Name    , p , s ) }
    .                      { \p s -> Token ( TK_Error   , p , s ) }

{
newtype Token = Token (TokenType, AlexPosn, String)

instance Positionable Token where
    getPos (Token (_, AlexPn _ r c, s)) = Just $ Pos r c r (r + length s)

data TokenType
    = TK_Bar
    | TK_Arrow
    | TK_If
    | TK_Then
    | TK_Else
    | TK_End
    | TK_Let
    | TK_Eq
    | TK_In
    | TK_LParen
    | TK_RParen
    | TK_Dot
    | TK_Nil
    | TK_Hd
    | TK_Tl
    | TK_At
    | TK_Colon
    | TK_Name
    | TK_Error
    deriving (Show, Eq)
}
