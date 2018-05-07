module TestParser where

import TestUtil

import Util
import Lexer
import Parser
import SugarSyntax

import qualified Data.Map as M

parseStr :: String -> Result (M.Map Name String, Name, Term)
parseStr progText = do
    (names, nextName, tokens) <- scan progText
    ast <- parse tokens
    return (swap names, nextName, ast)

tests :: [Test]
tests = []

p1 :: Test
p1 = Test "Simple expression 1" $ True
