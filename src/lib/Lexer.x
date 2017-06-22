{
module Lexer (Token (..), TokenType (..), tkTy, scan) where

import Util

import Control.Monad.Except
import qualified Data.Map as M
}

%wrapper "monadUserState"

$digit = 0-9
$lower = a-z
$upper = A-Z
$alpha = [$lower $upper]
$alnum = [$alpha $digit]

tokens :-
    $white+                ;
    \/\/.*\n               ; -- C-style single line comments
    "|"                    { mkTK TK_Bar    }
    "->"                   { mkTK TK_Arrow  }
    "if"                   { mkTK TK_If     }
    "then"                 { mkTK TK_Then   }
    "else"                 { mkTK TK_Else   }
    "end"                  { mkTK TK_End    }
    "let"                  { mkTK TK_Let    }
    "="                    { mkTK TK_Eq     }
    "in"                   { mkTK TK_In     }
    "fix"                  { mkTK TK_Fix    }
    "("                    { mkTK TK_LParen }
    ")"                    { mkTK TK_RParen }
    "."                    { mkTK TK_Dot    }
    "nil"                  { mkTK TK_Nil    }
    "<"                    { mkTK TK_Hd     }
    ">"                    { mkTK TK_Tl     }
    "@"                    { mkTK TK_At     }
    ":"                    { mkTK TK_Colon  }
    $alpha [$alnum \_]*    { mkName         }
    .                      { mkTK TK_Error  }

{
newtype Token = Token (Maybe Pos, TokenType) deriving (Show, Eq)

tkTy :: Token -> TokenType
tkTy (Token (_, ty)) = ty

instance Positionable Token where
    getPos (Token (pos, tk)) = pos

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
    | TK_Fix
    | TK_LParen
    | TK_RParen
    | TK_Dot
    | TK_Nil
    | TK_Hd
    | TK_Tl
    | TK_At
    | TK_Colon
    | TK_Name Name
    | TK_Error
    | TK_EOF
    deriving (Show, Eq)

scan :: String -> Result (M.Map String Name, Name, [Token])
scan = (either throwError return) . lexer

lexer :: String -> Either String (M.Map String Name, Name, [Token])
lexer s = runAlex s loop where
    loop = do
        tk <- alexMonadScan
        if tkTy tk == TK_EOF then do
            AlexUserState map nn <- getUserState
            return (map, nn, [])
        else do
            (map, nn, tks) <- loop
            AlexUserState map nn <- getUserState
            return (map, nn, (tk:tks))

mkTK :: TokenType -> AlexInput -> Int -> Alex Token
mkTK tk (AlexPn _ r c, _, _, _) l =
    return $ Token (Just (Pos r c r (c + l)), tk)

mkName :: AlexInput -> Int -> Alex Token
mkName (AlexPn _ r c, _, _, s) l = do
    let text = take l s
    map <- getNameMap
    nn  <- getNextName
    if text `M.member` map then
        return $ Token (Just (Pos r c r (c + l)), TK_Name (map M.! text))
    else do
        setNextName $ after nn
        setNameMap $ M.insert text nn map
        return $ Token (Just (Pos r c r (c + l)), TK_Name nn)

getNameMap :: Alex (M.Map String Name)
getNameMap = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, nameMap ust)

setNameMap :: (M.Map String Name) -> Alex ()
setNameMap map = Alex $ \s -> Right (s{alex_ust=(alex_ust s){nameMap=map}}, ())

getNextName :: Alex Name
getNextName = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, nextName ust)

setNextName :: Name -> Alex ()
setNextName nn = Alex $ \s -> Right (s{alex_ust=(alex_ust s){nextName=nn}}, ())

getUserState ::  Alex AlexUserState
getUserState = Alex $ \s -> Right (s, alex_ust s)

data AlexUserState = AlexUserState
    { nameMap  :: M.Map String Name
    , nextName :: Name
    } deriving Show

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState M.empty (Name 0)

alexEOF :: Alex Token
alexEOF = return $ Token (Nothing, TK_EOF)
}
