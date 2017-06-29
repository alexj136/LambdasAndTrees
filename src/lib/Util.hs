module Util where

import Prelude hiding (span)
import qualified Data.Map as M
import Control.Monad.Except
import Control.Monad.State

------------------
-- Result Monad --
------------------

type Result = Except (M.Map Name String -> String)

-- Throw an error that doesn't require name map conversion
throwBasic :: String -> Result a
throwBasic s = throwError $ \_ -> s

-------------------
-- The Name type --
-------------------

newtype Name = Name Integer deriving (Eq, Ord)

instance Show Name where
    show (Name n) = "n" ++ show n

-- Get the successor to a name
after :: Name -> Name
after (Name n) = Name (n + 1)

-- Generate a fresh name in the StateT Name monad
fresh :: Monad m => StateT Name m Name
fresh = state $ \n -> (n, after n)

-- Query a name map
(!?) :: M.Map Name String -> Name -> String
(!?) map name = maybe (show name) id (M.lookup name map)

-- Reverse the direction of a map - used to reverse the name map generated by
-- the lexer for use in printing error messages.
swap :: Ord a => Ord b => M.Map a b -> M.Map b a
swap = M.fromList . map (\(k,v) -> (v,k)) . M.toList

--------------------------------
-- Class for printable things --
--------------------------------

class PrettyPrint a where
    prettyPrint :: M.Map Name String -> a -> String

----------------------------------------------------------
-- Class for things that can have source code positions --
----------------------------------------------------------

class Positionable a where
    getPos :: a -> Maybe Pos

data Pos = Pos
    { startRow :: Int
    , startCol :: Int
    , endRow   :: Int
    , endCol   :: Int
    } deriving (Eq, Ord)

instance Show Pos where
    show (Pos sR sC eR eC) = "(row " ++ show sR ++ ", col "
        ++ show sC ++ ") -- (row " ++ show eR ++ ", col " ++ show eC ++ ")"

showMPos :: Maybe Pos -> String
showMPos = maybe "[NO POSITION DATA]" show

instance Positionable a => Positionable [a] where
    getPos as | length as == 0 = Nothing
              | length as == 1 = getPos $ head as
              | length as >  1 = (head as) `span` (last as)

-- Make a Pos from two positionables, where the first provides the start
-- positions and the second provides the end positions.
span :: Positionable a => Positionable b => a -> b -> Maybe Pos
span a b = do
    Pos sR sC _  _  <- getPos a
    Pos _  _  eR eC <- getPos b
    return $ Pos sR sC eR eC
