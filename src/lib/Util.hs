module Util where

import Prelude hiding (span)
import qualified Data.Map as M
import Control.Monad (liftM, ap)

------------------
-- Result Monad --
------------------

data Result a = Error String | Success a deriving (Show, Eq, Ord)

instance Functor Result where
    fmap = liftM

instance Applicative Result where
    pure  = return
    (<*>) = ap

instance Monad Result where
    (Error   s) >>= f = Error s
    (Success a) >>= f = f a
    return = Success

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
