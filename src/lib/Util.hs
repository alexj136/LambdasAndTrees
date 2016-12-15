module Util where

import qualified Data.Map as M
import Control.Monad (liftM, ap)

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
