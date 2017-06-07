{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module TestUtil where

import qualified Util as U
import SugarSyntax
import Types
import Test.QuickCheck (Testable, property)
import Test.QuickCheck.Property (failed, reason)
import Control.Monad.Except (runExcept)

instance Testable t => Testable (U.Result t) where
    property p = case runExcept p of
        Right e -> property e
        Left  s -> property $ failed { reason = s }

instance Testable Type where
    property _ = property True

instance Testable Term where
    property _ = property True
