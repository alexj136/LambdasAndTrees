module TestUtil where

import qualified Util as U
import SugarSyntax
import Types
import Test.QuickCheck (Testable, property)
import Test.QuickCheck.Property (failed, reason)

instance Testable t => Testable (U.Result t) where
    property (U.Success e) = property e
    property (U.Error   s) = property $ failed { reason = s }

instance Testable Type where
    property _ = property True

instance Testable Term where
    property _ = property True
