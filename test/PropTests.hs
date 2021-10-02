module PropTests where

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Prelude


tests :: TestTree
tests = testProperty "PoB" prop_PoB

prop_PoB :: Bool
prop_PoB = True
