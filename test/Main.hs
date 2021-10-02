module Main where

import           Prelude                    (IO, ($))
import           Test.Tasty

import           UnitTests
import           PropTests


main :: IO ()
main = defaultMain $ testGroup "proof of burn"
    [ UnitTests.tests
    , PropTests.tests
    ]
