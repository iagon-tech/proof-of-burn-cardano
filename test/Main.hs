module Main where

import           Prelude                    (IO, ($))
import           Test.Tasty

import           UnitTests


main :: IO ()
main = defaultMain $ testGroup "proof of burn"
    [ UnitTests.tests
    ]
