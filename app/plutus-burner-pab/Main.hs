{-# LANGUAGE TypeApplications    #-}

module Main where

import qualified Plutus.PAB.Effects.Contract.Builtin as Builtin
import Plutus.PAB.Run (runWith)
import Contracts


main :: IO ()
main = runWith (Builtin.handleBuiltin @Contracts)

