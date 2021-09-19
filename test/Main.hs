{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE BlockArguments        #-}

module Main where

import           Control.Lens
import           Control.Monad              hiding (fmap)
import           Control.Monad.Freer.Extras.Log as Log
import           Data.Default               (Default (..))
import qualified Data.Map                   as Map
-- import           Data.Monoid                (Last (..))
import           Data.Text                  (Text)
import           Ledger
import           Ledger.Ada                 as Ada
import           Plutus.Contract            as Contract
import           Plutus.Trace.Emulator      as Emulator
import           PlutusTx.Prelude           hiding (Semigroup(..), check)
import           Prelude                    (IO) -- , Semigroup(..), Show (..), putStrLn)
import           Plutus.Contract.Trace      as Emulator
import           Plutus.Contract.Test

import           Test.Tasty
-- import           Test.Tasty.HUnit

import           ProofOfBurn


main :: IO ()
main = defaultMain tests


tests :: TestTree
tests = testGroup "proof-of-burn tests"
    [ testsSimple
    ]


-- TODO which parameter is needed
--      here? ----------+
--                      |
--                      v
contract' :: Contract Value ProofOfBurn.Schema Text ()
contract' = ProofOfBurn.contract


testsSimple :: TestTree
testsSimple = testGroup "simple"
    [ check "lock"
        (     walletFundsChange w1 (Ada.lovelaceValueOf (-50_000_000))
         .&&. walletFundsChange w2 (Ada.lovelaceValueOf 0)
         .&&. walletFundsChange w3 (Ada.lovelaceValueOf 0)
        )
        do
            pob1 <- activateContractWallet w1 contract'
            void $ Emulator.waitNSlots 2
            callEndpoint @"lock" pob1 (pubKeyHash $ walletPubKey w2, Ada.lovelaceValueOf 50_000_000)
            void $ Emulator.waitNSlots 2
    -- TODO It seems that `redeem` must filed (or output some warning message).
    --      So implement it and check then
    -- [ testCase "first" $ runEmulatorTraceIO' def defaultEmCfg
    , check "redeem"
        (     walletFundsChange w1 (Ada.lovelaceValueOf 0)
         .&&. walletFundsChange w2 (Ada.lovelaceValueOf 0)
         .&&. walletFundsChange w3 (Ada.lovelaceValueOf 0)
        )
            do
                pob2 <- activateContractWallet w2 contract'
                void $ Emulator.waitNSlots 2
                callEndpoint @"redeem" pob2 ()
                void $ Emulator.waitNSlots 2
    , check "lock and redeem 1"
        (     walletFundsChange w1 (Ada.lovelaceValueOf (-50_000_000))
         .&&. walletFundsChange w2 (Ada.lovelaceValueOf ( 50_000_000))
         .&&. walletFundsChange w3 (Ada.lovelaceValueOf 0)
        )
        do
            pob1 <- activateContractWallet w1 contract'
            void $ Emulator.waitNSlots 2
            callEndpoint @"lock" pob1 (pubKeyHash $ walletPubKey w2, Ada.lovelaceValueOf 50_000_000)
            void $ Emulator.waitNSlots 2
            pob2 <- activateContractWallet w2 contract'
            void $ Emulator.waitNSlots 2
            callEndpoint @"redeem" pob2 ()
            void $ Emulator.waitNSlots 2
    ]
  where
    check = checkPredicateOptions (defaultCheckOptions & emulatorConfig .~ defaultEmCfg & minLogLevel .~ Log.Debug)
    -- check with output:
    -- check name _preds act = testCase name $ runEmulatorTraceIO' def defaultEmCfg act


defaultEmCfg :: Emulator.EmulatorConfig
defaultEmCfg = Emulator.EmulatorConfig (Left $ Map.fromList [(w1, v), (w2, v), (w3, v)]) def def
  where
    v :: Value
    v = Ada.lovelaceValueOf 100_000_000

