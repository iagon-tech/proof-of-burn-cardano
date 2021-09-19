{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Main where

import           Control.Monad              hiding (fmap)
import           Control.Monad.Freer.Extras as Extras
import           Data.Default               (Default (..))
import qualified Data.Map                   as Map
import           Data.Monoid                (Last (..))
import           Data.Text                  (Text)
import           Ledger
import           Ledger.Value               as Value
import           Ledger.Ada                 as Ada
import           Plutus.Contract            as Contract
import           Plutus.Trace.Emulator      as Emulator
import           PlutusTx.Prelude           hiding (Semigroup(..), unless)
import           Prelude                    (IO, Semigroup(..), Show (..), putStrLn)
import           Plutus.Contract.Trace      as Emulator
import           Plutus.Contract.Test
-- import           Wallet.Emulator.Wallet
import           Test.Tasty
import           Test.Tasty.HUnit

--import           Week06.Oracle.Core
--import           Week06.Oracle.Funds
--import           Week06.Oracle.Swap
import           ProofOfBurn

assetSymbol :: CurrencySymbol
assetSymbol = "ff"

assetToken :: TokenName
assetToken = "USDT"

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "proof-of-burn tests"
    [ testsSimple
    ]



emCfg :: Emulator.EmulatorConfig
-- emCfg = Emulator.EmulatorConfig (Left (Map.fromList [(knownWallet i, v) | i <- [1..3]])) def def
emCfg = Emulator.EmulatorConfig (Left $ Map.fromList [(w1, v), (w2, v), (w3, v)]) def def
  where
        v :: Value
        v = Ada.lovelaceValueOf 100_000_000


testsSimple :: TestTree
testsSimple = testGroup "simple"
    [ testCase "first" $ do
        -- putStrLn "TEST"
        runEmulatorTraceIO' def emCfg myTrace
    ]
    where

contract' :: Contract (Value) Schema Text ()
contract' = ProofOfBurn.contract

myTrace :: EmulatorTrace ()
myTrace = do
    pob1 <- activateContractWallet w1 contract'
    void $ Emulator.waitNSlots 2
    callEndpoint @"lock" pob1 (pubKeyHash $ walletPubKey w2, Ada.lovelaceValueOf 50_000_000)
    void $ Emulator.waitNSlots 2
    pob2 <- activateContractWallet w2 contract'
    void $ Emulator.waitNSlots 2
    callEndpoint @"redeem" pob2 ()
    void $ Emulator.waitNSlots 2
    return ()
