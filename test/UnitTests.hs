{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE BlockArguments        #-}

module UnitTests (tests) where

import           Control.Lens
import           Control.Monad              hiding (fmap)
import           Control.Monad.Freer.Extras.Log as Log
import           Data.Default               (Default (..))
import qualified Data.Map                   as Map
import           Data.Text                  (Text)
import           Ledger
import           Ledger.Ada                 as Ada
import           Plutus.Contract            as Contract
import           Plutus.Trace.Emulator      as Emulator
import           PlutusTx.Prelude           hiding (Semigroup(..), check)
-- import           Prelude                    (IO) -- , Semigroup(..), Show (..), putStrLn)
import           Plutus.Contract.Trace      as Emulator
import           Plutus.Contract.Test       as Test
import           Data.Aeson.Types           as JSON
import qualified Prelude
import           Wallet.Emulator.MultiAgent  (eteEvent)
import           Plutus.Trace.Emulator.Types (_ContractLog, cilMessage)

import           Test.Tasty
-- import           Test.Tasty.HUnit
-- import Debug.Trace

import           ProofOfBurn


-- | Contract instance
--
contract' :: Contract () ProofOfBurn.Schema Text ()
contract' = ProofOfBurn.contract


tests :: TestTree
tests = testGroup "unit tests"
    [ testLock
    , testLockTwoTimes
    , testRedeem
    , testLockAndRedeem1
    , testLockAndRedeem2
    , testLockTwiceAndRedeem
    , testBurnAndBurnedTrace1
    , testBurnAndBurnedTrace2
    , testBurnBurnedTraceAndRedeem
    ]


-- | Test `lock` endpoint.
--
--   Just call `lock` and ensure balance changed.
testLock :: TestTree
testLock = check "lock"
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

-- | Test `lock` endpoint.
--
--   Just call `lock` two times and ensure balance changed.
testLockTwoTimes :: TestTree
testLockTwoTimes = check "lock"
    (     walletFundsChange w1 (Ada.lovelaceValueOf (-50_000_000))
     .&&. walletFundsChange w2 (Ada.lovelaceValueOf 0)
     .&&. walletFundsChange w3 (Ada.lovelaceValueOf 0)
    )
    do
        pob1 <- activateContractWallet w1 contract'
        void $ Emulator.waitNSlots 2
        callEndpoint @"lock" pob1 (pubKeyHash $ walletPubKey w2, Ada.lovelaceValueOf 20_000_000)
        void $ Emulator.waitNSlots 2
        pob2 <- activateContractWallet w1 contract'
        void $ Emulator.waitNSlots 2
        callEndpoint @"lock" pob2 (pubKeyHash $ walletPubKey w2, Ada.lovelaceValueOf 30_000_000)
        -- TODO don't work with `pob1`
        -- callEndpoint @"lock" pob1 (pubKeyHash $ walletPubKey w2, Ada.lovelaceValueOf 30_000_000)
        void $ Emulator.waitNSlots 2


-- | Test `redeem` endpoint.
--
--   Check balances not changed.
testRedeem :: TestTree
testRedeem = check "redeem"
    (     walletFundsChange w1 (Ada.lovelaceValueOf 0)
     .&&. walletFundsChange w2 (Ada.lovelaceValueOf 0)
     .&&. walletFundsChange w3 (Ada.lovelaceValueOf 0)
     .&&. assertNoFailedTransactions
    )
    do
        pob2 <- activateContractWallet w2 contract'
        void $ Emulator.waitNSlots 2
        callEndpoint @"redeem" pob2 ()
        void $ Emulator.waitNSlots 2


-- | Test `lock` and `redeem` endpoints in pair.
--
--   Lock some value and redeem it in other wallet.
testLockAndRedeem1 :: TestTree
testLockAndRedeem1 = check "lock and redeem 1"
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

-- | Test `lock` and `redeem` endpoints in pair.
--
--   Lock some value and redeem it in other wallet.
testLockAndRedeem2 :: TestTree
testLockAndRedeem2 = check "lock and redeem 2"
    (      walletFundsChange w1 (Ada.lovelaceValueOf (  0)) -- TODO why here 50_000_000??
      .&&. walletFundsChange w2 (Ada.lovelaceValueOf (  0))
      .&&. walletFundsChange w3 (Ada.lovelaceValueOf ( -50_000_000))
    )
    do
        pob1 <- activateContractWallet w1 contract'
        pob2 <- activateContractWallet w3 contract'
        pob3 <- activateContractWallet w1 contract'
        void $ Emulator.waitNSlots 2
        --
        callEndpoint @"lock" pob1 (pubKeyHash $ walletPubKey w2, Ada.lovelaceValueOf 50_000_000)
        void $ Emulator.waitNSlots 2
        --
        callEndpoint @"lock" pob2 (pubKeyHash $ walletPubKey w1, Ada.lovelaceValueOf 50_000_000)
        void $ Emulator.waitNSlots 2
        --
        callEndpoint @"redeem" pob3 ()
        void $ Emulator.waitNSlots 2


testLockTwiceAndRedeem :: TestTree
testLockTwiceAndRedeem  = check "lock twice and redeem"
    (     walletFundsChange w1 (Ada.lovelaceValueOf (-50_000_000))
     .&&. walletFundsChange w2 (Ada.lovelaceValueOf ( 50_000_000))
     .&&. walletFundsChange w3 (Ada.lovelaceValueOf 0)
    )
    do
        pob1 <- activateContractWallet w1 contract'
        void $ Emulator.waitNSlots 2
        callEndpoint @"lock" pob1 (pubKeyHash $ walletPubKey w2, Ada.lovelaceValueOf 20_000_000)
        void $ Emulator.waitNSlots 2
        -- Repeat `lock` on same contract
        callEndpoint @"lock" pob1 (pubKeyHash $ walletPubKey w2, Ada.lovelaceValueOf 30_000_000)
        void $ Emulator.waitNSlots 2
        pob3 <- activateContractWallet w2 contract'
        void $ Emulator.waitNSlots 2
        callEndpoint @"redeem" pob3 ()
        void $ Emulator.waitNSlots 2


-- | Test `burn` and `burnedTrace` endpoints in pair.
--
--   Check that PoB can burn value to any string; and can't get trace for this. TODO ???
testBurnAndBurnedTrace1 :: TestTree
testBurnAndBurnedTrace1 = check "burn and burnedTrace 1"
    (     walletFundsChange w1 (Ada.lovelaceValueOf (-50_000_000))
      -- TODO Why not detected??
     .&&. assertInstanceLog (Emulator.walletInstanceTag w1) ((Prelude.elem expectedLogMsg) . mapMaybe (preview (eteEvent . cilMessage . _ContractLog)))
     .&&. assertNoFailedTransactions
    )
    do
        pob1 <- activateContractWallet w1 contract'
        void $ Emulator.waitNSlots 2
        callEndpoint @"burn" pob1 ("ab", Ada.lovelaceValueOf 50_000_000)
        void $ Emulator.waitNSlots 2
        --
        pob2 <- activateContractWallet w1 contract'
        callEndpoint @"burnedTrace" pob2 "ab"
        void $ Emulator.waitNSlots 2
  where
    expectedLogMsg = JSON.String "Nothing burned with given commitment" -- TODO give from main module


-- | Test `burn` and `burnedTrace` endpoints in pair.
--
--   Check that PoB can burn value to some address; and TODO get trace for this.
testBurnAndBurnedTrace2 :: TestTree
testBurnAndBurnedTrace2 = check "burn and burnedTrace 2"
    (     walletFundsChange w1 (Ada.lovelaceValueOf (-50_000_000))
     .&&. walletFundsChange w2 (Ada.lovelaceValueOf 0)
     .&&. walletFundsChange w3 (Ada.lovelaceValueOf 0)
      -- TODO Why not detected??
     .&&. assertInstanceLog (Emulator.walletInstanceTag w2) ((Prelude.elem expectedLogMsg) . mapMaybe (preview (eteEvent . cilMessage . _ContractLog)))
     .&&. assertNoFailedTransactions
    )
    do
        -- TODO Here we need public key hash to access to `burnedTrace`
        --
        --      But in which form? In which encoding??
        --
        --      Script can't find `getPubKeyHash $ pubKeyHash $ walletPubKey w3` 
        --
        let burnedAddr = getPubKeyHash $ pubKeyHash $ walletPubKey w3
        --Debug.Trace.traceM "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
        --Debug.Trace.traceShowM burnedAddr
        --Debug.Trace.traceM (Prelude.show w3)
        --Debug.Trace.traceM "^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^"
        pob1 <- activateContractWallet w1 contract'
        void $ Emulator.waitNSlots 2
        callEndpoint @"burn" pob1 (burnedAddr, Ada.lovelaceValueOf 50_000_000)
        void $ Emulator.waitNSlots 2
        --
        pob2 <- activateContractWallet w2 contract'
        void $ Emulator.waitNSlots 2
        callEndpoint @"burnedTrace" pob2 burnedAddr
        void $ Emulator.waitNSlots 2
  where
    expectedLogMsg = JSON.String "Nothing burned with given commitment" -- TODO give from main module TODO Actually "Value burned with given commitment: ..."


-- | Test `burn`, `burnedTrace`, `redeem` endpoints in pair.
--
--   Check that PoB can burn value to some address; and TODO get trace for this. Redeem of that value is not possible.
testBurnBurnedTraceAndRedeem :: TestTree
testBurnBurnedTraceAndRedeem = check "burn, burnedTrace and redeem"
    (      walletFundsChange w1 (Ada.lovelaceValueOf (-50_000_000))
      .&&. walletFundsChange w2 (Ada.lovelaceValueOf 0)
      .&&. walletFundsChange w3 (Ada.lovelaceValueOf 0)
      -- TODO Why not detected??
      .&&. assertInstanceLog (Emulator.walletInstanceTag w2) ((Prelude.elem expectedLogMsg) . mapMaybe (preview (eteEvent . cilMessage . _ContractLog)))
      .&&. Test.not assertNoFailedTransactions -- `redeem` is not working
    )
    do
        -- TODO Here we need public key hash to access to `burnedTrace`
        --
        --      But in which form? In which encoding??
        --
        --      Script can't find `getPubKeyHash $ pubKeyHash $ walletPubKey w3` 
        --
        let burnedAddr = getPubKeyHash $ pubKeyHash $ walletPubKey w3
        --Debug.Trace.traceM "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
        --Debug.Trace.traceShowM burnedAddr
        --Debug.Trace.traceM (Prelude.show w3)
        --Debug.Trace.traceM "^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^"
        pob1 <- activateContractWallet w1 contract'
        void $ Emulator.waitNSlots 2
        callEndpoint @"burn" pob1 (burnedAddr, Ada.lovelaceValueOf 50_000_000)
        void $ Emulator.waitNSlots 2
        --
        pob2 <- activateContractWallet w2 contract'
        void $ Emulator.waitNSlots 2
        callEndpoint @"burnedTrace" pob2 burnedAddr
        void $ Emulator.waitNSlots 2
        --
        pob3 <- activateContractWallet w3 contract'
        void $ Emulator.waitNSlots 2
        callEndpoint @"redeem" pob3 ()
        void $ Emulator.waitNSlots 2
  where
    expectedLogMsg = JSON.String "Nothing burned with given commitment" -- TODO give from main module TODO Actually "Value burned with given commitment: ..."


-- | Test configuration: three wallets with 100 ADA on each of them.
--
defaultEmCfg :: Emulator.EmulatorConfig
defaultEmCfg = Emulator.EmulatorConfig (Left $ Map.fromList [(w1, v), (w2, v), (w3, v)]) def def
  where
    v :: Ledger.Value
    v = Ada.lovelaceValueOf 100_000_000


-- | Shortage for `checkPredicateOptions`: call this function with default params.
--
check :: Prelude.String -> TracePredicate -> EmulatorTrace () -> TestTree
check = checkPredicateOptions (defaultCheckOptions & emulatorConfig .~ defaultEmCfg & minLogLevel .~ Log.Debug)

