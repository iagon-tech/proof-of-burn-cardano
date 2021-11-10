{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE LambdaCase            #-}

module UnitTests (tests) where

import           Control.Lens
import           Control.Monad              hiding (fmap)
import           Control.Monad.Freer.Extras.Log as Log
import           Data.Aeson.Types           as JSON
import           Data.Default               (Default (..))
import qualified Data.Map                   as Map
import           Data.Semigroup             ((<>))
import qualified Data.Text                as T
import           Ledger
import           Ledger.Ada                 as Ada
import           Plutus.Contract            as Contract
import           Plutus.Contract.Test       as Test
import           Plutus.Trace.Emulator      as Emulator
import           Plutus.Trace.Emulator.Types (_ContractLog, cilMessage)
import           PlutusTx.Prelude           hiding (Semigroup(..), check, elem)
import           Prelude (elem, show, String)
import           Wallet.Emulator.MultiAgent  (eteEvent)

import           Test.Tasty

import           ProofOfBurn


tests :: TestTree
tests = testGroup "unit tests"
    [ testLock
    , testLockTwice
    , testRedeem
    , testLockAndRedeem1
    , testLockAndRedeem2
    , testLockAndRedeemOurselves
    , testLockTwiceAndRedeem
    , testBurnAndValidateBurn1
    , testBurnAndValidateBurn2
    , testBurnAndValidateBurn3
    , testBurnAndValidateBurn4
    , testBurnAndValidateBurn5
    , testBurnedTwice1
    , testBurnedTwice2
    , testBurnBurnedTraceAndRedeem
    , testLockBurnRedeem
    ]


-- | Test `lock` endpoint.
--
--   Just call `lock` and ensure balance changed.
testLock :: TestTree
testLock = check "lock"
    (     walletFundsChange w1 (Ada.adaValueOf (-50))
     .&&. walletFundsChange w2 (Ada.adaValueOf 0)
     .&&. walletFundsChange w3 (Ada.adaValueOf 0)
    )
    do
        pob1 <- activateContractWallet w1 endpoints
        void $ Emulator.waitNSlots 1
        callEndpoint @"lock" pob1 (walletPubKeyHash w2, Ada.adaValueOf 50)
        void $ Emulator.waitNSlots 1


-- | Test `lock` endpoint.
--
--   Just call `lock` two times and ensure balance changed.
testLockTwice :: TestTree
testLockTwice = check "lock twice"
    (     walletFundsChange w1 (Ada.adaValueOf (-50))
     .&&. walletFundsChange w2 (Ada.adaValueOf 0)
     .&&. walletFundsChange w3 (Ada.adaValueOf 0)
    )
    do
        pob1 <- activateContractWallet w1 endpoints
        void $ Emulator.waitNSlots 1
        callEndpoint @"lock" pob1 (walletPubKeyHash w2, Ada.adaValueOf 20)
        void $ Emulator.waitNSlots 1
        callEndpoint @"lock" pob1 (walletPubKeyHash w2, Ada.adaValueOf 30)
        void $ Emulator.waitNSlots 1


-- | Test `redeem` endpoint.
--
--   Check balances not changed.
testRedeem :: TestTree
testRedeem = check "redeem"
    (     walletFundsChange w1 (Ada.adaValueOf 0)
     .&&. walletFundsChange w2 (Ada.adaValueOf 0)
     .&&. walletFundsChange w3 (Ada.adaValueOf 0)
     .&&. assertNoFailedTransactions
    )
    do
        pob2 <- activateContractWallet w2 endpoints
        void $ Emulator.waitNSlots 1
        callEndpoint @"redeem" pob2 ()
        void $ Emulator.waitNSlots 1


-- | Test `lock` and `redeem` endpoints in pair.
--
--   Lock some value and redeem it in other wallet.
testLockAndRedeem1 :: TestTree
testLockAndRedeem1 = check "lock and redeem 1"
    (     walletFundsChange w1 (Ada.adaValueOf (-50))
     .&&. walletFundsChange w2 (Ada.adaValueOf   50)
     .&&. walletFundsChange w3 (Ada.adaValueOf    0)
    )
    do
        pob1 <- activateContractWallet w1 endpoints
        void $ Emulator.waitNSlots 1
        callEndpoint @"lock" pob1 (walletPubKeyHash w2, Ada.adaValueOf 50)
        void $ Emulator.waitNSlots 1
        pob2 <- activateContractWallet w2 endpoints
        void $ Emulator.waitNSlots 1
        callEndpoint @"redeem" pob2 ()
        void $ Emulator.waitNSlots 1


-- | Test `lock` and `redeem` endpoints in pair.
--
--   Lock some value and redeem it in other wallet.
testLockAndRedeem2 :: TestTree
testLockAndRedeem2 = check "lock and redeem 2"
    (      walletFundsChange w1 (Ada.adaValueOf     0)
      .&&. walletFundsChange w2 (Ada.adaValueOf     0)
      .&&. walletFundsChange w3 (Ada.adaValueOf ( -50))
    )
    do
        pob1 <- activateContractWallet w1 endpoints
        pob2 <- activateContractWallet w3 endpoints
        pob3 <- activateContractWallet w1 endpoints
        void $ Emulator.waitNSlots 1
        --
        callEndpoint @"lock" pob1 (walletPubKeyHash w2, Ada.adaValueOf 50)
        void $ Emulator.waitNSlots 1
        --
        callEndpoint @"lock" pob2 (walletPubKeyHash w1, Ada.adaValueOf 50)
        void $ Emulator.waitNSlots 1
        --
        callEndpoint @"redeem" pob3 ()
        void $ Emulator.waitNSlots 5


-- | Test `lock` and `redeem` ourselves
--
--   Lock some value and redeem it in same wallet.
testLockAndRedeemOurselves :: TestTree
testLockAndRedeemOurselves = check "lock and redeem ourselves"
    ( walletFundsChange w1 (Ada.adaValueOf 0)
    )
    do
        h1 <- activateContractWallet w1 endpoints
        void $ Emulator.waitNSlots 1
        --
        callEndpoint @"lock" h1 (walletPubKeyHash w1, Ada.adaValueOf 50)
        void $ Emulator.waitNSlots 5
        --
        callEndpoint @"redeem" h1 ()
        void $ Emulator.waitNSlots 5


testLockTwiceAndRedeem :: TestTree
testLockTwiceAndRedeem  = check "lock twice and redeem"
    (     walletFundsChange w1 (Ada.adaValueOf (-50))
     .&&. walletFundsChange w2 (Ada.adaValueOf   50)
     .&&. walletFundsChange w3 (Ada.adaValueOf    0)
    )
    do
        pob1 <- activateContractWallet w1 endpoints
        void $ Emulator.waitNSlots 1
        callEndpoint @"lock" pob1 (walletPubKeyHash w2, Ada.adaValueOf 20)
        void $ Emulator.waitNSlots 1
        -- Repeat `lock` on same contract
        callEndpoint @"lock" pob1 (walletPubKeyHash w2, Ada.adaValueOf 30)
        void $ Emulator.waitNSlots 1
        pob3 <- activateContractWallet w2 endpoints
        void $ Emulator.waitNSlots 1
        callEndpoint @"redeem" pob3 ()
        void $ Emulator.waitNSlots 1


-- | Test `burn` and `validateBurn` endpoints in pair.
--
--   Check that PoB can burn value to any string; and can get trace for this.
testBurnAndValidateBurn1 :: TestTree
testBurnAndValidateBurn1 = check "burn and validateBurn 1"
    (     walletFundsChange w1 (Ada.adaValueOf (-50))
     .&&. assertInstanceLog (Emulator.walletInstanceTag w1) ((elem (burnedLogMsg 50_000_000)) . mapMaybe (preview (eteEvent . cilMessage . _ContractLog)))
     .&&. assertNoFailedTransactions
    )
    do
        pob1 <- activateContractWallet w1 endpoints
        void $ Emulator.waitNSlots 1
        callEndpoint @"burn" pob1 ("ab", Ada.adaValueOf 50)
        void $ Emulator.waitNSlots 1
        --
        pob2 <- activateContractWallet w1 endpoints
        callEndpoint @"validateBurn" pob2 "ab"
        void $ Emulator.waitNSlots 1


-- | Test `burn` and `validateBurn` endpoints in pair.
--
--   Check that PoB can burn value to some address.
testBurnAndValidateBurn2 :: TestTree
testBurnAndValidateBurn2 = check "burn and validateBurn 2"
    (     walletFundsChange w1 (Ada.adaValueOf (-50))
     .&&. walletFundsChange w2 (Ada.adaValueOf 0)
     .&&. walletFundsChange w3 (Ada.adaValueOf 0)
     .&&. assertInstanceLog (Emulator.walletInstanceTag w2) ((elem (burnedLogMsg 50_000_000)) . mapMaybe (preview (eteEvent . cilMessage . _ContractLog)))
     .&&. assertNoFailedTransactions
    )
    do
        let burnedAddr = getPubKeyHash $ walletPubKeyHash w3
        pob1 <- activateContractWallet w1 endpoints
        void $ Emulator.waitNSlots 1
        callEndpoint @"burn" pob1 (burnedAddr, Ada.adaValueOf 50)
        void $ Emulator.waitNSlots 1
        --
        pob2 <- activateContractWallet w2 endpoints
        void $ Emulator.waitNSlots 1
        callEndpoint @"validateBurn" pob2 burnedAddr
        void $ Emulator.waitNSlots 1


-- | Test `burn` and `validateBurn` endpoints in pair.
--
--   Check that PoB can burn value to any string; and can't get trace for some other string.
testBurnAndValidateBurn3 :: TestTree
testBurnAndValidateBurn3 = check "burn and validateBurn 3"
    (     walletFundsChange w1 (Ada.adaValueOf (-50))
     .&&. assertInstanceLog (Emulator.walletInstanceTag w1) ((elem nothingBurnedLogMsg) . mapMaybe (preview (eteEvent . cilMessage . _ContractLog)))
     .&&. assertNoFailedTransactions
    )
    do
        pob1 <- activateContractWallet w1 endpoints
        void $ Emulator.waitNSlots 1
        callEndpoint @"burn" pob1 ("ab", Ada.adaValueOf 50)
        void $ Emulator.waitNSlots 1
        --
        pob2 <- activateContractWallet w1 endpoints
        -- TODO check answer
        callEndpoint @"validateBurn" pob2 "bc"
        void $ Emulator.waitNSlots 1


-- | Test `burn` and `validateBurn` endpoints in pair.
--
--   Check that PoB can burn value to some address and can't get trace from other address.
testBurnAndValidateBurn4 :: TestTree
testBurnAndValidateBurn4 = check "burn and validateBurn 4"
    (     walletFundsChange w1 (Ada.adaValueOf (-50))
     .&&. walletFundsChange w2 (Ada.adaValueOf 0)
     .&&. walletFundsChange w3 (Ada.adaValueOf 0)
     .&&. assertInstanceLog (Emulator.walletInstanceTag w2) ((elem nothingBurnedLogMsg) . mapMaybe (preview (eteEvent . cilMessage . _ContractLog)))
     .&&. assertNoFailedTransactions
    )
    do
        let burnedAddr      = getPubKeyHash $ walletPubKeyHash w3
            burnedAddrWrong = getPubKeyHash $ walletPubKeyHash w4
        pob1 <- activateContractWallet w1 endpoints
        void $ Emulator.waitNSlots 1
        callEndpoint @"burn" pob1 (burnedAddr, Ada.adaValueOf 50)
        void $ Emulator.waitNSlots 1
        --
        pob2 <- activateContractWallet w2 endpoints
        void $ Emulator.waitNSlots 1
        callEndpoint @"validateBurn" pob2 burnedAddrWrong
        void $ Emulator.waitNSlots 1


-- | Test `burn` and `validateBurnWithAnswer` endpoints in pair.
--
--   Check that PoB can burn value to some address.
testBurnAndValidateBurn5 :: TestTree
testBurnAndValidateBurn5 = check "burn and validateBurn 5"
    (     walletFundsChange w1 (Ada.adaValueOf (-50))
     .&&. walletFundsChange w2 (Ada.adaValueOf 0)
     .&&. walletFundsChange w3 (Ada.adaValueOf 0)
     .&&. assertInstanceLog (Emulator.walletInstanceTag w2) ((elem (burnedLogMsg 50_000_000)) . mapMaybe (preview (eteEvent . cilMessage . _ContractLog)))
     .&&. assertNoFailedTransactions
    )
    do
        let burnedAddr = getPubKeyHash $ walletPubKeyHash w3
        pob1 <- activateContractWallet w1 endpoints
        void $ Emulator.waitNSlots 1
        callEndpoint @"burn" pob1 (burnedAddr, Ada.adaValueOf 50)
        void $ Emulator.waitNSlots 1
        --
        pob2 <- activateContractWallet w2 endpoints
        void $ Emulator.waitNSlots 1
        callEndpoint @"validateBurn" pob2 burnedAddr
        void $ Emulator.waitNSlots 1
        observableState pob2 >>= \case
            ContractStateAction (BurnedValueValidated (Just val)) _
               -> when ((Ada.adaValueOf 50) /= val) $ Emulator.throwError (GenericError $ "Must be 50 ADA, but got: " ++ show val)
            _  -> Emulator.throwError (GenericError "Must return traced value")

-- | Test `burn`, `validateBurn`, `redeem` endpoints in pair.
--
--   Check that PoB can burn value to some address. Redeem of that value is not possible.
testBurnBurnedTraceAndRedeem :: TestTree
testBurnBurnedTraceAndRedeem = check "burn, validateBurn and redeem"
    (     walletFundsChange w1 (Ada.adaValueOf (-50))
     .&&. walletFundsChange w2 (Ada.adaValueOf 0)
     .&&. walletFundsChange w3 (Ada.adaValueOf 0)
     .&&. assertInstanceLog (Emulator.walletInstanceTag w2) ((elem (burnedLogMsg 50_000_000)) . mapMaybe (preview (eteEvent . cilMessage . _ContractLog)))
     .&&. assertContractError endpoints (Emulator.walletInstanceTag w3) contractErrorPredicate ""
    )
    do
        let burnedAddr = getPubKeyHash $ walletPubKeyHash w3
        pob1 <- activateContractWallet w1 endpoints
        void $ Emulator.waitNSlots 1
        callEndpoint @"burn" pob1 (burnedAddr, Ada.adaValueOf 50)
        void $ Emulator.waitNSlots 1
        --
        pob2 <- activateContractWallet w2 endpoints
        void $ Emulator.waitNSlots 1
        -- TODO check answer
        callEndpoint @"validateBurn" pob2 burnedAddr
        void $ Emulator.waitNSlots 1
        --
        pob3 <- activateContractWallet w3 endpoints
        void $ Emulator.waitNSlots 1
        callEndpoint @"redeem" pob3 ()
        void $ Emulator.waitNSlots 1
  where
    contractErrorPredicate (OtherError "No UTxO to redeem from") = True
    contractErrorPredicate _ = False


-- | Test `burn`'ing to same address gives the sum.
--
testBurnedTwice1 :: TestTree
testBurnedTwice1 = check "burn different values to same address"
    (     walletFundsChange w1 (Ada.adaValueOf (-50))
     .&&. assertInstanceLog (Emulator.walletInstanceTag w1) ((elem (burnedLogMsg 50_000_000)) . mapMaybe (preview (eteEvent . cilMessage . _ContractLog)))
     .&&. assertNoFailedTransactions
    )
    do
        let burnedAddr = getPubKeyHash $ walletPubKeyHash w3
        --
        pob1 <- activateContractWallet w1 endpoints
        void $ Emulator.waitNSlots 1
        callEndpoint @"burn" pob1 (burnedAddr, Ada.adaValueOf 15)
        void $ Emulator.waitNSlots 1
        callEndpoint @"burn" pob1 (burnedAddr, Ada.adaValueOf 35)
        void $ Emulator.waitNSlots 1
        --
        pob2 <- activateContractWallet w1 endpoints
        callEndpoint @"validateBurn" pob2 burnedAddr
        void $ Emulator.waitNSlots 1

-- | Test `burn`'ing same value to same address gives the double value.
--
testBurnedTwice2 :: TestTree
testBurnedTwice2 = check "burn twice same values to same address"
    (     walletFundsChange w1 (Ada.adaValueOf (-50))
     .&&. assertInstanceLog (Emulator.walletInstanceTag w1) ((elem (burnedLogMsg 50_000_000)) . mapMaybe (preview (eteEvent . cilMessage . _ContractLog)))
     .&&. assertNoFailedTransactions
    )
    do
        let burnedAddr = getPubKeyHash $ walletPubKeyHash w3
        --
        pob1 <- activateContractWallet w1 endpoints
        void $ Emulator.waitNSlots 1
        callEndpoint @"burn" pob1 (burnedAddr, Ada.adaValueOf 25)
        void $ Emulator.waitNSlots 1
        callEndpoint @"burn" pob1 (burnedAddr, Ada.adaValueOf 25)
        void $ Emulator.waitNSlots 1
        --
        pob2 <- activateContractWallet w1 endpoints
        callEndpoint @"validateBurn" pob2 burnedAddr
        void $ Emulator.waitNSlots 1


-- | Test `lock`, `burn` and `redeem` endpoints in pair, making
--   sure that a burn doesn't "mess"  with the redeeming.
testLockBurnRedeem :: TestTree
testLockBurnRedeem = check "lock, burn and redeem"
    (     walletFundsChange w1 (Ada.adaValueOf (-80))
     .&&. walletFundsChange w2 (Ada.adaValueOf 0)
     .&&. walletFundsChange w3 (Ada.adaValueOf 30)
     .&&. assertInstanceLog (Emulator.walletInstanceTag w2) ((elem (burnedLogMsg 50_000_000)) . mapMaybe (preview (eteEvent . cilMessage . _ContractLog)))
    )
    do
        let burnedAddr = walletPubKeyHash w3
        --
        pob1 <- activateContractWallet w1 endpoints
        void $ Emulator.waitNSlots 1
        callEndpoint @"burn" pob1 (getPubKeyHash burnedAddr, Ada.adaValueOf 50)
        void $ Emulator.waitNSlots 1
        callEndpoint @"lock" pob1 (burnedAddr, Ada.adaValueOf 30)
        void $ Emulator.waitNSlots 1
        --
        pob2 <- activateContractWallet w2 endpoints
        void $ Emulator.waitNSlots 1
        callEndpoint @"validateBurn" pob2 (getPubKeyHash burnedAddr)
        void $ Emulator.waitNSlots 1
        --
        pob3 <- activateContractWallet w3 endpoints
        void $ Emulator.waitNSlots 1
        callEndpoint @"redeem" pob3 ()
        void $ Emulator.waitNSlots 1


-- | The message appeared in a smart contract log about successful burning tracing.
--
burnedLogMsg :: Integer -> JSON.Value
burnedLogMsg lovelaceVal = JSON.String ("Value burned with given commitment: " <> T.pack (show lovelaceVal))


-- | The message appeared in a smart contract log about burning.
--
nothingBurnedLogMsg :: JSON.Value
nothingBurnedLogMsg = JSON.String "Nothing burned with given commitment"


-- | Test configuration: three wallets with 100 ADA on each of them.
--
defaultEmCfg :: Emulator.EmulatorConfig
defaultEmCfg = Emulator.EmulatorConfig (Left $ Map.fromList [(w1, v), (w2, v), (w3, v), (w4, v)]) def def
  where
    v :: Ledger.Value
    v = Ada.adaValueOf 1000


-- | Shortage for `checkPredicateOptions`: call this function with default params.
--
check :: String -> TracePredicate -> EmulatorTrace () -> TestTree
check = checkPredicateOptions (defaultCheckOptions & emulatorConfig .~ defaultEmCfg & minLogLevel .~ Log.Debug)

