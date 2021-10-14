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
    , testLockTwoTimes
    , testRedeem
    , testLockAndRedeem1
    , testLockAndRedeem2
    , testLockAndRedeemOurselves
    , testLockTwiceAndRedeem
    , testBurnAndBurnedTrace1
    , testBurnAndBurnedTrace2
    , testBurnAndBurnedTrace3
    , testBurnAndBurnedTrace4
    , testBurnAndBurnedTrace5
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
    (     walletFundsChange w1 (Ada.lovelaceValueOf (-50_000_000))
     .&&. walletFundsChange w2 (Ada.lovelaceValueOf 0)
     .&&. walletFundsChange w3 (Ada.lovelaceValueOf 0)
    )
    do
        pob1 <- activateContractWallet w1 endpoints
        void $ Emulator.waitNSlots 2
        callEndpoint @"lock" pob1 (pubKeyHash $ walletPubKey w2, Ada.lovelaceValueOf 50_000_000)
        void $ Emulator.waitNSlots 2


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
        pob1 <- activateContractWallet w1 endpoints

        void $ Emulator.waitNSlots 2
        callEndpoint @"lock" pob1 (pubKeyHash $ walletPubKey w2, Ada.lovelaceValueOf 20_000_000)
        void $ Emulator.waitNSlots 2
        callEndpoint @"lock" pob1 (pubKeyHash $ walletPubKey w2, Ada.lovelaceValueOf 30_000_000)
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
        pob2 <- activateContractWallet w2 endpoints
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
        pob1 <- activateContractWallet w1 endpoints
        void $ Emulator.waitNSlots 2
        callEndpoint @"lock" pob1 (pubKeyHash $ walletPubKey w2, Ada.lovelaceValueOf 50_000_000)
        void $ Emulator.waitNSlots 2
        pob2 <- activateContractWallet w2 endpoints
        void $ Emulator.waitNSlots 2
        callEndpoint @"redeem" pob2 ()
        void $ Emulator.waitNSlots 2


-- | Test `lock` and `redeem` endpoints in pair.
--
--   Lock some value and redeem it in other wallet.
testLockAndRedeem2 :: TestTree
testLockAndRedeem2 = check "lock and redeem 2"
    (      walletFundsChange w1 (Ada.lovelaceValueOf (  0))
      .&&. walletFundsChange w2 (Ada.lovelaceValueOf (  0))
      .&&. walletFundsChange w3 (Ada.lovelaceValueOf ( -50_000_000))
    )
    do
        pob1 <- activateContractWallet w1 endpoints
        pob2 <- activateContractWallet w3 endpoints
        pob3 <- activateContractWallet w1 endpoints
        void $ Emulator.waitNSlots 2
        --
        callEndpoint @"lock" pob1 (pubKeyHash $ walletPubKey w2, Ada.lovelaceValueOf 50_000_000)
        void $ Emulator.waitNSlots 2
        --
        callEndpoint @"lock" pob2 (pubKeyHash $ walletPubKey w1, Ada.lovelaceValueOf 50_000_000)
        void $ Emulator.waitNSlots 2
        --
        callEndpoint @"redeem" pob3 ()
        void $ Emulator.waitNSlots 5


-- | Test `lock` and `redeem` ourselves
--
--   Lock some value and redeem it in same wallet.
testLockAndRedeemOurselves :: TestTree
testLockAndRedeemOurselves = check "lock and redeem ourselves"
    ( walletFundsChange w1 (Ada.lovelaceValueOf 0)
    )
    do
        h1 <- activateContractWallet w1 endpoints
        void $ Emulator.waitNSlots 2
        --
        callEndpoint @"lock" h1 (pubKeyHash $ walletPubKey w1, Ada.lovelaceValueOf 50_000_000)
        void $ Emulator.waitNSlots 5
        --
        callEndpoint @"redeem" h1 ()
        void $ Emulator.waitNSlots 5


testLockTwiceAndRedeem :: TestTree
testLockTwiceAndRedeem  = check "lock twice and redeem"
    (     walletFundsChange w1 (Ada.lovelaceValueOf (-50_000_000))
     .&&. walletFundsChange w2 (Ada.lovelaceValueOf ( 50_000_000))
     .&&. walletFundsChange w3 (Ada.lovelaceValueOf 0)
    )
    do
        pob1 <- activateContractWallet w1 endpoints
        void $ Emulator.waitNSlots 2
        callEndpoint @"lock" pob1 (pubKeyHash $ walletPubKey w2, Ada.lovelaceValueOf 20_000_000)
        void $ Emulator.waitNSlots 2
        -- Repeat `lock` on same contract
        callEndpoint @"lock" pob1 (pubKeyHash $ walletPubKey w2, Ada.lovelaceValueOf 30_000_000)
        void $ Emulator.waitNSlots 2
        pob3 <- activateContractWallet w2 endpoints
        void $ Emulator.waitNSlots 2
        callEndpoint @"redeem" pob3 ()
        void $ Emulator.waitNSlots 2


-- | Test `burn` and `validateBurn` endpoints in pair.
--
--   Check that PoB can burn value to any string; and can get trace for this.
testBurnAndBurnedTrace1 :: TestTree
testBurnAndBurnedTrace1 = check "burn and validateBurn 1"
    (     walletFundsChange w1 (Ada.lovelaceValueOf (-50_000_000))
     .&&. assertInstanceLog (Emulator.walletInstanceTag w1) ((elem (burnedLogMsg 50_000_000)) . mapMaybe (preview (eteEvent . cilMessage . _ContractLog)))
     .&&. assertNoFailedTransactions
    )
    do
        pob1 <- activateContractWallet w1 endpoints
        void $ Emulator.waitNSlots 2
        callEndpoint @"burn" pob1 ("ab", Ada.lovelaceValueOf 50_000_000)
        void $ Emulator.waitNSlots 2
        --
        pob2 <- activateContractWallet w1 endpoints
        callEndpoint @"validateBurn" pob2 "ab"
        void $ Emulator.waitNSlots 2


-- | Test `burn` and `validateBurn` endpoints in pair.
--
--   Check that PoB can burn value to some address.
testBurnAndBurnedTrace2 :: TestTree
testBurnAndBurnedTrace2 = check "burn and validateBurn 2"
    (     walletFundsChange w1 (Ada.lovelaceValueOf (-50_000_000))
     .&&. walletFundsChange w2 (Ada.lovelaceValueOf 0)
     .&&. walletFundsChange w3 (Ada.lovelaceValueOf 0)
     .&&. assertInstanceLog (Emulator.walletInstanceTag w2) ((elem (burnedLogMsg 50_000_000)) . mapMaybe (preview (eteEvent . cilMessage . _ContractLog)))
     .&&. assertNoFailedTransactions
    )
    do
        let burnedAddr = getPubKeyHash $ pubKeyHash $ walletPubKey w3
        pob1 <- activateContractWallet w1 endpoints
        void $ Emulator.waitNSlots 2
        callEndpoint @"burn" pob1 (burnedAddr, Ada.lovelaceValueOf 50_000_000)
        void $ Emulator.waitNSlots 2
        --
        pob2 <- activateContractWallet w2 endpoints
        void $ Emulator.waitNSlots 2
        callEndpoint @"validateBurn" pob2 burnedAddr
        void $ Emulator.waitNSlots 2


-- | Test `burn` and `validateBurn` endpoints in pair.
--
--   Check that PoB can burn value to any string; and can't get trace for some other string.
testBurnAndBurnedTrace3 :: TestTree
testBurnAndBurnedTrace3 = check "burn and validateBurn 3"
    (     walletFundsChange w1 (Ada.lovelaceValueOf (-50_000_000))
     .&&. assertInstanceLog (Emulator.walletInstanceTag w1) ((elem nothingBurnedLogMsg) . mapMaybe (preview (eteEvent . cilMessage . _ContractLog)))
     .&&. assertNoFailedTransactions
    )
    do
        pob1 <- activateContractWallet w1 endpoints
        void $ Emulator.waitNSlots 2
        callEndpoint @"burn" pob1 ("ab", Ada.lovelaceValueOf 50_000_000)
        void $ Emulator.waitNSlots 2
        --
        pob2 <- activateContractWallet w1 endpoints
        -- TODO check answer
        callEndpoint @"validateBurn" pob2 "bc"
        void $ Emulator.waitNSlots 2


-- | Test `burn` and `validateBurn` endpoints in pair.
--
--   Check that PoB can burn value to some address and can't get trace from other address.
testBurnAndBurnedTrace4 :: TestTree
testBurnAndBurnedTrace4 = check "burn and validateBurn 4"
    (     walletFundsChange w1 (Ada.lovelaceValueOf (-50_000_000))
     .&&. walletFundsChange w2 (Ada.lovelaceValueOf 0)
     .&&. walletFundsChange w3 (Ada.lovelaceValueOf 0)
     .&&. assertInstanceLog (Emulator.walletInstanceTag w2) ((elem nothingBurnedLogMsg) . mapMaybe (preview (eteEvent . cilMessage . _ContractLog)))
     .&&. assertNoFailedTransactions
    )
    do
        let burnedAddr      = getPubKeyHash $ pubKeyHash $ walletPubKey w3
            burnedAddrWrong = getPubKeyHash $ pubKeyHash $ walletPubKey w4
        pob1 <- activateContractWallet w1 endpoints
        void $ Emulator.waitNSlots 2
        callEndpoint @"burn" pob1 (burnedAddr, Ada.lovelaceValueOf 50_000_000)
        void $ Emulator.waitNSlots 2
        --
        pob2 <- activateContractWallet w2 endpoints
        void $ Emulator.waitNSlots 2
        callEndpoint @"validateBurn" pob2 burnedAddrWrong
        void $ Emulator.waitNSlots 2


-- | Test `burn` and `validateBurnWithAnswer` endpoints in pair.
--
--   Check that PoB can burn value to some address.
testBurnAndBurnedTrace5 :: TestTree
testBurnAndBurnedTrace5 = check "burn and validateBurn 5"
    (     walletFundsChange w1 (Ada.lovelaceValueOf (-50_000_000))
     .&&. walletFundsChange w2 (Ada.lovelaceValueOf 0)
     .&&. walletFundsChange w3 (Ada.lovelaceValueOf 0)
     .&&. assertInstanceLog (Emulator.walletInstanceTag w2) ((elem (burnedLogMsg 50_000_000)) . mapMaybe (preview (eteEvent . cilMessage . _ContractLog)))
     .&&. assertNoFailedTransactions
    )
    do
        let burnedAddr = getPubKeyHash $ pubKeyHash $ walletPubKey w3
        pob1 <- activateContractWallet w1 endpoints
        void $ Emulator.waitNSlots 2
        callEndpoint @"burn" pob1 (burnedAddr, Ada.lovelaceValueOf 50_000_000)
        void $ Emulator.waitNSlots 2
        --
        pob2 <- activateContractWallet w2 endpoints
        void $ Emulator.waitNSlots 2
        callEndpoint @"validateBurn" pob2 burnedAddr
        void $ Emulator.waitNSlots 2
        observableState pob2 >>= \case
            ContractStateAction (BurnedValueValidated (Just val)) _
               -> when ((Ada.lovelaceValueOf 50_000_000) /= val) $ Emulator.throwError (GenericError $ "Must be 50 ADA, but got: " ++ show val)
            _  -> Emulator.throwError (GenericError "Must return traced value")

-- | Test `burn`, `validateBurn`, `redeem` endpoints in pair.
--
--   Check that PoB can burn value to some address. Redeem of that value is not possible.
testBurnBurnedTraceAndRedeem :: TestTree
testBurnBurnedTraceAndRedeem = check "burn, validateBurn and redeem"
    (     walletFundsChange w1 (Ada.lovelaceValueOf (-50_000_000))
     .&&. walletFundsChange w2 (Ada.lovelaceValueOf 0)
     .&&. walletFundsChange w3 (Ada.lovelaceValueOf 0)
     .&&. assertInstanceLog (Emulator.walletInstanceTag w2) ((elem (burnedLogMsg 50_000_000)) . mapMaybe (preview (eteEvent . cilMessage . _ContractLog)))
     .&&. assertContractError endpoints (Emulator.walletInstanceTag w3) contractErrorPredicate ""
    )
    do
        let burnedAddr = getPubKeyHash $ pubKeyHash $ walletPubKey w3
        pob1 <- activateContractWallet w1 endpoints
        void $ Emulator.waitNSlots 2
        callEndpoint @"burn" pob1 (burnedAddr, Ada.lovelaceValueOf 50_000_000)
        void $ Emulator.waitNSlots 2
        --
        pob2 <- activateContractWallet w2 endpoints
        void $ Emulator.waitNSlots 2
        -- TODO check answer
        callEndpoint @"validateBurn" pob2 burnedAddr
        void $ Emulator.waitNSlots 2
        --
        pob3 <- activateContractWallet w3 endpoints
        void $ Emulator.waitNSlots 2
        callEndpoint @"redeem" pob3 ()
        void $ Emulator.waitNSlots 2
  where
    contractErrorPredicate (OtherError "No UTxO to redeem from") = True
    contractErrorPredicate _ = False


-- | Test `burn`'ing to same address gives the sum.
--
testBurnedTwice1 :: TestTree
testBurnedTwice1 = check "burn different values to same address"
    (     walletFundsChange w1 (Ada.lovelaceValueOf (-50_000_000))
     .&&. assertInstanceLog (Emulator.walletInstanceTag w1) ((elem (burnedLogMsg 50_000_000)) . mapMaybe (preview (eteEvent . cilMessage . _ContractLog)))
     .&&. assertNoFailedTransactions
    )
    do
        let burnedAddr = getPubKeyHash $ pubKeyHash $ walletPubKey w3
        --
        pob1 <- activateContractWallet w1 endpoints
        void $ Emulator.waitNSlots 2
        callEndpoint @"burn" pob1 (burnedAddr, Ada.lovelaceValueOf 15_000_000)
        void $ Emulator.waitNSlots 2
        callEndpoint @"burn" pob1 (burnedAddr, Ada.lovelaceValueOf 35_000_000)
        void $ Emulator.waitNSlots 2
        --
        pob2 <- activateContractWallet w1 endpoints
        callEndpoint @"validateBurn" pob2 burnedAddr
        void $ Emulator.waitNSlots 2

-- | Test `burn`'ing same value to same address gives the double value.
--
testBurnedTwice2 :: TestTree
testBurnedTwice2 = check "burn twice same values to same address"
    (     walletFundsChange w1 (Ada.lovelaceValueOf (-50_000_000))
     .&&. assertInstanceLog (Emulator.walletInstanceTag w1) ((elem (burnedLogMsg 50_000_000)) . mapMaybe (preview (eteEvent . cilMessage . _ContractLog)))
     .&&. assertNoFailedTransactions
    )
    do
        let burnedAddr = getPubKeyHash $ pubKeyHash $ walletPubKey w3
        --
        pob1 <- activateContractWallet w1 endpoints
        void $ Emulator.waitNSlots 2
        callEndpoint @"burn" pob1 (burnedAddr, Ada.lovelaceValueOf 25_000_000)
        void $ Emulator.waitNSlots 2
        callEndpoint @"burn" pob1 (burnedAddr, Ada.lovelaceValueOf 25_000_000)
        void $ Emulator.waitNSlots 2
        --
        pob2 <- activateContractWallet w1 endpoints
        callEndpoint @"validateBurn" pob2 burnedAddr
        void $ Emulator.waitNSlots 2


-- | Test `lock`, `burn` and `redeem` endpoints in pair, making
--   sure that a burn doesn't "mess"  with the redeeming.
testLockBurnRedeem :: TestTree
testLockBurnRedeem = check "lock, burn and redeem"
    (     walletFundsChange w1 (Ada.lovelaceValueOf (-80_000_000))
     .&&. walletFundsChange w2 (Ada.lovelaceValueOf 0)
     .&&. walletFundsChange w3 (Ada.lovelaceValueOf 30_000_000)
     .&&. assertInstanceLog (Emulator.walletInstanceTag w2) ((elem (burnedLogMsg 50_000_000)) . mapMaybe (preview (eteEvent . cilMessage . _ContractLog)))
    )
    do
        let burnedAddr = pubKeyHash $ walletPubKey w3
        --
        pob1 <- activateContractWallet w1 endpoints
        void $ Emulator.waitNSlots 2
        callEndpoint @"burn" pob1 (getPubKeyHash burnedAddr, Ada.lovelaceValueOf 50_000_000)
        void $ Emulator.waitNSlots 2
        callEndpoint @"lock" pob1 (burnedAddr, Ada.lovelaceValueOf 30_000_000)
        void $ Emulator.waitNSlots 2
        --
        pob2 <- activateContractWallet w2 endpoints
        void $ Emulator.waitNSlots 2
        callEndpoint @"validateBurn" pob2 (getPubKeyHash burnedAddr)
        void $ Emulator.waitNSlots 2
        --
        pob3 <- activateContractWallet w3 endpoints
        void $ Emulator.waitNSlots 2
        callEndpoint @"redeem" pob3 ()
        void $ Emulator.waitNSlots 2


-- | The message appeared in a smart contract log about successful burning tracing.
--
burnedLogMsg :: Integer -> JSON.Value
burnedLogMsg int = JSON.String ("Value burned with given commitment: " <> T.pack (show int))


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
    v = Ada.lovelaceValueOf 100_000_000


-- | Shortage for `checkPredicateOptions`: call this function with default params.
--
check :: String -> TracePredicate -> EmulatorTrace () -> TestTree
check = checkPredicateOptions (defaultCheckOptions & emulatorConfig .~ defaultEmCfg & minLogLevel .~ Log.Debug)

