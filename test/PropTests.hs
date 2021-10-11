{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
module PropTests where

-- TODO:
-- 1. Make `redeem` action work before `lock` -- and catch exception.
-- 2. Make `burnedTrace` action work when checking actual burned value
-- 3. Make tests with negative (?) values
-- 4. Lensify operations with states

import           Control.Lens                       hiding (elements)
import           Control.Monad                      (void, when)
import           Data.Default                       (Default (..))
import           Data.Map                           (Map)
import qualified Data.Map                           as Map
import           Data.Maybe                         (isJust)
import           Ledger                             hiding (singleton)
import           Ledger.Ada                         as Ada
import           Plutus.Contract                    (ContractError)
import           Plutus.Contract.Test
import           Plutus.Contract.Test.ContractModel
import           Plutus.Trace.Emulator as Emulator

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           ProofOfBurn

-- import qualified Debug.Trace as T


data POBModel = POBModel
    { _pobLocks :: Map Wallet Ada
    , _pobBurns :: Map Wallet Ada
    }
    deriving Show

makeLenses ''POBModel


-- | There were few issues related to single instances of contract.
--   That's why we check the interaction of several contract instances.
newtype ContractInstanceId = ContractInstanceId Int deriving (Show, Eq)

contractInstanceIds :: [ContractInstanceId]
contractInstanceIds = map ContractInstanceId [1,2,3]

genContractInstanceId :: Gen ContractInstanceId
genContractInstanceId =  elements contractInstanceIds


instance ContractModel POBModel where

    data Action POBModel
        = Lock        ContractInstanceId Wallet Wallet Ada
        | Redeem      ContractInstanceId        Wallet
        | Burn        ContractInstanceId Wallet Wallet Ada
        | BurnedTrace ContractInstanceId Wallet Wallet
        deriving (Show, Eq)

    data ContractInstanceKey POBModel w s e where
        POBKey           :: ContractInstanceId -> Wallet -> ContractInstanceKey POBModel ContractState ProofOfBurn.Schema ContractError
        POBWithAnswerKey :: ContractInstanceId -> Wallet -> ContractInstanceKey POBModel ContractState ProofOfBurn.Schema ContractError

    arbitraryAction _ = oneof $
        [ Lock   <$> genContractInstanceId <*> genWallet <*> genWallet <*> genAda
        , Redeem <$> genContractInstanceId <*> genWallet
        , Burn        <$> genContractInstanceId <*> genWallet <*> genWallet <*> genAda
        , BurnedTrace <$> genContractInstanceId <*> genWallet <*> genWallet
        ]

    initialState = POBModel Map.empty Map.empty

    nextState (Lock _cid wFrom wTo val) = do
        withdraw wFrom (toValue val)
        (pobLocks . at wTo) $~ (Just . maybe val (+val))
        wait 1

    nextState (Redeem _cid wTo) = do
        v <- askContractState (Map.lookup wTo . _pobLocks)
        case v of
            Nothing -> return ()
            Just (vv::Ada) -> do
                deposit wTo (toValue vv)
                (pobLocks . at wTo) $= Nothing
        wait 1

    nextState (Burn _cid wFrom wTo val) = do
        withdraw wFrom (toValue val)
        (pobBurns . at wTo) $~ (Just . maybe val (+val))
        wait 1

    nextState (BurnedTrace _cid _wFrom _wTo) = do
        -- Nothing to do
        wait 1

    perform h ms = \case
        (Lock cid wFrom wTo val) -> do
            callEndpoint @"lock"   (h $ POBKey cid wFrom) (pubKeyHash $ walletPubKey wTo, toValue val)
            delay 1
        (Redeem cid wTo) -> do
            callEndpoint @"redeem" (h $ POBKey cid wTo) ()
            delay 1
        (Burn cid wFrom wTo val) -> do
            callEndpoint @"burn" (h $ POBKey cid wFrom) (getPubKeyHash $ pubKeyHash $ walletPubKey wTo, toValue val)
            delay 1
        (BurnedTrace cid wFrom wTo) -> do
            let ch = h $ POBWithAnswerKey cid wFrom
            let (alreadyBurned :: Maybe Value)  = toValue <$> (ms ^. contractState . pobBurns . at wTo)
            callEndpoint @"validateBurn" ch (getPubKeyHash $ pubKeyHash $ walletPubKey wTo)
            delay 2
            observableState ch >>= \case
              ContractStateAction (BurnedValueValidated val) _ ->
                when (alreadyBurned /= val)
                  $ Emulator.throwError (GenericError $ "Must be " <> show alreadyBurned <> ", but got: " ++ show val)
              _ -> Emulator.throwError (GenericError "Must return traced value")
                {-
                T.traceM "&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&"
                T.traceM ("Must be equal; expected: " ++ Prelude.show alreadyBurned ++ "\nactual: " ++ Prelude.show actualBurned)
                T.traceM "-- MODEL: ----------------"
                T.traceShowM ms
                T.traceM "^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^"
                Prelude.error ("Must be equal; expected: " ++ Prelude.show alreadyBurned ++ "\nactual: " ++ Prelude.show actualBurned)
                -}

    precondition _ms (Lock _cid _wFrom _wTo _v) = True
    precondition ms (Redeem _cid       wTo)   =
        -- TODO remove this check: script will work with empty redeem (but it is need to catch error)
        isJust (ms ^. contractState . pobLocks . at wTo)
    precondition _ms (Burn _cid _wFrom _wTo _v) = True
    precondition _ms (BurnedTrace _cid _wFrom _wTo) = True

deriving instance Eq (ContractInstanceKey POBModel w s e)
deriving instance Show (ContractInstanceKey POBModel w s e)

delay :: Int -> EmulatorTrace ()
delay = void . waitNSlots . fromIntegral


-- We need at least 5 wallets, because we want to test at least this cases:
--
-- 1. lock from w1, w2 for w3;
-- 2. lock from w1 to w2, from w3 to w4, and w5 is not touched;
-- 3. lock from w1 to w2, burn from w3 for w4, and w5 wants to check.
wallets :: [Wallet]
wallets = [w1, w2, w3, w4, w5]

genWallet :: Gen Wallet
genWallet = elements wallets

genAda :: Gen Ada
genAda = (lovelaceOf . (+1)) <$> (getNonNegative <$> arbitrary)

--

instanceSpec :: [ContractInstanceSpec POBModel]
instanceSpec = concat
    [ [ ContractInstanceSpec (POBKey           cid w) w (ProofOfBurn.endpoints)
      , ContractInstanceSpec (POBWithAnswerKey cid w) w (ProofOfBurn.endpoints)
      ]
    | w   <- wallets
    , cid <- contractInstanceIds
    ]

-- * --------------------------------------------------------------------------

tests :: TestTree
tests = testProperty "prop tests" prop_POB


prop_POB :: Actions POBModel -> Property
prop_POB = withMaxSuccess 100 . propRunActionsWithOptions
    (defaultCheckOptions & emulatorConfig .~ EmulatorConfig (Left initDistr) def def)
    instanceSpec
    (const $ pure True)
  where
    initDistr :: InitialDistribution
    initDistr = Map.fromList [ (w, lovelaceValueOf 1_000_000_000)
                             | w <- wallets ]

