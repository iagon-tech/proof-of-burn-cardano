{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}
module PropTests where

import           Control.Lens                       hiding (elements)
import           Control.Applicative
import           Control.Monad                      (void, when)
import           Data.Default                       (Default (..))
import           Data.Map                           (Map)
import qualified Data.Map                           as Map
import           Data.Maybe                         (isJust, fromMaybe)
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
        = Lock         ContractInstanceId Wallet Wallet Ada
        | Redeem       ContractInstanceId        Wallet
        | Burn         ContractInstanceId Wallet Wallet Ada
        | ValidateBurn ContractInstanceId Wallet Wallet
        deriving (Show, Eq)

    data ContractInstanceKey POBModel w s e where
        POBKey :: ContractInstanceId -> Wallet -> ContractInstanceKey POBModel ContractState ProofOfBurn.Schema ContractError

    arbitraryAction _ = oneof $
        [ Lock         <$> genContractInstanceId <*> genWallet <*> genWallet <*> genAda
        , Redeem       <$> genContractInstanceId <*> genWallet
        , Burn         <$> genContractInstanceId <*> genWallet <*> genWallet <*> genAda
        , ValidateBurn <$> genContractInstanceId <*> genWallet <*> genWallet
        ]

    initialState = POBModel Map.empty Map.empty

    nextState (Lock _cid wFrom wTo val) = do
        withdraw wFrom (toValue val)
        (pobLocks . at wTo) $~ (Just . maybe val (+val))
        wait 1

    nextState (Redeem _cid wTo) = do
        askContractState (Map.lookup wTo . _pobLocks) >>= \case
            Nothing -> return ()
            Just (vv::Ada) -> do
                deposit wTo (toValue vv)
                (pobLocks . at wTo) $= Nothing
        wait 1

    nextState (Burn _cid wFrom wTo val) = do
        withdraw wFrom (toValue val)
        (pobBurns . at wTo) $~ (Just . maybe val (+val))
        wait 1

    nextState (ValidateBurn _cid _wFrom _wTo) = do
        -- Nothing to do
        wait 1

    perform h ms = \case
        (Lock cid wFrom wTo val) -> do
            callEndpoint @"lock"   (h $ POBKey cid wFrom) (walletPubKeyHash wTo, toValue val)
            delay 1
        (Redeem cid wTo) -> do
            callEndpoint @"redeem" (h $ POBKey cid wTo) ()
            delay 1
        (Burn cid wFrom wTo val) -> do
            callEndpoint @"burn" (h $ POBKey cid wFrom) (getPubKeyHash $ walletPubKeyHash wTo, toValue val)
            delay 1
        (ValidateBurn cid wFrom wTo) -> do
            let ch = h $ POBKey cid wFrom
                (alreadyBurned :: Maybe Value) = toValue <$> (ms ^. contractState . pobBurns . at wTo)
            callEndpoint @"validateBurn" ch (getPubKeyHash $ walletPubKeyHash wTo)
            delay 1
            observableState ch >>= \case
                ContractStateAction (BurnedValueValidated val) _ ->
                    when (alreadyBurned /= val) $
                        Emulator.throwError (GenericError $ "Must be " <> show alreadyBurned <> ", but got: " ++ show val)
                _ -> Emulator.throwError (GenericError "Must return traced value")
            delay 2

    precondition _ms (Lock _cid _wFrom _wTo _v) = True
    precondition ms (Redeem _cid       wTo)   =
        isJust (ms ^. contractState . pobLocks . at wTo)
    precondition _ms (Burn _cid _wFrom _wTo _v) = True
    precondition _ms (ValidateBurn _cid _wFrom _wTo) = True


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
instanceSpec =
    [ ContractInstanceSpec (POBKey cid w) w (ProofOfBurn.endpoints)
    | w   <- wallets
    , cid <- contractInstanceIds
    ]

-- * --------------------------------------------------------------------------

tests :: TestTree
tests = testProperties "prop tests"
    [ ("burn validating in emulator is works",  prop_BurnValidatingInEmulatorIsWorks)
    , ("observable state don't break emulator", prop_GetObservableStateDon'tBreakEmulator)
    , ("lock and then redeem",                  prop_LockAndRedeem)
    , ("lock small values",                     prop_LockSmallValues)
    , ("random actions is consistent",          prop_RandomActionsIsConsistent)
    ]


prop_BurnValidatingInEmulatorIsWorks :: Property
prop_BurnValidatingInEmulatorIsWorks = withMaxSuccess 1 $ withDLTest anyActions_ mkPropForActions $
    DLScript (
        [ Do $ Burn cid w1 w3 10000
        , Do $ Burn cid w2 w3 10000
        ]
        ++
        [ Do $ ValidateBurn cid ww1 ww2 | ww1 <- wallets, ww2 <- wallets ])
    where
      cid = ContractInstanceId 1


prop_GetObservableStateDon'tBreakEmulator :: Property
prop_GetObservableStateDon'tBreakEmulator = withMaxSuccess 1 $ withDLTest anyActions_ mkPropForActions $
    DLScript
        [ Do $ ValidateBurn (ContractInstanceId 2) w1 w2
        , Do $ Burn         (ContractInstanceId 1) w1 w2 10000
        , Do $ Lock         (ContractInstanceId 2) w1 w2 20000
        ]


prop_RandomActionsIsConsistent :: Property
prop_RandomActionsIsConsistent = withMaxSuccess 100 $ forAllDL anyActions_ mkPropForActions

instance Arbitrary (Action POBModel) where
    arbitrary = arbitraryAction undefined

prop_LockAndRedeem :: Property
prop_LockAndRedeem = withMaxSuccess 10 $ forAllDL myTest mkPropForActions
  where
    myTest = do
        action $ Lock (ContractInstanceId 1) w1 w2 20000
        anyBurnValidateActions_
        (_alreadyBurned :: Ada) <- (\ms -> fromMaybe 0 $ (ms ^. contractState . pobBurns . at w2)) <$> getModelState
        action $ Redeem (ContractInstanceId 2) w2
    anyBurnValidateActions_ :: DL POBModel ()
    anyBurnValidateActions_ = stopping <|> (anySpecifiedAction (liftA2 (||) isBurn isValidateBurn) >> anyBurnValidateActions_)
    anySpecifiedAction :: (Action POBModel -> Bool) -> DL POBModel ()
    anySpecifiedAction predicate = forAllQ (whereQ arbitraryQ predicate) >>= action
    isBurn Burn{}                 = True
    isBurn _                      = False
    isValidateBurn ValidateBurn{} = True
    isValidateBurn _              = False


mkPropForActions :: Actions POBModel -> Property
mkPropForActions = propRunActionsWithOptions
    (defaultCheckOptions & emulatorConfig .~ EmulatorConfig (Left initDistr) def def)
    instanceSpec
    (const $ pure True)
  where
    initDistr :: InitialDistribution
    initDistr = Map.fromList [ (w, lovelaceValueOf 1_000_000_000)
                             | w <- wallets ]


prop_LockSmallValues :: Property
prop_LockSmallValues = withMaxSuccess 1 $ withDLTest anyActions_ mkPropForActions $
    DLScript
        [ Do $ Lock   (ContractInstanceId 1) w1 w2 (Lovelace {getLovelace = 1000})
        , Do $ Lock   (ContractInstanceId 2) w1 w2 (Lovelace {getLovelace = 2000})
        , Do $ Redeem (ContractInstanceId 3) w2
        ]

