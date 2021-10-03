{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module PropTests where


import           Control.Lens                       hiding (elements)
import           Control.Monad                      (void, when)
import           Data.Default                       (Default (..))
import           Data.Map                           (Map)
import qualified Data.Map                           as Map
import           Data.Maybe                         (isJust, isNothing)
import           Data.Monoid                        (Last (..))
import           Data.String                        (IsString (..))
import           Data.Text                          (Text)
import           Plutus.Contract.Test
import           Plutus.Contract.Test.ContractModel
import           Plutus.Trace.Emulator
import           Ledger                             hiding (singleton)
import           Ledger.Ada                         as Ada
import           Ledger.Value

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           ProofOfBurn


data POBState = POBState
    { _psVal :: !Integer
    } deriving Show

makeLenses ''POBState


newtype POBModel = POBModel { _pobModel :: Map Wallet POBState }
    deriving Show

makeLenses ''POBModel


instance ContractModel POBModel where

    data Action POBModel
        = Lock Wallet Wallet Integer
        | Redeem Wallet
        deriving (Show, Eq)


    data ContractInstanceKey POBModel w s e where
        POBKey :: Wallet -> ContractInstanceKey POBModel () ProofOfBurn.Schema Text

    arbitraryAction _ = oneof $
        [ Lock   <$> genWallet <*> genWallet <*> genNonNeg -- TODO gen any number, even negative and zero
        -- , Redeem <$> genWallet
        ]

    initialState = POBModel Map.empty

    nextState (Lock wFrom wTo v) = do
        -- do something
        withdraw wFrom (Ada.lovelaceValueOf v)
        wait 1

    nextState (Redeem w) = do
        -- do something
        -- TODO catch exception from wrong `redeem` :-/
        wait 1


    perform h _ cmd = case cmd of
        (Lock   wFrom wTo v) -> callEndpoint @"lock"   (h $ POBKey wFrom) (pubKeyHash $ walletPubKey wTo, Ada.lovelaceValueOf v) >> delay 1
        -- TODO test redeem from SOME wallets (+ to unit tests)
        (Redeem       wTo)   -> callEndpoint @"redeem" (h $ POBKey wTo)   ()                                                     >> delay 1

    -- TODO
    precondition s (Lock wFrom wTo v) = True
    precondition s (Redeem     wTo)   = True

deriving instance Eq (ContractInstanceKey POBModel w s e)
deriving instance Show (ContractInstanceKey POBModel w s e)

delay :: Int -> EmulatorTrace ()
delay = void . waitNSlots . fromIntegral

wallets :: [Wallet]
wallets = [w1, w2, w3, w4]

genWallet :: Gen Wallet
genWallet = elements wallets

genNonNeg :: Gen Integer
genNonNeg = (+1) <$> (getNonNegative <$> arbitrary)


instanceSpec :: [ContractInstanceSpec POBModel]
instanceSpec =
    [ContractInstanceSpec (POBKey w) w ProofOfBurn.endpoints | w <- wallets]

-- * --------------------------------------------------------------------------

tests :: TestTree
tests = testProperty "PoB" prop_POB


prop_POB :: Actions POBModel -> Property
prop_POB = withMaxSuccess 1 . propRunActionsWithOptions
    (defaultCheckOptions & emulatorConfig .~ EmulatorConfig (Left initDistr) def def)
    instanceSpec
    (const $ pure True)
  where
    initDistr :: InitialDistribution
    initDistr = Map.fromList [ (w, lovelaceValueOf 1_000_000_000)
                             | w <- wallets ]
