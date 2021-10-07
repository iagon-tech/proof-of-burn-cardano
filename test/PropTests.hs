{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE NamedFieldPuns        #-}
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
import           Control.Exception.Lens
import           Control.Monad                      (void, when)
import           Data.Default                       (Default (..))
import           Data.Map                           (Map)
import qualified Data.Map                           as Map
import           Data.Maybe                         (isJust, isNothing)
import           Data.Monoid                        (Last (..))
import           Data.String                        (IsString (..))
import           Data.Text                          (Text)
import           Plutus.Contract                    (ContractError)
import           Plutus.Contract.Test
import           Plutus.Contract.Test.ContractModel
import           Plutus.Trace.Emulator
import           Ledger                             hiding (singleton)
import           Ledger.Ada                         as Ada
import           Ledger.Value

-- import Control.Eff.Exception as Eff
import Control.Monad.Freer.Error

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           ProofOfBurn


--data POBState = POBState
--    { _psLocks :: !Map Wallet Integer
--    } deriving Show

--makeLenses ''POBState
--

-- TODO use `Value` (or even `Ada Value`) instead of `Integer`


data POBModel = POBModel { _pobLocks :: Map Wallet Integer }
    deriving Show

makeLenses ''POBModel


instance ContractModel POBModel where

    data Action POBModel
        = Lock Wallet Wallet Integer
        | Redeem Wallet
        deriving (Show, Eq)


    data ContractInstanceKey POBModel w s e where
        -- TODO So we can't use contract twice?? Well, ok, let's use current slot to make new intsance
        POBKey :: Wallet -> Slot -> ContractInstanceKey POBModel () ProofOfBurn.Schema ContractError

    arbitraryAction _ = oneof $
        [ Lock   <$> genWallet <*> genWallet <*> genNonNeg -- TODO gen any number, even negative and zero
        , Redeem <$> genWallet
        ]

    initialState = POBModel Map.empty

    nextState (Lock wFrom wTo v) = do
        withdraw wFrom (Ada.lovelaceValueOf v)
        (pobLocks . at wTo) $~ (Just . maybe v (+v))
        wait 1

    nextState (Redeem wTo) = do
        v <- askContractState (Map.lookup wTo . _pobLocks) -- TODO change to lens
        case v of
            Nothing -> return ()
            Just (vv::Integer) -> do
                deposit wTo (Ada.lovelaceValueOf vv)
                (pobLocks . at wTo) $= Nothing -- TODO change to deletion via lens
        wait 1


    perform h ms cmd = do
        let slot = ms ^. Plutus.Contract.Test.ContractModel.currentSlot
        case cmd of
            (Lock   wFrom wTo v) -> callEndpoint @"lock"   (h $ POBKey wFrom slot) (pubKeyHash $ walletPubKey wTo, Ada.lovelaceValueOf v) >> delay 1
            (Redeem       wTo)   -> 
                -- TODO here it is need to catch and process error from endpoint
                callEndpoint @"redeem" (h $ POBKey wTo slot)   () >> delay 1
                -- catchError (callEndpoint @"redeem" (h $ POBKey wTo slot)   ()) (\(_:: Error EmulatorRuntimeError ) -> undefined)
                -- catching _ContractError (callEndpoint @"redeem" (h $ POBKey wTo slot)   ()) (\_ -> undefined)
                -- >> delay 1

    precondition s (Lock wFrom wTo v) = True
    precondition s (Redeem     wTo)   =
        -- TODO remove this check: script will work with empty redeem (but it is need to catch error)
        isJust (s ^. contractState . pobLocks . at wTo)

deriving instance Eq (ContractInstanceKey POBModel w s e)
deriving instance Show (ContractInstanceKey POBModel w s e)

delay :: Int -> EmulatorTrace ()
delay = void . waitNSlots . fromIntegral

wallets :: [Wallet]
wallets = [w1, w2, w3] -- TODO add more wallets

genWallet :: Gen Wallet
genWallet = elements wallets

genNonNeg :: Gen Integer
genNonNeg = (+1) <$> (getNonNegative <$> arbitrary)


instanceSpec :: [ContractInstanceSpec POBModel]
instanceSpec =
    [ContractInstanceSpec (POBKey w s) w ProofOfBurn.endpoints | w <- wallets
                                                               , s <- map Slot [0..100] ] -- TODO it seems very weird to make contract for every `lock`...
                                                                                          --      another way is to precreate some contracts for `lock` only and use counter
                                                                                          --      in model, not `currentSlot`
                                                                                          --
                                                                                          -- Another problem is that instances make very slow
                                                                                          --
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
