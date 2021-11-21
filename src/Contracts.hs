{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Contracts(
      Contracts(..)
    , handlers
    ) where

import Control.Monad.Freer
import Data.Aeson (FromJSON, ToJSON)
import Data.Default (Default (def))
import GHC.Generics (Generic)
import Prettyprinter

import ProofOfBurn qualified as ProofOfBurn
import Data.OpenApi.Schema qualified as OpenApi
import Language.PureScript.Bridge (argonaut, equal, genericShow, mkSumType)
import Playground.Types (FunctionSchema)
import Plutus.PAB.Effects.Contract.Builtin (Builtin, BuiltinHandler (..), HasDefinitions (..), SomeBuiltin (..))
import Plutus.PAB.Effects.Contract.Builtin qualified as Builtin
import Plutus.PAB.Run.PSGenerator (HasPSTypes (..))
import Plutus.PAB.Simulator (SimulatorEffectHandlers)
import Plutus.PAB.Simulator qualified as Simulator
import Schema (FormSchema)

data Contracts = ProofOfBurn
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON, ToJSON, OpenApi.ToSchema)

instance Pretty Contracts where
    pretty = viaShow

instance HasPSTypes Contracts where
    psTypes =
        [ equal . genericShow . argonaut $ mkSumType @Contracts
        ]

instance HasDefinitions Contracts where
    getDefinitions = [ ProofOfBurn
                     ]
    getContract = getExampleContracts
    getSchema = getExampleContractsSchema

getExampleContractsSchema :: Contracts -> [FunctionSchema FormSchema]
getExampleContractsSchema = \case
    ProofOfBurn         -> Builtin.endpointsToSchemas @ProofOfBurn.Schema

getExampleContracts :: Contracts -> SomeBuiltin
getExampleContracts = \case
    ProofOfBurn         -> SomeBuiltin ProofOfBurn.contract

handlers :: SimulatorEffectHandlers (Builtin Contracts)
handlers =
    Simulator.mkSimulatorHandlers def def
    $ interpret (contractHandler Builtin.handleBuiltin)
