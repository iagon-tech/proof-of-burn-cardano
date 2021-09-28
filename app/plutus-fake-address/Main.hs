{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

{-|
Module      : Main
Description : Creating a fake address
Copyright   : (c) Migamake, 2021
License     : proprietary
Maintainer  : hasufell@hasufell.de
Stability   : experimental
Portability : unix

Create a fake address for burning funds.
-}
module Main where

import Cardano.Api
  ( makeShelleyAddress
  , SerialiseAddress(serialiseAddress)
  , StakeAddressReference(NoStakeAddress)
  , NetworkId(Testnet, Mainnet)
  , NetworkMagic(NetworkMagic)
  )
import Cardano.Api.Shelley
  ( fromShelleyPaymentCredential )
import Cardano.Binary
  ( ToCBOR (..) )
import Cardano.Crypto.Hash
  ( ByteString
  , Hash(UnsafeHash)
  , HashAlgorithm
  )
import Cardano.Crypto.Hash.Class
  ( digest )
import Cardano.Crypto.Hash.SHA3_256
  ( SHA3_256 )
import Data.Bits
  ( xor )
import Data.Coerce
  ( coerce )
import Data.Proxy
  ( Proxy(..) )
import GHC.IO.Handle.FD
  ( stderr )
import GHC.IO.Handle.Text
  ( hPutStrLn )
import System.Environment
  ( lookupEnv )
import System.Posix.Env.ByteString
  ( getArgs )

import qualified Cardano.Crypto.DSIGN as DSIGN
import qualified Cardano.Crypto.DSIGN.Ed25519 as Ed25519
import qualified Cardano.Crypto.Hash as Hash
import qualified Cardano.Ledger.Credential as Shelley
import qualified Cardano.Ledger.Crypto as Crypto
import qualified Cardano.Ledger.Keys as Ledger
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.IO as T


main :: IO ()
main = do
  mnet <- lookupEnv "NETWORK"
  network <- case T.unpack . T.toLower . T.pack <$> mnet of
        Just "testnet" -> do
          hPutStrLn stderr "Picking testnet with network magic 1097911063"
          pure $ Testnet (NetworkMagic 1097911063)
        Just "mainnet" -> pure Mainnet
        Just net       -> fail ("Don't know network " <> net)
        Nothing        -> fail "No network specified. Set env var NETWORK=<testnet|mainnet>"
  (arg:_) <- getArgs

  commitment <- flipCommitment . sha3_256 $ arg
  let address = serialiseAddress
        $ makeShelleyAddress
            network
            (fromShelleyPaymentCredential (Shelley.KeyHashObj (mkKeyHash commitment)))
            NoStakeAddress
  T.putStr address

 where
  mkDummyHash :: forall h a. HashAlgorithm h => Proxy h -> ByteString -> Hash.Hash h a
  mkDummyHash _ = coerce . Ledger.hashWithSerialiser @h toCBOR

  mkKeyHash :: forall c discriminator. Crypto.Crypto c => ByteString -> Ledger.KeyHash discriminator c
  mkKeyHash = Ledger.KeyHash . mkDummyHash (Proxy @(Crypto.ADDRHASH c))

  flipCommitment :: MonadFail m => ByteString -> m ByteString
  flipCommitment bs = do
    case BS.unsnoc bs of
      Nothing             -> fail "Hash was empty" -- input was empty
      Just (seq, lastElt) -> pure . BS.snoc seq . xor lastElt $ 1

  sha3_256 :: BS.ByteString -> BS.ByteString
  sha3_256 = digest (Proxy @SHA3_256)
