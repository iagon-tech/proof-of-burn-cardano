{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# OPTIONS_GHC -fno-cse        #-}

module Main where

import           Prelude
import Cardano.Api
    ( makeShelleyAddress,
      scriptDataToJson,
      serialiseToTextEnvelope,
      writeFileTextEnvelope,
      SerialiseAddress(serialiseAddress),
      StakeAddressReference(NoStakeAddress),
      Error(displayError),
      NetworkId(Mainnet, Testnet),
      PlutusScript,
      PlutusScriptV1,
      ScriptData(ScriptDataList, ScriptDataConstructor),
      ScriptDataJsonSchema(ScriptDataJsonDetailedSchema),
      HasTextEnvelope,
      TextEnvelopeDescr,
      NetworkMagic(NetworkMagic) )
import Cardano.Api.Shelley
    ( fromShelleyPaymentCredential, fromPlutusData, toAlonzoData )
import qualified Cardano.Ledger.Alonzo.Data as Alonzo
import qualified Plutus.V1.Ledger.Api as Plutus
import qualified Data.Aeson as Aeson
import           Data.Aeson.Encode.Pretty
                   (Config (..), encodePretty', defConfig, keyOrder)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import           Data.Text (Text)
import           Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy as L
import System.Console.CmdArgs
    ( Data,
      Typeable,
      (&=),
      cmdArgs,
      modes,
      argPos,
      help,
      name,
      program,
      summary,
      typ,
      typFile,
      Default(def) )
import           System.IO ( hPrint, hPutStrLn, hPutStr, stderr )
import ProofOfBurn
    ( burnerSBS, burnerSerialised, MyDatum, MyRedeemer )
import Cardano.Binary
  ( ToCBOR (..) )
import Cardano.Crypto.Hash
  ( HashAlgorithm
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
import System.Environment
  ( lookupEnv )
import qualified Cardano.Crypto.Hash as Hash
import qualified Cardano.Ledger.Credential as Shelley
import qualified Cardano.Ledger.Crypto as Crypto
import qualified Cardano.Ledger.Keys as Ledger
import qualified Data.Text as T
import qualified Data.Text.IO as T


data Command = GeneratePlutus   { dest     :: Maybe FilePath }
             | EncodeRedeemer   { redeemer :: String         }
             | EncodeDatum      { datum    :: String         }
             | GenerateBurnAddr { comm     :: String         }
              deriving (Show, Data, Typeable)


main :: IO ()
main = do
  cmdArgs (modes [generatePlutus, generateBurn, encodeDatum, encodeRedeemer]
      &= help "plutus burner" &= program "plutus-burner" &= summary "Plutus burner cli") >>= \case
    GeneratePlutus dest' -> writePlutusScript dest' burnerSerialised burnerSBS
    EncodeDatum datum' -> do
      let d = encodeData
            . read @MyDatum
            $ datum'
      putStrLn d
    EncodeRedeemer redeemer' -> do
      let d = encodeData
            . read @MyRedeemer
            $ redeemer'
      putStrLn d
    GenerateBurnAddr comm' -> generateBurnAddress (LBS.toStrict . encodeUtf8 . L.pack $ comm')
 where
  generatePlutus :: Command
  generatePlutus = GeneratePlutus {
      dest = Nothing &= name "d" &= name "dest" &= typFile &= help "Script file destination"
    } &= help "Generate the plutus script" &= name "generate-plutus"

  generateBurn :: Command
  generateBurn = GenerateBurnAddr {
      comm = def &= typ "DATUM" &= argPos 0
    } &= help "Generate a burn address for non-smart contract use" &= name "generate-addr"

  encodeRedeemer :: Command
  encodeRedeemer = EncodeRedeemer {
      redeemer = def &= typ "REDEEMER" &= argPos 0
    } &= help "Encode a datum or a redeemer as JSON" &= name "encode-redeemer"

  encodeDatum :: Command
  encodeDatum = EncodeDatum {
      datum = def &= typ "DATUM" &= argPos 0
    } &= help "Encode a datum as JSON" &= name "encode-datum"

  encodeData :: Plutus.ToData a => a -> String
  encodeData = L.unpack
             . decodeUtf8
             . Aeson.encode
             . scriptDataToJson ScriptDataJsonDetailedSchema
             . fromPlutusData
             . Plutus.builtinDataToData
             . Plutus.toBuiltinData

generateBurnAddress :: ByteString -> IO ()
generateBurnAddress datum' = do
  mnet <- lookupEnv "NETWORK"
  network <- case T.unpack . T.toLower . T.pack <$> mnet of
        Just "testnet" -> do
          putStderrLn "Picking testnet with network magic 1097911063"
          pure $ Testnet (NetworkMagic 1097911063)
        Just "mainnet" -> pure Mainnet
        Just net       -> fail ("Don't know network " <> net)
        Nothing        -> fail "No network specified. Set env var NETWORK=<testnet|mainnet>"

  commitment <- flipCommitment' . sha3_256 $ datum'
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

  flipCommitment' :: MonadFail m => ByteString -> m ByteString
  flipCommitment' bs = do
    case BS.unsnoc bs of
      Nothing             -> fail "Hash was empty" -- input was empty
      Just (seq', lastElt) -> pure . BS.snoc seq' . xor lastElt $ 1

  sha3_256 :: ByteString -> ByteString
  sha3_256 = digest (Proxy @SHA3_256)

writePlutusScript :: Maybe FilePath -> PlutusScript PlutusScriptV1 -> SBS.ShortByteString -> IO ()
writePlutusScript filename scriptSerial scriptSBS =
  do
  case Plutus.defaultCostModelParams of
        Just m ->
          let Alonzo.Data pData = toAlonzoData (ScriptDataConstructor 0 [ScriptDataList []])
              (logout, e) = Plutus.evaluateScriptCounting Plutus.Verbose m scriptSBS [pData]
          in do putStderr ("Log output: " :: String) >> printStderr logout
                case e of
                  Left evalErr -> putStderr ("Eval Error: " :: String) >> printStderr evalErr
                  Right exbudget -> putStderr ("Ex Budget: " :: String) >> printStderr exbudget
        Nothing -> error "defaultCostModelParams failed"
  case filename of
    Nothing -> BS.putStrLn (textEnvelopeToJSON Nothing scriptSerial)
    Just filename' -> do
      result <- writeFileTextEnvelope filename' Nothing scriptSerial
      case result of
        Left err -> printStderr $ displayError err
        Right () -> return ()
 where
  -- https://github.com/input-output-hk/cardano-node/pull/3347
  textEnvelopeToJSON :: HasTextEnvelope a =>  Maybe TextEnvelopeDescr -> a -> ByteString
  textEnvelopeToJSON mbDescr a  =
    LBS.toStrict $ encodePretty' textEnvelopeJSONConfig
                                 (serialiseToTextEnvelope mbDescr a)
              <> "\n"

  textEnvelopeJSONConfig :: Config
  textEnvelopeJSONConfig = defConfig { confCompare = textEnvelopeJSONKeyOrder }

  textEnvelopeJSONKeyOrder :: Text -> Text -> Ordering
  textEnvelopeJSONKeyOrder = keyOrder ["type", "description", "cborHex"]

printStderr :: Show a => a -> IO ()
printStderr = hPrint stderr . show

putStderrLn :: String -> IO ()
putStderrLn = hPutStrLn stderr

putStderr :: String -> IO ()
putStderr = hPutStr stderr
