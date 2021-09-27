{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE MultiWayIf                 #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}

-- | This is a burner contract,
--
-- And add function implementations (and rename them to
-- something suitable) for the endpoints:
--   * lock - which locks the value to be redeemed by an addressee;
--   * redeem - which redeems the locked value;
--   * burn - which burns the value instead of locking it.
--
--  In order to check that value was indeed burned, one needs to run a `burned` script with the same commitment value.
module ProofOfBurn where 

import           Control.Monad             (void, when)
import           Control.Lens              hiding (snoc, unsnoc)
import           Data.Bits                 (xor)
import           Data.Char                 (ord, chr)
import           Data.Sequences            (snoc, unsnoc)
import           Data.Maybe                (fromJust, catMaybes)
import qualified Data.Map.Strict           as Map
import qualified Data.List.NonEmpty        as NonEmpty(toList)
import           Plutus.Contract
import qualified PlutusTx
import qualified PlutusTx.IsData  as PlutusTx
import           PlutusTx.Prelude hiding (Applicative (..))
import           PlutusTx.Builtins.Internal (BuiltinByteString(..))
import           Ledger                    (Address, Datum(..), scriptAddress)
import           Ledger.AddressMap         (UtxoMap)
import qualified Ledger.Constraints        as Constraints
import qualified Ledger.Typed.Scripts      as Scripts
import           Ledger.Typed.Scripts.Validators(ValidatorType)
import           Ledger.Value              (Value, isZero, valueOf)
import           Ledger.Tx
import           Plutus.V1.Ledger.Crypto
import           Plutus.V1.Ledger.Contexts
import           Plutus.V1.Ledger.Tx (Tx(..), TxOutRef, TxOutTx(..), txData, txOutDatum)
import qualified Plutus.V1.Ledger.Scripts as Plutus
import           Playground.Contract
import qualified Data.Text                as T
import Wallet.Emulator.Wallet
import Ledger.Crypto
import qualified Prelude
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Lazy  as LBS
import           Codec.Serialise
import           Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1)
import           Plutus.ChainIndex.Tx

-- | Utxo datum holds either:
--     * the hash of the address that can redeem the value, if it was locked;
--     * the hash with the last bit flipped, if it was burned.
newtype MyDatum = MyDatum { fromMyDatum :: BuiltinByteString }
PlutusTx.makeLift ''MyDatum
PlutusTx.unstableMakeIsData ''MyDatum

-- | Redeemer holds no data, since the address is taken from the context.
newtype MyRedeemer = MyRedeemer { _nothing :: () }
PlutusTx.makeLift ''MyRedeemer
PlutusTx.unstableMakeIsData ''MyRedeemer

{-# INLINABLE validateSpend #-}
-- | Spending validator checks that hash of the redeeming address is the same as the Utxo datum.
validateSpend :: ValidatorType Burner
validateSpend (MyDatum addrHash) _myRedeemerValue ScriptContext { scriptContextTxInfo = txinfo } =
   traceIfFalse "owner has not signed" (addrHash `elem` fmap (sha3_256 . getPubKeyHash) (txInfoSignatories txinfo))

-- | The address of the contract (the hash of its validator script).
contractAddress :: Address
contractAddress = Ledger.scriptAddress (Scripts.validatorScript burnerTypedValidator)

-- | Type level tag for the Burner script.
data Burner
instance Scripts.ValidatorTypes Burner where
    type instance RedeemerType  Burner = MyRedeemer -- Argument given to redeem value, if possible (empty)
    type instance DatumType     Burner = MyDatum    -- Validator script argument

-- | The script instance is the compiled validator (ready to go onto the chain)
burnerTypedValidator :: Scripts.TypedValidator Burner
burnerTypedValidator = Scripts.mkTypedValidator @Burner
    $$(PlutusTx.compile [|| validateSpend ||])
    $$(PlutusTx.compile [|| wrap          ||])
  where
    wrap = Scripts.wrapValidator @MyDatum @MyRedeemer

-- | The schema of the contract, with two endpoints.
type Schema = Endpoint "lock"   (PubKeyHash, Value)        -- lock the value
          .\/ Endpoint "burn"   (BuiltinByteString, Value) -- burn the value
          .\/ Endpoint "burnedTrace"   BuiltinByteString   -- burn the value
          .\/ Endpoint "redeem" ()                         -- redeem the locked value

burnerScript :: Plutus.Script
burnerScript = Plutus.unValidatorScript burnerValidator

burnerValidator :: Plutus.Validator
burnerValidator = Scripts.validatorScript burnerTypedValidator

burnerSBS :: SBS.ShortByteString
burnerSBS = SBS.toShort . LBS.toStrict $ serialise burnerScript

burnerSerialised :: PlutusScript PlutusScriptV1
burnerSerialised = PlutusScriptSerialised burnerSBS

contract :: AsContractError e => Contract w Schema e ()
contract = selectList [lock, burn, burnedTrace, redeem]

-- | The "lock" contract endpoint.
--
--   Lock the value to the given addressee.
lock :: AsContractError e => Promise w Schema e ()
lock = endpoint @"lock" lock'

lock' :: AsContractError e => (PubKeyHash, Value) -> Contract w Schema e ()
lock' (addr, lockedFunds) = do
    let hash = sha3_256 $ getPubKeyHash addr
    let tx = Constraints.mustPayToTheScript (MyDatum hash) lockedFunds
    void $ submitTxConstraints burnerTypedValidator tx

-- | The "burn" contract endpoint.
--
--   Burn the value with the given commitment.
burn :: AsContractError e => Promise w Schema e ()
burn = endpoint @"burn" burn'

burn' :: AsContractError e => (BuiltinByteString, Value) -> Contract w Schema e ()
burn' (aCommitment, burnedFunds) = do
    let hash = flipCommitment $ sha3_256 aCommitment
    let tx = Constraints.mustPayToTheScript (MyDatum hash) burnedFunds
    void $ submitTxConstraints burnerTypedValidator tx

-- | Flip lowest bit in the commitment
--
--   Requires non-empty bytestring as argument
flipCommitment :: BuiltinByteString -> BuiltinByteString
flipCommitment (BuiltinByteString bs) = BuiltinByteString $ do
  case unsnoc bs of
    Nothing             -> traceError "Hash was empty" -- input was empty
    Just (seq, lastElt) -> snoc seq
                         $ lastElt `xor` 1

-- | The "redeem" contract endpoint.
--
--   Can redeem the value, if it was published, not burned.
redeem :: AsContractError e => Promise w Schema e ()
redeem = endpoint @"redeem" $ \() -> do
    unspentOutputs <- utxosAt contractAddress
    let redeemer = MyRedeemer ()
        tx       = collectFromScript unspentOutputs redeemer
    void $ submitTxConstraintsSpending burnerTypedValidator unspentOutputs tx


burnedTrace :: AsContractError e => Promise w Schema e ()
burnedTrace = endpoint @"burnedTrace" $ \aCommitment -> do
  burnedVal <- burned aCommitment
  if | isZero burnedVal -> logInfo @Prelude.String "Nothing burned with given commitment"
     | otherwise        -> logInfo @Prelude.String ("Value burned with given commitment: "  <> Prelude.show (valueOf burnedVal "" ""))

-- | The "burned" confirmation endpoint.
--
--   Confirms that the value was burned with the given commitment.
--   It returns total value locked with a given commitment.
burned :: AsContractError e => BuiltinByteString -> Contract w Schema e Value
burned aCommitment = do
    unspentOutputs :: Map.Map TxOutRef (ChainIndexTxOut, ChainIndexTx) <- utxosTxOutTxAt contractAddress
    return $ totalValue (withCommitment `Map.filter` unspentOutputs)
  where
    -- | Total value of Utxos in a `UtxoMap`.
    totalValue        :: Map.Map TxOutRef (ChainIndexTxOut, ChainIndexTx) -> Value
    totalValue         = foldOf (folded . _1 . _ScriptChainIndexTxOut . _4)
    -- | Check if the given transaction output is commited to burn with the same hash as `aCommitment`.
    withCommitment    :: (ChainIndexTxOut, ChainIndexTx) -> Bool
    withCommitment (PublicKeyChainIndexTxOut {}, _) = False
    withCommitment (ScriptChainIndexTxOut { _ciTxOutDatum = txOutDatumHash }, ChainIndexTx{ _citxData = txData })
      = burnHash == Just commitmentHash
      where
        -- | Extract hash from the Utxo.
        burnHash  :: Maybe BuiltinByteString
        burnHash   = do
          dh            <- either Just (const Nothing) txOutDatumHash
          Datum d       <- dh `Map.lookup` txData     -- lookup Datum from the hash
          MyDatum aHash <- PlutusTx.fromBuiltinData d -- Unpack from PlutusTx.Datum type.
          return           aHash
    commitmentHash = flipCommitment $ sha3_256 aCommitment


-- | Script endpoints available for calling the contract.
endpoints :: Contract () Schema T.Text ()
endpoints = contract

mkSchemaDefinitions ''Schema

$(mkKnownCurrencies [])
