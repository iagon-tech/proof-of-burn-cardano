{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

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

import           Control.Monad             (void, when, forM, forM_)
import           Control.Applicative       (liftA3)
import Control.Lens ( foldOf, folded, Field1(_1), Field4(_4), to )
import           Data.Bits                 (xor)
import           Data.Functor              (($>))
import           Data.Char                 (ord, chr)
import qualified Data.Aeson                as Aeson
import           Data.Aeson.TH
import           Data.Sequences            (snoc, unsnoc)
import           Data.Maybe                (fromJust, catMaybes)
import qualified Data.Map.Strict           as Map
import qualified Data.List.NonEmpty        as NonEmpty(toList)
import Plutus.Contract
    ( (>>),
      logInfo,
      logError,
      endpoint,
      ownPubKeyHash,
      submitTxConstraints,
      submitTxConstraintsSpending,
      awaitTxConfirmed,
      utxosAt,
      utxosTxOutTxAt,
      collectFromScript,
      selectList,
      tell,
      Endpoint,
      Contract,
      Promise,
      AsContractError,
      type (.\/), ContractError (OtherError) )
import Plutus.ChainIndex.Client ()
import qualified PlutusTx
import qualified PlutusTx.IsData  as PlutusTx
import PlutusTx.Prelude
    ( otherwise,
      return,
      (>>=),
      Bool(False),
      Maybe(..),
      Either(Right, Left),
      (.),
      flip,
      sha3_256,
      either,
      elem,
      const,
      ($),
      traceError,
      traceIfFalse,
      BuiltinByteString,
      Eq((==)),
      Functor(fmap),
      Semigroup((<>)), Monoid(mempty), foldr, BuiltinString )
import           PlutusTx.Builtins.Internal (BuiltinByteString(..), decodeUtf8)
import           Ledger                    (Address(..), Datum(..), scriptAddress, datumHash)
import           Ledger.AddressMap         (UtxoMap)
import qualified Ledger.Constraints        as Constraints
import qualified Ledger.Typed.Scripts      as Scripts
import           Ledger.Typed.Scripts.Validators(ValidatorType)
import           Ledger.Value              (Value, isZero, valueOf)
import Ledger.Tx
    ( TxOutRef,
      ChainIndexTxOut(PublicKeyChainIndexTxOut, ScriptChainIndexTxOut,
                      _ciTxOutDatum),
      _ScriptChainIndexTxOut,
      getCardanoTxId,
      Address, getCardanoTxUnspentOutputsTx )
import Plutus.V1.Ledger.Crypto
    ( PubKey, PubKeyHash(getPubKeyHash) )
import Plutus.V1.Ledger.Credential ()
import Plutus.V1.Ledger.Contexts
    ( ScriptContext(ScriptContext, scriptContextTxInfo),
      TxInfo(txInfoSignatories) )
import           Plutus.V1.Ledger.Tx (Tx(..), TxOutRef, TxOutTx(..), TxOut(..), txData, txOutDatum)
import qualified Plutus.V1.Ledger.Scripts as Plutus
import Playground.Contract
    ( mkKnownCurrencies, mkSchemaDefinitions, KnownCurrency )
import qualified Data.Text                as T
import Wallet.Emulator.Wallet ()
import Ledger.Crypto ( pubKeyHash )
import qualified Prelude
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Lazy  as LBS
import Codec.Serialise ( serialise )
import           Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1)
import Plutus.ChainIndex.Tx
    ( ChainIndexTx(ChainIndexTx, _citxData) )
import           Cardano.Prelude ( Set, MonadError (throwError, catchError) )
import qualified Data.Set as Set
import Text.ParserCombinators.ReadPrec
import GHC.Read
import qualified Text.Read.Lex as L
import           Plutus.V1.Ledger.Address
import           Plutus.V1.Ledger.Crypto


-- | Utxo datum holds either:
--     * the hash of the address that can redeem the value, if it was locked;
--     * the hash with the last bit flipped, if it was burned.
newtype MyDatum = MyDatum { fromMyDatum :: BuiltinByteString }
  deriving (Prelude.Show)
PlutusTx.makeLift ''MyDatum
PlutusTx.unstableMakeIsData ''MyDatum

instance Prelude.Read MyDatum where
  readPrec =
    parens
    (do expectP (L.Ident "MyDatum")
        x <- step readPrec
        return $ MyDatum (BuiltinByteString x)
    )
  readListPrec = readListPrecDefault
  readList     = readListDefault


-- | Redeemer holds no data, since the address is taken from the context.
newtype MyRedeemer = MyRedeemer { _nothing :: () }
  deriving (Prelude.Read, Prelude.Show)
PlutusTx.makeLift ''MyRedeemer
PlutusTx.unstableMakeIsData ''MyRedeemer


data ContractAction =
    BurnedValueValidated (Maybe Value)
  | BurnedValue          Value BuiltinByteString
  | LockedValue          Value BuiltinByteString
  | Redeemed             Value BuiltinByteString
  deriving Prelude.Show

$(deriveJSON defaultOptions ''ContractAction)

data ContractState = ContractStateAction ContractAction ContractState
                   | None
  deriving Prelude.Show

$(deriveJSON defaultOptions ''ContractState)


instance Prelude.Monoid ContractState where
  mempty = None

instance Prelude.Semigroup ContractState where
  prevState <> (ContractStateAction a _s) = ContractStateAction a prevState
  prevState <> None                       = prevState

stateActions :: ContractState -> [ContractAction]
stateActions None = []
stateActions (ContractStateAction a s) = stateActions s <> [a]

previousState :: ContractState -> ContractState
previousState (ContractStateAction _ s) = s
previousState None = None

contractAction :: ContractState -> Maybe ContractAction
contractAction (ContractStateAction a _) = Just a
contractAction None = Nothing

tellAction :: ContractAction -> Contract ContractState Schema e ()
tellAction action = tell (ContractStateAction action None)

{-# INLINABLE validateSpend #-}
-- | Spending validator checks that hash of the redeeming address is the same as the Utxo datum.
validateSpend :: ValidatorType Burner
validateSpend (MyDatum addrHash) _myRedeemerValue ScriptContext { scriptContextTxInfo = txinfo } =
   traceIfFalse traceMsg (addrHash `elem` allPubkeyHashes)
 where
  requiredSigs :: [PubKeyHash]
  requiredSigs = txInfoSignatories txinfo
  allPubkeyHashes :: [BuiltinByteString]
  allPubkeyHashes = fmap (sha3_256 . getPubKeyHash) requiredSigs
  sigsS :: BuiltinString
  sigsS = "[" <> foldr (\a b -> decodeUtf8 a <> ", " <> b) mempty allPubkeyHashes <> "]"
  traceMsg :: BuiltinString
  traceMsg
    | allPubkeyHashes == [] = "No required signatures attached. Owner has not signed."
    | otherwise             = "Owner has not signed, expected " <> decodeUtf8 addrHash <> ", but got: " <> sigsS

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
type Schema = Endpoint "lock"          (PubKeyHash, Value)        -- lock the value
          .\/ Endpoint "burn"          (BuiltinByteString, Value) -- burn the value
          .\/ Endpoint "validateBurn"  BuiltinByteString          -- validate a burn
          .\/ Endpoint "redeem"        ()                         -- redeem the locked value

burnerScript :: Plutus.Script
burnerScript = Plutus.unValidatorScript burnerValidator

burnerValidator :: Plutus.Validator
burnerValidator = Scripts.validatorScript burnerTypedValidator

burnerSBS :: SBS.ShortByteString
burnerSBS = SBS.toShort . LBS.toStrict $ serialise burnerScript

burnerSerialised :: PlutusScript PlutusScriptV1
burnerSerialised = PlutusScriptSerialised burnerSBS

contract :: Contract ContractState Schema ContractError ()
contract = selectList [lock, burn, validateBurn, redeem] >> contract

-- | The "lock" contract endpoint.
--
--   Lock the value to the given addressee.
lock :: AsContractError e => Promise ContractState Schema e ()
lock = endpoint @"lock" lock'

lock' :: AsContractError e => (PubKeyHash, Value) -> Contract ContractState Schema e ()
lock' (addr, lockedFunds) = do
    pubk <- ownPubKeyHash
    let hash = sha3_256 $ getPubKeyHash addr
    let txConstraint = Constraints.mustPayToTheScript (MyDatum hash) lockedFunds <> Constraints.mustBeSignedBy pubk
    tx <- submitTxConstraints burnerTypedValidator txConstraint
    awaitTxConfirmed $ getCardanoTxId tx
    logInfo @Prelude.String "Tx locked"
    tellAction (LockedValue lockedFunds hash)

-- | The "burn" contract endpoint.
--
--   Burn the value with the given commitment.
burn :: AsContractError e => Promise ContractState Schema e ()
burn = endpoint @"burn" burn'

burn' :: AsContractError e => (BuiltinByteString, Value) -> Contract ContractState Schema e ()
burn' (aCommitment, burnedFunds) = do
    let hash = flipCommitment $ sha3_256 aCommitment
    let txConstraint = Constraints.mustPayToTheScript (MyDatum hash) burnedFunds
    tx <- submitTxConstraints burnerTypedValidator txConstraint
    awaitTxConfirmed $ getCardanoTxId tx
    logInfo @Prelude.String "Tx burned"
    tellAction (BurnedValue burnedFunds hash)

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
redeem :: Promise ContractState Schema ContractError ()
redeem = endpoint @"redeem" $ \() -> do
    pubk <- ownPubKeyHash
    unspentOutputs' <- utxosTxOutTxAt contractAddress
    let relevantOutputs = filterUTxOs unspentOutputs' (filterByPubKey pubk)
        txInputs = Map.map Prelude.fst $ relevantOutputs
    when (Map.null relevantOutputs) $ do
      logError @Prelude.String $ "No UTxO to redeem from"
      throwError (OtherError $ T.pack "No UTxO to redeem from")
    let redeemer = MyRedeemer ()
    vals <- forM (Map.toList txInputs) $ \(k, v) -> do
        let txInput = Map.singleton k v
            txConstraint = collectFromScript txInput redeemer <> Constraints.mustBeSignedBy pubk
        tx <- try $ submitTxConstraintsSpending burnerTypedValidator txInput txConstraint
        case tx of
          Left e -> do
            logError @Prelude.String $ "Error redeeming tx: " <> Prelude.show e
            return Nothing
          Right tx -> do
              awaitTxConfirmed . getCardanoTxId $ tx
              let val = foldOf (folded . to txOutValue) . getCardanoTxUnspentOutputsTx $ tx
              logInfo @Prelude.String ("Tx redeemed with value " <> Prelude.show val)
              return (Just val)
    tellAction (Redeemed (Prelude.mconcat $ catMaybes vals) (getPubKeyHash pubk))
 where
   filterUTxOs ::  Map.Map TxOutRef (ChainIndexTxOut, ChainIndexTx)
                -> ((ChainIndexTxOut, ChainIndexTx) -> Bool)
                -> Map.Map TxOutRef (ChainIndexTxOut, ChainIndexTx)
   filterUTxOs txMap p = flip Map.filter txMap p

   filterByPubKey :: PubKeyHash -> (ChainIndexTxOut, ChainIndexTx) -> Bool
   filterByPubKey pubk txtpl = fmap fromMyDatum (getMyDatum txtpl) == Just (sha3_256 (getPubKeyHash pubk))

validateBurn' :: AsContractError e => BuiltinByteString -> Contract ContractState Schema e ()
validateBurn' aCommitment = do
  burnedVal <- burned aCommitment
  if | isZero burnedVal -> do
        logInfo @Prelude.String "Nothing burned with given commitment"
        tellAction (BurnedValueValidated Nothing)
     | otherwise        -> do
        logInfo @Prelude.String ("Value burned with given commitment: "  <> Prelude.show (valueOf burnedVal "" ""))
        tellAction (BurnedValueValidated (Just burnedVal))


validateBurn :: Promise ContractState Schema ContractError ()
validateBurn = endpoint @"validateBurn" $ validateBurn'

-- | The "burned" confirmation endpoint.
--
--   Confirms that the value was burned with the given commitment.
--   It returns total value locked with a given commitment.
burned :: AsContractError e => BuiltinByteString -> Contract w Schema e Value
burned aCommitment = do
    unspentOutputs :: Map.Map TxOutRef (ChainIndexTxOut, ChainIndexTx) <- utxosTxOutTxAt contractAddress
    return $ totalValue (withCommitment `Map.filter` unspentOutputs)
  where
    -- | Check if the given transaction output is commited to burn with the same hash as `aCommitment`.
    withCommitment    :: (ChainIndexTxOut, ChainIndexTx) -> Bool
    withCommitment txtpl = fmap fromMyDatum (getMyDatum txtpl) == Just commitmentHash

    commitmentHash = flipCommitment $ sha3_256 aCommitment

-- | Total value of Utxos in a `UtxoMap`.
totalValue :: Map.Map TxOutRef (ChainIndexTxOut, ChainIndexTx) -> Value
totalValue = foldOf (folded . _1 . _ScriptChainIndexTxOut . _4)

getMyDatum :: (ChainIndexTxOut, ChainIndexTx) -> Maybe MyDatum
getMyDatum (PublicKeyChainIndexTxOut {}, _) = Nothing
getMyDatum (ScriptChainIndexTxOut { _ciTxOutDatum = Left dh }, ChainIndexTx{ _citxData = txData }) = do
  Datum d       <- dh `Map.lookup` txData     -- lookup Datum from the hash
  PlutusTx.fromBuiltinData d                  -- Unpack from PlutusTx.Datum type.
getMyDatum (ScriptChainIndexTxOut { _ciTxOutDatum = Right (Datum datum) }, _) =
   PlutusTx.fromBuiltinData datum


-- | Script endpoints available for calling the contract.
endpoints :: Contract ContractState Schema ContractError ()
endpoints = contract


try :: Contract w s e a -> Contract w s e (Either e a)
try a = catchError (fmap Right $ a) (\e -> return (Left e))


mkSchemaDefinitions ''Schema

$(mkKnownCurrencies [])
