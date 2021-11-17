# Creating smart contract transactions via CLI

Since Cardano's [smart contract support](https://iohk.io/en/blog/posts/2021/09/12/today-will-feel-like-a-destination-yet-a-new-exciting-journey-begins/),
there have been [various](https://plutus-pioneer-program.readthedocs.io) [tutorials](https://github.com/input-output-hk/Alonzo-testnet) and
[documentation](https://developers.cardano.org/docs/smart-contracts/plutus) [resources](https://docs.cardano.org/plutus/learn-about-plutus)
about how to create smart contracts.

In my experience, there are still a lot of missing gaps to fill and gotchas to be aware of. Since the Plutus PAB backend is still WIP,
although there exists a [pre-release](https://github.com/input-output-hk/plutus-apps/releases/tag/v2021-11-05) (date: 2021-11-17),
I want to give an overview of how we managed to deploy and run the [proof-of-burn](https://github.com/iagon-tech/proof-of-burn-cardano)
contract against the testnet.

The important part to understand about this approach is that this does not allow running actual plutus PAB endpoints you've written
in your smart contract. The only thing re-used from your smart contract will be the
[transaction validator](https://docs.cardano.org/plutus/Plutus-validator-scripts) (also called "Plutus script"), that is run
when constructing and validating transactions on the chain.

## Prerequisites

First, we'll need:

1. a running [node](https://github.com/input-output-hk/cardano-node) to submit transactions
2. a running [wallet](https://github.com/input-output-hk/cardano-wallet) to create transactions, do coin selection etc.
3. some chain indexer (either local or remote like [blockfrost](https://blockfrost.io/)), so we can query script UTxOs etc.

Then we need these additional cardano cli tools:

1. [`cardano-cli`](https://github.com/input-output-hk/cardano-node/tree/master/cardano-cli): main tool to manaddraddresses
2. [`cardano-address`](https://github.com/input-output-hk/cardano-addresses/tree/master/command-line): some additional address functionality
3. [`bech32`](https://github.com/input-output-hk/bech32): for decoding

Then we need a plethora of command line utility tools:

1. [`jq`](https://stedolan.github.io/jq/) for command line json processing
2. `xxd` for dealing with hex strings
3. `bc` to convert hex strings to binary representation
4. `openssl` for some hashing functionality
5. `curl` for making HTTP requests to our wallet and chain indexer

It can be difficult to figure out the correct combination of tools. We used:

1. wallet, `cardano-address` and `bech32` from [wallet 2021-09-29 release](https://github.com/input-output-hk/cardano-wallet/releases/tag/v2021-09-29) (just unpack the release tarball)
2. node and `cardano-cli` from commit [9dd31b3f8f17fba30882e98bb02810a7a504ba38](https://github.com/input-output-hk/cardano-node/commit/9dd31b3f8f17fba30882e98bb02810a7a504ba38) (has to be built from source)

It's possible that the latest wallet and node releases are fine too.

## Extracting the "Plutus script"

And finally, we need an actual smart contract, from which we can extract the plutus script. We'll use the example
[proof-of-burn contract](https://github.com/iagon-tech/proof-of-burn-cardano/blob/main/src/ProofOfBurn.hs).

In the contract, we have our `TypedValidator`, which is the Plutus Script definition:

```hs
burnerTypedValidator :: Scripts.TypedValidator Burner
burnerTypedValidator = Scripts.mkTypedValidator @Burner
    $$(PlutusTx.compile [|| validateSpend ||])
    $$(PlutusTx.compile [|| wrap          ||])
  where
    wrap = Scripts.wrapValidator @MyDatum @MyRedeemer
```

We then turn this into a serialized script:

```hs
import Codec.Serialise          (serialise )
import Cardano.Api.Shelley      (PlutusScript (..), PlutusScriptV1)
import Plutus.V1.Ledger.Scripts (unValidatorScript)
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Lazy  as LBS

burnerSerialised :: PlutusScript PlutusScriptV1
burnerSerialised = PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . Plutus.unValidatorScript $ burnerValidator
```

In our `main`, we can write it to a file:

```hs
import Cardano.Api (writeFileTextEnvelope, Error(displayError))

main :: IO ()
main = do
  result <- writeFileTextEnvelope "result.plutus" Nothing burnerSerialised
  case result of
    Left err -> print $ displayError err
    Right () -> return ()
```

This file `result.plutus` will be used when submitting a transaction, getting the script address etc.

## Starting wallet, node etc.

We'll not cover how to start wallet, node etc. These are explained in the respective documentations:

1. https://github.com/input-output-hk/cardano-node/blob/master/README.rst
2. https://input-output-hk.github.io/cardano-wallet/user-guide/cli

Make sure to use the testnet and use the correct
[configuration files](https://hydra.iohk.io/job/Cardano/cardano-node/cardano-deployment/latest-finished/download/1/index.html).

## Creating transactions

The biggest issue when creating transactions on the command line is getting the datum/redeemers right. These must be passed to
`cardano-cli` in some JSON format, which is a bit underdocumented (as can be seen [here](https://github.com/input-output-hk/plutus-apps/issues/16)).

We assume that a wallet has already been created/restored and that it has some funds. If you haven't added some,
use the testnet faucet: https://testnets.cardano.org/en/testnets/cardano/tools/faucet/

### Verifying JSON schema of datum/redeemer

In your contract repo, start a repl, e.g. via `cabal repl --build-depends cardano-api --build-depends aeson`,
then construct an example datum, e.g. for our example contract:

```
Prelude ProofOfBurn> :set -XOverloadedStrings
Prelude ProofOfBurn> import Cardano.Api
Prelude Cardano.Api ProofOfBurn> import Ledger.Scripts
Prelude Cardano.Api Ledger.Scripts ProofOfBurn> import qualified PlutusTx
Prelude Cardano.Api Ledger.Scripts PlutusTx ProofOfBurn> import qualified Data.Aeson as Aeson
Prelude Cardano.Api Ledger.Scripts PlutusTx Aeson ProofOfBurn> let datum = MyDatum "test"
Prelude Cardano.Api Ledger.Scripts PlutusTx Aeson ProofOfBurn> Aeson.encode . scriptDataToJson ScriptDataJsonDetailedSchema . toCardanoAPIData . PlutusTx.toBuiltinData $ datum
"{\"constructor\":0,\"fields\":[{\"bytes\":\"74657374\"}]}"
```

That's it... our datum format is `"{\"constructor\":0,\"fields\":[{\"bytes\":\"74657374\"}]}"`, where `74657374` is our actual data, hex-encoded.

We do the same thing for the redeemer type:

```
Prelude Cardano.Api Ledger.Scripts PlutusTx Aeson ProofOfBurn> let redeemer = MyRedeemer ()
Prelude Cardano.Api Ledger.Scripts PlutusTx Aeson ProofOfBurn> Data.Aeson.encode . scriptDataToJson ScriptDataJsonDetailedSchema . toCardanoAPIData . PlutusTx.toBuiltinData $ redeemer
"{\"constructor\":0,\"fields\":[{\"constructor\":0,\"fields\":[]}]}"
```

### Creating wallet keys

Since we can't use the wallet REST API for all tasks yet, we need a lot of key/address files to be passed to `cardano-cli`.

Firs we create the root private key, for which we need the recovery phrase:

```sh
echo "<recovery-phrase>" |
	cardano-address key from-recovery-phrase Shelley > root.prv
```

Then the account private/public keys:

```sh
cat root.prv |
	cardano-address key child 1852H/1815H/0H |
	tee acct.prv |
	cardano-address key public --with-chain-code > acct.pub
```

### Creating lock transaction

First we need to create the script address from the `result.plutus` script and save it to `burn.addr`:

```sh
cardano-cli address build \
	--payment-script-file "result-plutus" \
	--testnet-magic=1097911063 \
	--out-file "burn.addr"
```

Then we create files for the datum and its hash:

```sh
echo "{\"constructor\":0,\"fields\":[{\"bytes\":\"74657374\"}]}" > datum.json
cardano-cli transaction hash-script-data --script-data-file "datum.json" > datum.hash
```

Now we need to perform a coin selection, which figures out which UTxOs can be used as inputs
for the transaction. We also need the wallet-id, which you should have gotten when creating it.
Otherwise check your wallets via `curl -s -X GET "http://localhost:8090/v2/wallets`.

```sh
cat <<EOF >/tmp/payload
{
  "payments": [
    {
      "address": "$(cat burn.addr)",
      "amount": {
        "quantity": 9000000,
        "unit": "lovelace"
      }
    }
  ]
}
EOF

curl -s -H "Content-Type: application/json" --data @/tmp/payload "http://localhost:8090/v2/wallets/<wallet-id>/coin-selections/random" > coin_selection.json
```

This will return a json of the following type:

```json
{
  "inputs": [
    {
      "address": "addr1sjck9mdmfyhzvjhydcjllgj9vjvl522w0573ncustrrr2rg7h9azg4cyqd36yyd48t5ut72hgld0fg2xfvz82xgwh7wal6g2xt8n996s3xvu5g",
      "amount": {
        "quantity": 42000000,
        "unit": "lovelace"
      },
      "assets": [
        {
          "policy_id": "65ab82542b0ca20391caaf66a4d4d7897d281f9c136cd3513136945b",
          "asset_name": "",
          "quantity": 0
        }
      ],
      "id": "1423856bc91c49e928f6f30f4e8d665d53eb4ab6028bd0ac971809d514c92db1",
      "derivation_path": [
        "1852H",
        "1815H",
        "0H",
        "0",
        "0",
      ],
      "index": 0
    }
  ],
  "outputs": [
    {
      "address": "addr1sjck9mdmfyhzvjhydcjllgj9vjvl522w0573ncustrrr2rg7h9azg4cyqd36yyd48t5ut72hgld0fg2xfvz82xgwh7wal6g2xt8n996s3xvu5g",
      "amount": {
        "quantity": 42000000,
        "unit": "lovelace"
      },
      "assets": [
        {
          "policy_id": "65ab82542b0ca20391caaf66a4d4d7897d281f9c136cd3513136945b",
          "asset_name": "",
          "quantity": 0
        }
      ]
    }
  ],
  "change": [
    {
      "address": "addr1sjck9mdmfyhzvjhydcjllgj9vjvl522w0573ncustrrr2rg7h9azg4cyqd36yyd48t5ut72hgld0fg2xfvz82xgwh7wal6g2xt8n996s3xvu5g",
      "amount": {
        "quantity": 42000000,
        "unit": "lovelace"
      },
      "assets": [
        {
          "policy_id": "65ab82542b0ca20391caaf66a4d4d7897d281f9c136cd3513136945b",
          "asset_name": "",
          "quantity": 0
        }
      ],
      "derivation_path": [
        "1852H",
        "1815H",
        "0H",
        "0",
        "1",
      ],
    }
  ],
  "collateral": [
    {
      "address": "addr1sjck9mdmfyhzvjhydcjllgj9vjvl522w0573ncustrrr2rg7h9azg4cyqd36yyd48t5ut72hgld0fg2xfvz82xgwh7wal6g2xt8n996s3xvu5g",
      "amount": {
        "quantity": 42000000,
        "unit": "lovelace"
      },
      "id": "1423856bc91c49e928f6f30f4e8d665d53eb4ab6028bd0ac971809d514c92db1",
      "derivation_path": [
        "1852H",
        "1815H",
        "0H",
        "0",
        "0",
      ],
      "index": 0
    }
  ],
  "withdrawals": [],
  "certificates": [],
  "deposits": [],
  "metadata": "string"
}
```

For simplicity, we'll assume that there is only one input, one change and one collaterall address (`cardano-cli` of course
allows to specify multiple inputs).

We'll also assume there are two different derivation paths (e.g. for inputs and change) 
`["1852H","1815H","0H","0","0"]` and `["1852H","1815H","0H","0","2"]`.
For each of those, we need to create signing and verification keys:

```sh
# if derivation path is ["1852H","1815H","0H","0","0"], pass "1852H/1815H/0H/0/0"
cat root.prv |
	cardano-address key child "1852H/1815H/0H/0/0" > addr0.prv

cardano-cli key convert-cardano-address-key --shelley-payment-key --signing-key-file "addr0.prv" --out-file key0.skey
cardano-cli key verification-key --signing-key-file key0.skey --verification-key-file key0.vkey
```

Now we should have the files `key0.skey`, `key0.vkey`, `key1.skey` and `key1.vkey`.

And we can create the transaction finally:

```sh
# generate protocol parameters
cardano-cli query protocol-parameters \
	--testnet-magic=1097911063 \
	> pparams.json

cardano-cli transaction build \
	--alonzo-era \
	\ # format is "${tx_id}#${tx_index}"
	--tx-in "$(cat coin_selection.json | jq -e -r '.inputs | .[0].id')#$(cat coin_selection.json | jq -e -r '.inputs | .[0].index')" \
	\ # same format
	--tx-in-collateral "$(cat coin_selection.json | jq -e -r '.collateral | .[0].id')#$(cat coin_selection.json | jq -e -r '.collateral | .[0].index')" \
	\ # amount needs to match what we fed into coin selection
	--tx-out "$(cat burn.addr)+9000000" \
	--tx-out-datum-hash=$(cat datum.hash)" \
	--change-address "$(cat coin_selection.json | jq -e -r '.change | .[0].address')" \
	--testnet-magic=1097911063 \
	--protocol-params-file "pparams.json" \
	--witness-override 2 \
	--required-signer="key0.skey" \
	--required-signer="key1.skey" \
	--out-file "tx.raw"
```

Then we sign it:

```sh
cardano-cli transaction sign \
		--tx-body-file tx.raw \
		--signing-key-file="key0.skey" \
		--signing-key-file="key1.skey" \
		--out-file "tx.signed"
```

And can finally submit it:

```sh
cardano-cli transaction submit \
	--testnet-magic=1097911063 \
	--tx-file "tx.signed"
```

Now, using blockfrost, we can query the UTxO of the script address (you need so sign up for a free account to get a token or use some other indexer/service):

```sh
curl -s -X GET -H "project_id: ${BLOCKFROST_API_TOKEN}" \
	"https://cardano-testnet.blockfrost.io/api/v0/addresses/$(cat burn.addr)/utxos"
```

This might take a while, so retry at will.
