# Proof of burn

## Smart contract version

This is an implementation of proof of burn utilizing smart contracts.

### Running in playground

Build the playground:

1. clone https://github.com/input-output-hk/plutus
2. set up IOHK binary cache: https://github.com/input-output-hk/plutus#how-to-set-up-the-iohk-binary-caches
3. run `nix-shell` in two terminals
4. in one terminal run `cd plutus-playground-server && plutus-playground-server`
5. in the other run `cd plutus-playground-client && npm run start`

Then copy paste `ProofOfBurn.hs` and compile and evaluate.

### Trying on testnet via cli

This only uses the plutus script, but not any of the endpoints. Submission of contracts,
checking their values etc. is used via command line tools and utilizing BLOCKFROST blockchain
indexer.

1. Build plutus script

```sh
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
source ~/.ghcup/env
cabal run plutus-burner -- generate-plutus -d result.plutus
```

2. Starting node and wallet:

```sh
mkdir out/
export NETWORK=testnet
export BLOCKFROST_API_TOKEN=<token>
docker-compose build --build-arg CARDANO_NODE_GIT_REF=9dd31b3f8f17fba30882e98bb02810a7a504ba38
docker-compose up -d
docker-compose run cardano-wallet \
	sh -c 'chmod 777 /ipc/node.socket'
```

3. Creating/restoring a wallet

```sh
scripts/cardano-cli.sh 'wallet.sh bootstrap_wallet out/ <mnemonic sentence phrase> <password>'
```

4. Check that your wallet is done syncing

```sh
scripts/cardano-cli.sh 'wallet.sh wallet <wallet-id> | jq .state'
```

5. Burn funds

```sh
scripts/cardano-cli.sh 'wallet.sh burn_funds out/ <wallet-id> <commitment> <amount>'
```

6. Validate burn

```sh
# might need to re-run a few times, until the burn transaction reached the blockchain
scripts/cardano-cli.sh 'wallet.sh validate_burn <commitment> $(cat out/burn.addr)'
```

7. Check that we can't redeem

```sh
# find txhash/txix to redeem
scripts/cardano-cli.sh 'wallet.sh get_utxo $(cat out/burn.addr)' | jq -r .

# use txhash/txix from previous step
scripts/cardano-cli.sh \
	'wallet.sh redeem_funds out/ <wallet-id> <txhash> <txix> <datum>'
```

### Lock-Redeem

The steps above show a proof-of-burn workflow. This script can do a simple lock-redeem as well, which
also serves as a practical proof that a burned value cannot be redeemed, despite this functionality
existent in the script.

Here's a brief step-by-step guide to lock and redeem:

1. Follow the first 4 steps from the burn-workflow above

2. Generate datum, so that we can redeem ourselves

```sh
# we need this value for the next step
scripts/cardano-cli.sh 'wallet.sh get_pubkey_hash out/root.prv "1852H/1815H/0H/0/0"'
# we need this value for the next step
scripts/cardano-cli.sh 'wallet.sh sha3_256 <output-from-prev-step>'
```

3. Lock

```sh
# need output from step 2, which is our datum
scripts/cardano-cli.sh 'wallet.sh lock_funds out/ <wallet-id> <datum> <amount-to-lock>'
```

4. Check that the script transaction made it to the chain

```sh
# look for your locked value in the output (might need to re-run a few times)
# and find txhash/txix to redeem
scripts/cardano-cli.sh 'wallet.sh get_utxo $(cat out/burn.addr)' | jq -r .
```

5. Check wallet balance before redeeming

```sh
scripts/cardano-cli.sh 'wallet.sh available_funds <wallet-id>'
```

6. Redeem

```sh
scripts/cardano-cli.sh 'wallet.sh redeem_funds out/ <wallet-id> <txhash> <txix> <datum>'
```

5. Check wallet balance after redeeming

```sh
# might need to re-run a few times until transaction made it to the chain
scripts/cardano-cli.sh 'wallet.sh available_funds <wallet-id>'
```

### Trying on testnet via plutus PAB

Unlike the cli version, this doesn't just run the plutus script, but the real contract endpoints.
For that we need an entire stack of applications:

1. **node (unix socket)**: communication entrypoint to the blockchain
2. **wallet (HTTP REST API)**: manages wallet, keys, transaction creation etc.
3. **chain indexer (HTTP REST API)**: imports relevant blockchain data into an sqlite database for more efficient queries by the PAB backend
4. **plutus PAB (HTTP REST API)**: contract backend

Via e.g. curl, we can then use the [PAB HTTP API](https://github.com/input-output-hk/plutus-apps/blob/main/plutus-pab/ARCHITECTURE.adoc#the-pab-http-api)
to start and manage contract instances.

The following steps outline a contract submission:

1. Due to some [issues](https://github.com/input-output-hk/plutus-apps/issues/86), we build the node separately:

```sh
git clone https://github.com/input-output-hk/cardano-node.git
cd cardano-node
git checkout 37d86a65b4ffd7a500bd6fc7246793b1363ff60a
cabal build cardano-node:exe:cardano-node
cd ..
export CARDANO_NODE_DIR="$(pwd)/cardano-node"
```

2. Build the rest of the stack

```sh
cabal build proof-of-burn:exe:plutus-burner-pab cardano-wallet:exe:cardano-wallet plutus-chain-index:exe:plutus-chain-index
```

3. Start the node

```sh
./start-testnet-node.sh
```

4. Start the wallet backend


5. Create/restore a wallet

```sh
cat <<'EOF' > testnet/restore-wallet.json
{ "name": "PAB testing wallet"
, "mnemonic_sentence": ["word1", "word2", ...]
, "passphrase": "pab123456789"
}
EOF

curl -H "content-type: application/json" -XPOST \
  -d @testnet/restore-wallet.json \
  localhost:8090/v2/wallets

# use the returned wallet id
export WALLET_ID=...
```

6. Apply for [testnet faucet](https://testnets.cardano.org/en/testnets/cardano/tools/faucet/) to add some funds to your wallet

7. Start chain indexer

```sh
./start-testnet-chainindex.sh
```

8. Start PAB backend

```sh
./start-testnet-pab.sh
```

9. Wait for all clients to sync, this can take 3 hours up to 1.5 days. To check the progress of the slowest component (chain indexer), run `curl -s localhost:9083/tip | jq '.tipSlot.getSlot'` and compare it with [testnet explorer](https://explorer.cardano-testnet.iohkdev.io/en)

10. Start a contract instance

```sh
export INSTANCE_ID=$( curl -H "Content-Type: application/json" -v -X POST -d "{\"caID\":[],\"caWallet\":{\"getWalletId\":\"$WALLET_ID\"}}" localhost:9080/api/contract/activate | jq -r '.unContractInstanceId')
```

11. Lock-redeem a value for yourself

```sh
# check balance
curl -H "content-type: application/json" -XGET \
        localhost:8090/v2/wallets/${WALLET_ID} | jq -r '.balance.available.quantity'

# check contract instance status (hooks should be non-empty)
curl -H "Content-Type: application/json" -v -X GET \
        localhost:9080/api/contract/instance/$INSTANCE_ID/status | jq .

# lock value
LOCK_VALUE=7000067 curl -H "Content-Type: application/json" -v -X POST -d \
        "[{\"getPubKeyHash\":\"$(scripts/wallet.sh get_pubkey_hash ${WALLET_ID})\"}, {\"getValue\":[[{\"unCurrencySymbol\":\"\"},[[{\"unTokenName\":\"\"},${LOCK_VALUE}]]]]}]" \
        localhost:9080/api/contract/instance/$INSTANCE_ID/endpoint/lock

# check contract instance status (hooks should be non-empty), there should be a `LockedValue` in `observableState`
# otherwise try again after a minute, until the transaction is verified on the chain
curl -H "Content-Type: application/json" -v -X GET \
        localhost:9080/api/contract/instance/$INSTANCE_ID/status | jq .

# check balance again
curl -H "content-type: application/json" -XGET \
        localhost:8090/v2/wallets/${WALLET_ID} | jq -r '.balance.available.quantity'

# redeem
curl -H "Content-Type: application/json" -v -X POST -d \
        "[]" \
        localhost:9080/api/contract/instance/$INSTANCE_ID/endpoint/redeem

# check contract instance status (hooks should be non-empty), there should be a `Redeemed` in `observableState`
# otherwise try again after a minute, until the transaction is verified on the chain
curl -H "Content-Type: application/json" -v -X GET \
        localhost:9080/api/contract/instance/$INSTANCE_ID/status | jq .

# check final balance
curl -H "content-type: application/json" -XGET \
        localhost:8090/v2/wallets/${WALLET_ID} | jq -r '.balance.available.quantity'
```

## UTxO version with fake address

This is an implementation of proof of burn utilizing fake addresses.

### Trying on testnet

1. Build fake address app

```sh
export NETWORK=testnet
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
source ~/.ghcup/env
cabal build generate-burn-address
```

2. Starting node and wallet:

```sh
mkdir out/
export BLOCKFROST_API_TOKEN=<token>
docker-compose up -d
docker-compose run cardano-wallet \
	sh -c 'chmod 777 /ipc/node.socket'
```

3. Creating/restoring a wallet

```sh
scripts/cardano-cli.sh 'wallet.sh bootstrap_wallet <mnemonic sentence phrase> <password>'
```

4. Burn funds

```sh
# keep the output address
cabal run plutus-burner -- generate-addr "mysecret"
# insert address
scripts/cardano-cli.sh 'wallet.sh send_funds out/ <wallet-id> <fake-address> <amount>'
```

5. Verify burn

```sh
# insert address
scripts/cardano-cli.sh 'wallet.sh get_utxo <fake-address> | jq .'
```

## Resources

* https://plutus.readthedocs.io
* https://plutus-pioneer-program.readthedocs.io
* https://github.com/chris-moreton/plutus-scripts
* https://github.com/input-output-hk/Alonzo-testnet
* https://github.com/input-output-hk/lobster-challenge
* https://playground.plutus.iohkdev.io/
* https://github.com/input-output-hk/cardano-wallet/wiki/Wallet-command-line-interface
* https://developers.cardano.org/docs/smart-contracts/plutus
* https://docs.cardano.org/plutus/learn-about-plutus
* https://input-output-hk.github.io/cardano-wallet/api/edge
* https://developers.cardano.org/docs/transaction-metadata/retrieving-metadata
