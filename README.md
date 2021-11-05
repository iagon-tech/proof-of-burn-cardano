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

### Trying on testnet

1. Build plutus script

```sh
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
source ~/.ghcup/env
cabal run plutus-burner
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
cabal run generate-burn-address -- "mysecret"
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
