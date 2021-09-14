# Proof of burn

## Running in playground

Build the playground:

1. clone https://github.com/input-output-hk/plutus
2. set up IOHK binary cache: https://github.com/input-output-hk/plutus#how-to-set-up-the-iohk-binary-caches
3. run `nix-shell` in two terminals
4. in one terminal run `cd plutus-playground-server && plutus-playground-server`
5. in the other run `cd plutus-playground-client && npm run start`

Then copy paste `ProofOfBurn.hs` and compile and evaluate.

## Trying on testnet

1. Download cardano-wallet [release](https://github.com/input-output-hk/cardano-wallet/releases) and copy all binaries to your PATH

2. Download testnet config from https://hydra.iohk.io/build/7366583/download/1/index.html

```sh
mkdir -p config/testnet/
cd config/testnet/
curl -O https://hydra.iohk.io/build/7366583/download/1/testnet-config.json
curl -O https://hydra.iohk.io/build/7366583/download/1/testnet-byron-genesis.json
curl -O https://hydra.iohk.io/build/7366583/download/1/testnet-shelley-genesis.json
curl -O https://hydra.iohk.io/build/7366583/download/1/testnet-alonzo-genesis.json
curl -O https://hydra.iohk.io/build/7366583/download/1/testnet-topology.json
curl -O https://hydra.iohk.io/build/7366583/download/1/testnet-db-sync-config.json
curl -O https://hydra.iohk.io/build/7366583/download/1/rest-config.json
cd ../..
```

3. Run the node

```sh
mkdir -p db/testnet
cardano-node run \
		--topology config/testnet/testnet-topology.json \
		--database-path db/testnet \
		--socket-path node.socket \
		--port 3001 \
		--config config/testnet/testnet-config.json
export CARDANO_NODE_SOCKET_PATH=$(pwd)/node.socket
```

4. Run the wallet

```sh
mkdir -p db/wallet
cardano-wallet serve \
  --testnet config/testnet/testnet-byron-genesis.json \
  --node-socket node.socket \
  --database db/wallet
```

5. Create a wallet

```sh
cardano-address recovery-phrase generate > passphrase
cat passphrase
cardano-wallet wallet create from-recovery-phrase "My Wallet" # enther the phrase from the previous step
```

6. Get payment address

```sh
cardano-wallet address list $(cardano-wallet wallet list | jq -r '.[].id') | jq -r '.[0].id'
```

7. Apply for faucet funds by entering the address from the previous step in: https://testnets.cardano.org/en/testnets/cardano/tools/faucet/

8. Check that you got the funds

```sh
cardano-wallet wallet utxo $(cardano-wallet wallet list | jq -r '.[].id')
```
