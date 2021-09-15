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

9. Create keys

```sh
cat passphrase \
    | cardano-address key from-recovery-phrase Shelley > root.prv
cat root.prv \
    | cardano-address key child 1852H/1815H/0H \
    | tee acct.prv \
    | cardano-address key public --with-chain-code > acct.pub
cat root.prv \
	| cardano-address key child 1852H/1815H/0H/0/0 > addr.prv
cardano-cli key convert-cardano-address-key --shelley-payment-key --signing-key-file addr.prv --out-file key.skey
cardano-cli key verification-key --signing-key-file key.skey --verification-key-file key.vkey
```


10. Create addresses

```sh
# script address
cardano-cli address build --payment-script-file result.plutus --testnet-magic 1097911063 --out-file burn.addr

# payment address
cardano-cli address build \
	--payment-verification-key-file key.vkey \
	--out-file payment.addr \
	--testnet-magic 1097911063
```

10. Burn some funds

```sh
# generate protocol parameters
cardano-cli query protocol-parameters --testnet-magic 1097911063 > pparams.json

# hash datum value
echo "1234567" > datum.txt
cardano-cli transaction hash-script-data --script-data-value $(cat datum.txt) > burn_hash.txt

# find the TxHash and TxIx that contains your faucet funds
cardano-cli query utxo --testnet-magic 1097911063 --address $(cat payment.addr)

# build the transaction
cardano-cli transaction build \
	--alonzo-era \
	--tx-in <TxHash>#<TxIx> \
	--tx-out $(cat burn.addr)+98000000 \
	--tx-out-datum-hash $(cat burn_hash.txt) \
	--change-address $(cat payment.addr) \
	--testnet-magic 1097911063 \
	--protocol-params-file pparams.json \
	--out-file tx.raw

# sign the stransaction
cardano-cli transaction sign --tx-body-file tx.raw --signing-key-file payment.skey --out-file tx.sign

# submit the transaction
cardano-cli transaction submit --testnet-magic 1097911063 --tx-file tx.sign
```

11. Query script address balance

```sh
# recored the script TxHash and TxIx
cardano-cli query utxo --testnet-magic 1097911063 --address (cat burn.addr)
```

12. Check that we can't redeem

```sh
# tx-in is from script (previous step)
# collateral is remaining utxo from wallet
cardano-cli transaction build \
       --alonzo-era \
       --tx-in <TxHash>#<TxIx> \
       --tx-in-script-file result.plutus \
       --tx-in-datum-value $(cat datum.txt) \
       --tx-in-redeemer-value $(cat datum.txt ) \
       --tx-in-collateral <TxHash>#<TxIx> \
       --change-address $(cat payment.addr) \
       --protocol-params-file pparams.json \
       --testnet-magic 1097911063 \
       --out-file tx.raw
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
