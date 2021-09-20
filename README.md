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

1. Starting node and wallet:

```sh
export NETWORK=testnet
docker-compose up -d
```

2. Creating/restoring a wallet

```sh
. scripts/wallet.sh
create_wallet "Wallet name" "mnemonic sentence phrase" "password"
```

3. Get unused payment address

```sh
first_unused_payment_address "$(first_wallet_id)"
```

4. Apply for faucet funds by entering the address from the previous step in: https://testnets.cardano.org/en/testnets/cardano/tools/faucet/

5. Check that you got the funds

```sh
available_funds "$(first_wallet_id)"
```

6. Create keys

```sh
mkdir out/
docker-compose run -v $(pwd):/pwd -w /pwd -u $(id -u ${USER}):$(id -g ${USER}) cardano-wallet \
	sh -c 'source scripts/wallet.sh && create_keys out <mnemonic>'
```

7. Build plutus script

```sh
cabal run plutus-burner
```

8. Create addresses

```sh
# script address
docker-compose run -v $(pwd):/pwd -w /pwd -u $(id -u ${USER}):$(id -g ${USER}) cardano-wallet \
	sh -c 'source scripts/wallet.sh && script_address out result.plutus'

# payment address
docker-compose run -v $(pwd):/pwd -w /pwd -u $(id -u ${USER}):$(id -g ${USER}) cardano-wallet \
	sh -c 'source scripts/wallet.sh && payment_address out'
```

9. Burn some funds

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
* https://input-output-hk.github.io/cardano-wallet/api/edge
