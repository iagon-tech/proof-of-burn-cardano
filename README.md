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

1. Build plutus script

```sh
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
source ~/.ghcup/env
cabal run plutus-burner -- 42 out/result.plutus
```

2. Starting node and wallet:

```sh
mkdir out/
export NETWORK=testnet
docker-compose up -d
docker-compose run cardano-wallet \
	sh -c 'chmod 777 /ipc/node.socket'
```

3. Creating/restoring a wallet

```sh
. wallet.sh
create_wallet "Wallet name" "mnemonic sentence phrase" "password"
```

4. Get unused payment address

```sh
first_unused_payment_address "$(first_wallet_id)" > out/faucet.addr
cat out/faucet.addr
```

5. Apply for faucet funds by entering the address from the previous step in: https://testnets.cardano.org/en/testnets/cardano/tools/faucet/

6. Check that you got the funds

```sh
available_funds "$(first_wallet_id)"
```

7. Create keys

```sh
scripts/cardano-cli.sh \
	'wallet.sh create_keys out <mnemonic>'
```

8. Create addresses

```sh
# script address
scripts/cardano-cli.sh \
	'wallet.sh script_address out out/result.plutus'

# payment address
scripts/cardano-cli.sh \
	'wallet.sh payment_address out'
```

9. Burn some funds

```sh
# hash datum value
scripts/cardano-cli.sh \
	'wallet.sh hash_datum_value "<commitment>" > out/burn_hash.txt'

# find the TxHash and TxIx that contains your faucet funds
scripts/cardano-cli.sh \
	'wallet.sh get_utxo $(cat out/faucet.addr)'

# build the transaction
scripts/cardano-cli.sh \
	'wallet.sh create_script_transaction out "<TxHash>#<TxIx>" "$(cat out/burn.addr)" "98000000" "$(cat out/burn_hash.txt)" "$(cat out/payment.addr)"'

# sign the stransaction
scripts/cardano-cli.sh \
	'wallet.sh sign_transaction "out/tx.raw" "out/key.skey" "out/tx.sign"'

# submit the transaction
scripts/cardano-cli.sh \
	'wallet.sh submit_transaction "out/tx.sign"'
```

10. Query script address balance

```sh
# recored the script TxHash and TxIx
scripts/cardano-cli.sh \
	'wallet.sh get_utxo $(cat out/burn.addr)'
```

11. Check that we can't redeem

```sh
# First "<TxHash>#<TxIx>" is from script (previous step)
# Second "<TxHash>#<TxIx>" is collateral (remaining utxo from wallet)
scripts/cardano-cli.sh \
	'wallet.sh create_script_transaction out "<TxHash>#<TxIx>" "out/result.plutus" <commitment> <commitment> "<TxHash>#<TxIx>" "$(cat out/payment.addr)"'
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
