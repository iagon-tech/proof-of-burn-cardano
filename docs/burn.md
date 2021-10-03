---
title: Proof of Burn
subtitle: Exploring Cardano in Alonzo era
author:
  - Michał J. Gajda
  - Julian Ospald
bibliography: burn.bib
---

# Introduction

We take on the challenge by Charles Hoskinson[@hoskinson-youtube] and implement Proof-of-Burn protocol[@proof-of-burn] on Cardano. After explaining the idea, we take the design through three different stages:

1. Pure smart contract implementation as suggested.
2. The ways how smart contracts Alonzo differ from these of Ethereum-style ledgers,
   and how to work around these.
3. Explain the weakness of smart contract approach,
   and solving it by using direct crafting of transactions.

# Proof-of-burn and its applications

The burning of cryptocurrencies and crypto tokens is sending them to a black hole address.
The address with no access key to retrieve funds.
At the same time, the public can verify that a burn took place, but only by knowing a "secret": the commitment value.
Proof-of-burn[@proof-of-burn] is a protocol proposed for assuring the burning of funds in a way that is not censorable by the middlemen.
The burning of blockchain funds may serve to buyback the tokens and boost valuations of the remaining tokens.
Or it may be used as a proof of commitment in blockchain protocols[@blockchain-protocols].
Burning in large amounts may cause deflationary pressure since it decreases the total amount of the token in circulation.

The burning of funds is a usual blockchain transaction and needs to be accepted by the miner.
However, some people are opposed to burning some tokens. To prevent this issue,
we would like to have a protocol that allows people to burn the tokens but not to censor these burns.
CEO of IOHK was particularly interested in having it implemented for the Cardano network[@hoskinson-youtube].

# Smart contract for Proof-of-Burn

We describe the mechanics of a Proof-of-Burn on a Cardano smart contract.
Cardano smart contracts are programs that run on the Cardano network
and allow the contract developer to execute financial actions according to rules pre-set.

Smart contracts may establish transactions between multiple parties
that are transparent and verifiable.
Driving factors for the adoption of smart contracts are decentralized financial services [@blockchain-financial-services; @uniswap], and decentralized organizations [@dao].

The smart contract would have four possible actions:

* **burn** -- which sends the funds to a black hole address with a secret hashed commitment value,
* **burned** -- that validates that a burn with the given commitment value took place,
* **lock** -- that sends the funds to an address with a key,
* **redeem** -- that redeems the funds locked by the **lock**.

Please note that endpoints run within the wallet of the user.
After the endpoint script runs in the wallet, the resulting transactions go to the blockchain.
In our case, the transaction will move money to the `Redeemer` script. This script verifies
that money is only available to the target address whose hash it holds.

``` {.haskell}
type HashOfTargetAddr = BuiltinByteString
newtype MyDatum = MyDatum { fromMyDatum :: HashOfTargetAddr }

validateSpend :: ValidatorType Burner
validateSpend (MyDatum addrHash) _myRedeemerValue
              ScriptContext { scriptContextTxInfo = txinfo } =
   traceIfFalse "owner has not signed"
     (addrHash `elem` fmap (sha3_256 . getPubKeyHash) (txInfoSignatories txinfo))
```

In the case of **lock**, the hash may be of our own address.
In the case of **burn**, the hash points to a black hole. We achieve it by hashing a secret _commitment_ value
and then _flipping a bit_ in it. Since we use the cryptographic hash function[@proof-of-burn], it is almost impossible to find a value
that would match the resulting hash. 

``` {.haskell}
burn' :: AsContractError e => (BuiltinByteString, Value) -> Contract w Schema e ()
burn' (aCommitment, burnedFunds) = do
    let hash = flipCommitment $ sha3_256 aCommitment
    let tx = Constraints.mustPayToTheScript (MyDatum hash) burnedFunds
    void $ submitTxConstraints burnerTypedValidator tx
```

Note that `flipCommitment` just flips the least significant bit of a hash:

``` {.haskell}
flipCommitment :: BuiltinByteString -> BuiltinByteString
flipCommitment (BuiltinByteString bs) = BuiltinByteString $ do
  case unsnoc bs of
    Just (seq, lastElt) -> snoc seq
                         $ lastElt `xor` 1
```

The middlemen that accept our transactions will not know whether the transaction is a **burn** or a **lock**.
And thus cannot selectively censor all our **burn** transactions.

Both **burn** and **lock** use the same `Redeemer` format, and only the transaction submitter knows whether the action was a **burn** or a **lock**.

And later, we can check that the value was burned by calling the `burned` endpoint with the given commitment.
Until the _commitment_ value is revealed, nobody knows that the funds were burned.

If you are interested in the code, here is [the repository](https://github.com/iagon-tech/proof-of-burn-cardano/blob/main/src/ProofOfBurn.hs).

# Deploying the smart contract to testnet

We can deploy the smart contract to the Cardano Testnet by executing the following.

To start a testnet node with our own wallet (you need a mnemonic phrase and a random password), we need:

1. Install Haskell toolchain:
``` {.sh}
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
source ~/.ghcup/env
```

Build the Plutus script:
``` {.sh}
cabal run plutus-burner -- 42 out/result.plutus
```

Start the containers with cardano-node and cardano-wallet:
``` {.haskell}
mkdir out/
export NETWORK=testnet
export BLOCKFROST_API_TOKEN=<token>
docker-compose up --build -d
docker-compose run cardano-wallet \
	sh -c 'chmod 777 /ipc/node.socket'
```


Finally to restore wallet, this outputs the _wallet-id_ which we need for the following steps:
If you want a random mnemonic phrase for testing, run `scripts/cardano-cli.sh 'cardano-address recovery-phrase generate'`

``` {.sh}
scripts/cardano-cli.sh 'wallet.sh bootstrap_wallet <mnemonic sentence phrase> <password>'
```

Afterwards you may burn your own money with:
``` {.sh}
scripts/cardano-cli.sh 'wallet.sh burn_funds out/ <wallet-id> <commitment> <amount>'
```

To verify that it was burned:
``` {.sh}
scripts/cardano-cli.sh 'wallet.sh validate_burn <commitment> $(cat out/burn.addr)'
```

The outside world doesn't know whether this transaction is a burn or a lock. All they know
is that it's a smart contract.

However, after we publish this script, opponents of the burn may try to compile start
denying Redeemers that correspond to our Redeemer script hash. That would be some effort,
but could lead to censoring of our burns.

Can we make it even harder to censor the burn...?

# From a smart contract to wallet script

We already saw that most of the action in a smart contract
happens in the user's wallet.
So can we make an implementation that only uses the wallet,
and makes the smart contract unnecessary?

This would have a practical boon: selectively preventing smart contract transactions
that correspond to burns is impossible. But a censor wanting to prevent all burns
could just block all scripts that try to burn anything ever^[Since each new GHC version generates slightly different Plutus code, it would require some effort by the means of static analysis or CI/CD setup that checks script hashes. But it is theoretically possible.].
If we only ever use a wallet, then the only way to censor the burns is to censor all Cardano transactions.
That would be an ultimate resistance to censorship.

For this to work, we need to replace the public key hash with the hash of the commitment value and flip the lowest bit of this commitment.
But this is not all.
Cardano prevents typos in addresses by verifying its structure and CRC code.
The easiest way for us to generate address from the commitment is using the following script and Cardano API library:

``` {.haskell}
burnAddress = do
  commitment <- flipCommitment . sha3_256 $ arg
  return $ serialiseAddress
         $ makeShelleyAddress
             network
             (fromShelleyPaymentCredential (Shelley.KeyHashObj (mkKeyHash commitment)))
             NoStakeAddress
```

To see how it works, you may generate the burn address with:
``` {.haskell}
cabal run generate-burn-address -- "mySecretCommitment"
```

Then you may submit the transaction to the Cardano blockchain with:
``` {.sh}
scripts/cardano-cli.sh 'wallet.sh send_funds out/ <wallet-id> <burn-address> <amount>'
```

In order to verify the burn you need to check transactions sent to the burn address:

``` {.haskell}
scripts/cardano-cli.sh 'wallet.sh get_utxo <burn-address> | jq .'
```