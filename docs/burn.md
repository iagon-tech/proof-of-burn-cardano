---
title: Proof of Burn
subtitle: Exploring Cardano in Alonzo era
author: Michał J. Gajda, and Julian Ospald
bibliography: burn.bib
---

# Introduction

We take on challenge by Charles Hoskinson and implement Proof-of-Burn on Cardano.
After explaining the idea, we take the design through three different stages:

1. Pure smart contract implementation as suggested.
2. The ways how smart contracts Alonzo differ from these of Ethereum-style ledgers,
   and how to work around these.
3. Weakness of smart contract approach,
   and solving it by using direct money transfer transactions.

# Proof-of-burn and its applications

Burning of cryptocurrencies and crypto tokens is sending them to a wallet that has no access key.
That makes them irretrievable in a way that can be verified by the public.
Proof-of-burn[@proof-of-burn] is a protocol proposed for assuring burning of funds in a way that is not censorable by the middlemen.
Burning of blockchain funds may serve to buyback the tokens and boost valuations of remaining tokens.
Or it may be used as a proof of commitment in blockchain protocols[@blockchain-protocols].
Burning in large amounts may cause deflationary pressure, since it decreases total amount of the token in the circulation.

Burning of funds is a usual transaction of the blockchain, and needs to be accepted by the miner.
However, there are some people that are opposed to burning some tokens. To prevent this issue,
we would like to have a protocol that allows people to burn the tokens, but not to censor these burns.
CEO of IOHK was particularly interested in having it implemented for Cardano network[@hoskinson-youtube].

# Smart contract for Proof-of-Burn

We describe mechanics of a Proof-of-Burn on a Cardano smart contract.
Cardano smart contracts are basically programs that run on Cardano network,
and allow execution of financial actions according to rules pre-set by contract developer.

Smart contracts are used to establish transactions between multiple parties
that are transparent and verifiable.
Driving factors for their adoption are decentralized financial services[@blockchain-financial-services; @uniswap], and decentralized organizations [@dao].

The smart contract would have three possible actions:

* **burn** -- which sends the funds to an address without a key,
* **lock** -- that sends the funds to an address with a key,
* **redeem** -- that redeems the funds locked by the **lock**.

Please note that endpoints are executed within the wallet of the user.
After the endpoint script is executed in the wallet, the resulting transactions are submitted to the blockchain.
In our case the transaction will move money to the `Redeemer` script that verifies
that money is redeemed by the target address.

``` {.haskell}
type HashOfTargetAddr = BuiltinByteString
newtype MyDatum = MyDatum { fromMyDatum :: HashOfTargetAddr }

validateSpend :: ValidatorType Burner
validateSpend (MyDatum addrHash) _myRedeemerValue ScriptContext { scriptContextTxInfo = txinfo } =
   traceIfFalse "owner has not signed" (addrHash `elem` fmap (sha3_256 . getPubKeyHash) (txInfoSignatories txinfo))
```

In case of **lock** the target address can be our own.
In case of **burn** the target address is a black hole. We achieve it by hashing a secret _commitment_ value,
and then flipping a bit in it. Since we use cryptographic hash function[@proof-of-burn], it is very difficult to find a value
that would match the resulting hash.

``` {.haskell}
burn' :: AsContractError e => (BuiltinByteString, Value) -> Contract w Schema e ()
burn' (aCommitment, burnedFunds) = do
    let hash = flipCommitment $ sha3_256 aCommitment
    let tx = Constraints.mustPayToTheScript (MyDatum hash) burnedFunds
    void $ submitTxConstraints burnerTypedValidator tx
```

Note that `flipCommitment` just flips the least significant bit of a hash:
```
flipCommitment :: BuiltinByteString -> BuiltinByteString
flipCommitment (BuiltinByteString bs) = BuiltinByteString $ do
  case unsnoc bs of
    Just (seq, lastElt) -> snoc seq
                         $ lastElt `xor` 1
```

The middle men that accept our transactions will not know whether the transaction is a **burn** or a **lock**.
And thus cannot selectively censor all our **burn** transactions.

This is because both **burn** and **lock** use the same `Redeemer` format,
only the submitter of the transaction knows whether the action was a **burn** or a **lock**.

And later we can check that the value was burned by calling the `burned` endpoint with the given commitment.
Until the _commitment_ value is revealed, nobody knows that the funds were burned.

If you are interested in the code, here is [the repository](https://github.com/iagon-tech/proof-of-burn-cardano/blob/main/src/ProofOfBurn.hs).
# Deploying the smart contract to testnet

We can deploy the smart contract to the Cardano Testnet by executing the following.

Now we could make a transaction that is not known to be a `burn` (it may be `lock`),
and publish the proof that it was the burn afterwards.

After you [pull Docker images with the Cardano CLI](https://github.com/iagon-tech/proof-of-burn-cardano/blob/main/README.md#trying-on-testnet) or install it, you can run the following scripts,
to deploy the contract:
``` {.haskell}
-- Julian, please fill in...
```

Afterwards you may burn your own money with:
``` {.haskell}
scripts/cardano-cli.sh 'wallet.sh get_utxo $(cat out/burn.addr)' | jq -r .
```

The verify it was burned:
```
scripts/cardano-cli.sh 'wallet.sh validate_burn <commitment> $(cat out/burn.addr)'
```

Excellent demonstration... but determined opponent would just blacklist our contract hash,
and reject all transactions for this contract.
Since Cardano has a multitude of distributed miners, we could still submit the transaction to a different miner, but can we do even better...?

# From smart contract to wallet script

We already saw that most of the action in a smart contract
happens in the user's wallet.
So can we make an implementation that only uses the wallet,
and makes smart contract unnecessary?

This would have a practical boon: selectively preventing smart contract transactions
that correspond to burns is impossible. But censor wanting to prevent all burns
could just block all scripts that try to burn anything ever^[Since each new GHC version generates slightly different Plutus code, it would require some effort by the means of static analysis or CI/CD setup that checks script hashes. But it is theoretically possible.]
If we only ever use a wallet, then the only way to censor the burns is to censor all Cardano transactions.
That would be an ultimate resistance to censorship.

For this to work, we need to replace public key hash with the hash of the commitment value, and flip the lowest bit of this commitment.
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
cabal run plutus-fake-address -- "mySecretCommitment"
```

Then you may submit the transaction to the Cardano blockchain with:
``` {.sh}
scripts/cardano-cli.sh 'wallet.sh send_funds out/ <wallet-id> <burn-address> <amount>'
```

In order to verify the burn you need to check transactions sent to the burn address:

``` {.haskell}
scripts/cardano-cli.sh 'wallet.sh get_utxo <burn-address> | jq .'
```