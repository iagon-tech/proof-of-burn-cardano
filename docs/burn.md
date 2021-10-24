---
title: Proof of Burn
subtitle: Exploring Cardano in Alonzo era
author:
  - Michał J. Gajda
  - Julian Ospald
  - Dmitry Krylov
bibliography: burn.bib
---

# Introduction

We take on the challenge by Charles Hoskinson[@hoskinson-youtube] and implement Proof-of-Burn protocol[@proof-of-burn] on Cardano.

1. First we will explain the idea of proof-of-burn and applications of the burning.
2. We will show smart contract implementation of proof-of-burn on Cardano network and show the mechanism of action.
3. We show how to deploy and test smart contract on the test net.
4. We show how to implement proof-of-burn protocol without smart contract, by sending the money to black-hole address.

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

Security of proof-of-burn is based on the same mechanism as security of cryptocoin transfer transactions:
cryptographic hash function.

Cryptographic hash function is easy to compute, but very difficult to reverse. It is so difficult
to reverse, that it is usually considered that given a change in a _single bit of input_,
computed result will change each bit of the output randomly.
That means that if we change just a single bit of the output of a cryptographic hash function,
and try to find an input that generates the changed output, the computation would take
extremely long time. This means that flipping a lowest bit in a cryptographic hash function
make a _black hole address_. Whatever is sent to this address is very hard or impossible to recover.

This means that security of crypto transactions can be based solely on the public key cryptography,
and cryptographic hash functions:
Whenever money sent, a new _unspent transaction output_ is created. This UTxO records both the amount of money,
and a _cryptographic hash_ of the public key of the receiver. For receiver to use the money,
one has to sign a new transaction that spends it with the same public key.

But why flipping the lowest bit in an output of a hash function instead of just using a hash of 0x0?
Well, using a known value would make the burns immediately visible.
But the idea of the protocol is to first burn the money, and then prove it was burn afterwards
in a separate step.
For this to work, we first put a hash derived from a _commitment_ value. Later we can show
the commitment value to prove that we computed a _black hole address_.

# Smart contract for Proof-of-Burn on Cardano network

We describe the mechanics of a Proof-of-Burn on a Cardano smart contract.
Cardano smart contracts are programs that run on the Cardano network
and allow the contract developer to execute financial actions according to rules pre-set.

Smart contracts may establish transactions between multiple parties
that are transparent and verifiable.
Driving factors for the adoption of smart contracts are decentralized financial services [@blockchain-financial-services; @uniswap], and decentralized organizations [@dao].

Traditional Ethereum-style smart contracts consist of state recorded on the ledger, and program that is called asynchronously
by applications in order to change this state. Since the state of the ledger belonging to a smart contract can only
be changed through the program, all permissible state manipulations can be inferred from the program code
recorded on the blockchain. 

Cardano smart contracts have different architecture, aiming to make attacks harder by allowing user
to first simulate each transaction locally within the wallet of the user. The resulting
change is then validated by the blockchain node, and this very change is recorded.

For this purpose, Cardano smart contracts are described by three components:

* redeemer scripts, that permit or forbid spending of eUTxOs;
* wallet scripts, that are run on behalf of the user in order to redeem funds, and create new eUTxOs;
* eUTxOs, each holding funds and a datum that can be used for redeemers to confirm under which conditions these funds may be used again.

That means that Cardano smart contract has no central state on the ledger: each eUTxO has a separate state (datum)
that is unseparable from the funds in this eUTxOs.

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

The interface above uses command line scripts.
In future we will write about using Plutus PAB library that will allow to sign wallet transactions
from web applications. Currently this library is still incomplete.

But coming back to the point of censorship.
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

# Smart contract testing


The Plutus platform has good support for automatic testing.

At first, the Plutus platform supports smart contract unit testing using the `EmulatorTrace` monad.
This monad allows to call smart contract in the testing environment and supports trace of wallet balances changes.
So a developer can write testing scenarios like this:


``` {.haskell}
tests :: TestTree
tests = testGroup "unit tests" [ testLockAndRedeem ]

testLockAndRedeem :: TestTree
testLockAndRedeem = checkPredicate "lock and redeem"
  (     walletFundsChange w1 (Ada.adaValueOf (-50))
   .&&. walletFundsChange w2 (Ada.adaValueOf   50)
   .&&. walletFundsChange w3 (Ada.adaValueOf    0)
  )
  do
    h1 <- activateContractWallet w1 contract
    Emulator.waitNSlots 1
    let toAddr = pubKeyHash $ walletPubKey w2
    callEndpoint @"lock" h1 (toAddr, adaValueOf 50)
    Emulator.waitNSlots 1
    h2 <- activateContractWallet w2 contract
    Emulator.waitNSlots 1
    callEndpoint @"redeem" h2 ()
    Emulator.waitNSlots 1
```

Here one makes an instance `h1` of smart contract, then call `lock` endpoint with wallet `w2` and value (50 Ada),
then one calls `redeem` (with another instance `h2`).
After the end of the test scenario `checkPredicate` checks that provided the predicate
is true, i.e. the balance of wallet `w1` is decreased by 50 Ada,
`w2` is increased by 50 Ada, and the balance of untouched wallet `w3` is unchanged.

We provided unit tests for our proof-of-burn contract in [UnitTests.hs](../test/UnitTests.hs).

But main power testing feature of the Plutus platform is a *property-based testing*.

Plutus platform property-based testing allows to generate a bunch of random scenarios (with specified properties) for interacting with the smart contract under test.
And then run them to check that these scenarios work as expected.

So developers have to specify simple interact actions, for example:


``` {.haskell}
data Action POBModel
  = Lock         Wallet Wallet Ada
    -- ^ lock somve value from one wallet to another
  | Redeem              Wallet
    -- ^ redeem all locked values for specified wallet
  | Burn         Wallet Wallet Ada
    -- ^ burn some value
  | ValidateBurn Wallet Wallet
    -- ^ check that wallet make burn with commitement
```

Now Plutus platform can generate random sequence (with specified properties)
like `[Lock w1 w2 10, Lock w3 w1 20, Redeem w2, Burn w3 "h@ck_me" 50]`.

Each generated sequence run in two processes: first is *simulation*, second is running smart contract (under emulator in
`EmulatorTrace` monad). For example, simulated run of `Lock` action is:

``` {.haskell}
nextState (Lock wFrom wTo v) = do
  withdraw wFrom (Ada.lovelaceValueOf v)
  (pobLocks . at wTo) $~ (Just . maybe v (+v))
  wait 1
```

Here we just mark that value `v` withdrawn from simulated wallet `wFrom` and mark that it locked in favor of the wallet
`wTo`.

Simultaneously, the tester also runs the smart contract for this `Lock` action:

``` {.haskell}
perform h _modelState = \case
  (Lock wFrom wTo val) -> do
    let toAddr = pubKeyHash $ walletPubKey wTo
    callEndpoint @"lock" (h $ POBKey wFrom) (toAddr, toValue val)
    waitNSlots 1
```

After each run, the platform check that the simulation results match those on the actual run.
Particularly, it checks that simulated and real balances match. In our example, it checks that
simulated balance after `withdraw` matches with balance after `callEndpoint @"lock"` runned in `EmulatorTrace` monad.

All previously described Plutus platform testing abilities combined into one: *dynamic logic test scenarios* which lets
freely mix specific and random action sequences. For example:

``` {.haskell}
lockTest :: DL POBModel ()
lockTest = do
  action $ Lock w1 w2 20
  action $ Lock w3 w2 30
  action $ Redeem w2
  assertModel "at least 50 lovelaces must be redeemed"
              (redeemedFor w2 > 50)
```

Here we execute several random actions and then check that after some locks several Adas will be redeemed.

An important feature of DL test script running is the output of erroneous action sequence. For example, in early
stage of testing our proof-of-burn contract, we discovered a faulty sequence (which we then fixed, but left
on our test list):

``` {.haskell}
prop_GetObservableStateDon'tBreakEmulator :: Property
prop_GetObservableStateDon'tBreakEmulator =
  withMaxSuccess 1 $
  withDLTest anyActions_ mkPropForActions $
  DLScript
    [ Do $ ValidateBurn w1 w2
    , Do $ Burn         w1 w2 10000
    , Do $ Lock         w1 w2 20000
    ]
```

Here `DLScript [...]` is (slightly corrected) output of the Plutus platform runner.

Thus, using the platform's extensive testing capabilities, we checked
the performance of our application and identified and corrected significant bugs.

# Conclusion

We shown how to implement proof-of-burn as both smart contract, and a wallet transactions.
Since Alonzo smart contracts do not yet enjoy convenient infrastructure like the promised
PAB library for manipulating them from web applications, we recommend users
to use wallet scripts at the moment.

PAB is the planned library for submitting transactions on user wallet to the Cardano Node
without need to run wallet CLI scripts. It will facilitate making Cardano applications
in the near future.
