---
title: Testing smart contracts on Cardano platform
author:
  - Dmitry Krylov
  - Michał J. Gajda
bibliography: cardano-testing.bib
---

# Introduction

Writing good smart contract is not trivial thing. Developers have to take into
account all possible ways of using smart contract. Developing of smart contract
also requires rechecking behaviour of smart contract because of requirements
changing. That is why automated testing of smart contracts so useful.

The Plutus platform has good support for automatic testing.

In this article we show how to make automatic test for smart contracts on example of our
smart contract "proof of burn".  <!-- TODO link to PoB here -->

The *burning* of cryptocurrencies and crypto tokens is sending them to a black hole address.
The address with no access key to retrieve funds.
At the same time, the public can verify that a burn took place, but only by knowing a "secret": the commitment value.
Proof-of-burn[@proof-of-burn] is a protocol proposed for assuring the burning of funds in a way that is not censorable by the middlemen.
The burning of blockchain funds may serve to buyback the tokens and boost valuations of the remaining tokens.
Or it may be used as a proof of commitment in blockchain protocols[@blockchain-protocols].
Burning in large amounts may cause deflationary pressure since it decreases the total amount of the token in circulation.

<!-- TODO: more detail with less words -->

Plutus platform supports common unit tests. Althoug unit test are suitable for
basic testing, Plutus platform allow developer to write tests with semi-random
behaviour (this called "dynamic logic tests"). Such tests combine user-specified
and random generated scenarious.  This allows to find more bugs in smart
contracts.

# Unit testing

At first, the Plutus platform supports smart contract unit testing using the
`EmulatorTrace` monad. This monad allows to call smart contract in the testing
environment. This monad also simulate wallets and trace of wallet balance
changes. For example, if called smart contract sends money from one wallet to
other, `EmulatorTrace` simulate both wallets, catch money movement and allow
developer to check final balances on these wallets.

Let's look at a code example:

``` {.haskell .numberLines}
testLockAndRedeem :: TestTree
testLockAndRedeem = checkPredicate "lock and redeem"
  (     walletFundsChange w1 (Ada.adaValueOf (-50))
   .&&. walletFundsChange w2 (Ada.adaValueOf   50)
   .&&. walletFundsChange w3 (Ada.adaValueOf    0)
  )
  do
    hndl1 <- activateContractWallet w1 contract
    let toAddr = pubKeyHash $ walletPubKey w2
    callEndpoint @"lock" hnld1 (toAddr, adaValueOf 50)
    Emulator.waitNSlots 1
    hndl2 <- activateContractWallet w2 contract
    callEndpoint @"redeem" hndl2 ()
```

In this test scenario we lock 50 ADA from simulated wallet `w1` in favour of
other wallet `w2` and check that other (for example, `w3`) wallets are
unchanged.

Here we have post-check state in lines 3-5 and we have testing scenario itself
in lines 8-13. All that executed by function `checkPredicate` (and we pass it
name of test, predicate to post-checking and test scenario itself).

In test scenario we first make an instance `hndl1` of our proof-of-burn smart
contrace bounded to simulated wallet `w1` (line 8), then call `lock` endpoint in
line 10. We call this endpoint with two arguments: address where to send
`toAddr` value `adaValueOf 50`. Then it is need to actualize changes in tested
blockchain so we wait one tick in line 11. Then we make other smart contract
instance bounded to simulated wallet `w2` (line 12) and call endpoint `redeem`.
Test scenario is completed.

Then the predicate in line 3-5 is checking. Here we make sure that balance of
simulated wallet `w1` decreased by 50 ADA, balance of `w2` increased by 50 ADA
and balance of `w3` remains the same.

We provided unit tests for our proof-of-burn smart contract in
[UnitTests.hs](../test/UnitTests.hs).

# Dynamic-logic testing

Unit-test scenarious are hardwired (not changed its behaviour). But sometimes
it is required to check smart contract properties on wide range of behaviours.
For make it possible, Plutus platform have *dynamic logic testing* (or
*property-based testing*). This feature allows to implement more complex test
scenarious, first of all, tests with random-generated sequence of actions.

<!-- TODO: express in a more simple way; -->

Plutus platform dynamic logic testing allows to generate a bunch of random
scenarios (with specified properties) for interacting with the smart contract
under test. And then run them to check that these scenarios work as expected.

So developers have to specify basic actions for interact with script, for example:


``` {.haskell}
data Action POBModel
  = Lock         Wallet Wallet Ada
    -- ^ lock some value from one wallet to another
  | Redeem       Wallet
    -- ^ redeem all locked values for specified wallet
  | Burn         Wallet ByteString Ada
    -- ^ burn some value from wallet using commitement
  | ValidateBurn Wallet ByteString
    -- ^ check that wallet make burn with commitement
```

Proof-of-burn smart contract have four endpoints, so we introduce for actions,
one action for each endpoint. We specify arguments for each endpoint in
corresponded action, for example, for `lock` endpoint we specify the wallet to
withdraw, the wallet to which funds will be sent after redeem and value.

Now Plutus platform can generate random sequence (with specified properties)
like `[Lock w1 w2 10, Lock w3 w1 20, Redeem w2, Burn w3 "h@ck_me" 50]`.
It is specified in `arbitraryAction` function in `ContractModel` instance using
common `QuickTest` functions:

``` {.haskell}
arbitraryAction _modelState = oneof $
  [ Lock         <$> genWallet <*> genWallet <*> genAda
  , Redeem       <$> genWallet
  , Burn         <$> genWallet <*> genByteString <*> genAda
  , ValidateBurn <$> genWallet <*> genByteString
  ]
```

Note that current model state is passed to this function, so we can generate
actions in depends of this current state. This can be used in more complex test
scenarios. For example, if in some smart contract it is necessary to do
initialization first, we can check passed `modelState` and generate
initialisation action; and then, when initialization occurs, we can generate all
other actions.

Each generated sequence run in two processes: first is *simulation*, second is
running smart contract (under emulator in `EmulatorTrace` monad). For example,
simulated run of `Lock` action is:

``` {.haskell}
nextState (Lock wFrom wTo v) = do
  withdraw wFrom (Ada.lovelaceValueOf v)
  (pobLocks . at wTo) $~ (Just . maybe v (+v))
  wait 1
```

Here we just mark that value `v` withdrawn from simulated wallet `wFrom` and
mark that it locked in favor of the wallet `wTo`.

Simultaneously, the tester also runs the smart contract for this `Lock` action:

``` {.haskell}
perform h _modelState = \case
  (Lock wFrom wTo val) -> do
    let toAddr = pubKeyHash $ walletPubKey wTo
    callEndpoint @"lock" (h $ POBKey wFrom) (toAddr, toValue val)
    waitNSlots 1
```

After each run, the platform check that the simulation results match those on
the actual run.  Particularly, it checks that simulated and real balances match.
In our example, it checks that simulated balance after `withdraw` matches with
balance after `callEndpoint @"lock"` runned in `EmulatorTrace` monad.

All previously described Plutus platform testing abilities combined into one:
*dynamic logic test scenarios* which lets freely mix specific and random action
sequences. For example:

``` {.haskell}
lockTest :: DL POBModel ()
lockTest = do
  action $ Lock w1 w2 20
  action $ Lock w3 w2 30
  action $ Redeem w2
  assertModel "at least 50 lovelaces must be redeemed"
              (\modelState -> redeemedFor modelState w2 > 50)
```

Here we execute several random actions and then check that after some locks
several Adas will be redeemed.

An important feature of DL test script running is the output of erroneous action
sequence. For example, in early stage of testing our proof-of-burn contract, we
discovered a faulty sequence (which we then fixed, but left on our test list):

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

# Validation criteria for the Proof-of-burn smart contract

We develop some criteria presented by respective property test listed below:

``` {.haskell}
tests :: TestTree
tests = testProperties "prop tests"
    [ ("lock and then redeem",                  prop_LockAndRedeem)
    , ("burn and then validate",                prop_BurnAndValidate)
    , ("random actions is consistent",          prop_RandomActionsIsConsistent)
    , ("burn validating in emulator is works",  prop_BurnValidatingInEmulatorIsWorks)
    , ("observable state don't break emulator", prop_GetObservableStateDon'tBreakEmulator)
    ]
```

* `prop_LockAndRedeem` -- here we check that `redeem` gets all the means locked
  with `lock` and that `burn` not interfere with this. To do this we first call
  `lock`, then some arbitrary actions, then `redeem`, and then check that
  redeemed expected value.
* `prop_BurnAndVerify` -- similar to the previous one, but we check that
  `lock`/`redeem` not interfere with `burn`/`validateBurn`.
* `prop_RandomActionsIsConsistent` -- pure random actions. Our testing model
  have internal checks and for any sequence of actions all this checks must be
  passed.

As stated earlier, dynamic logic test runner can output `DLTest` list of
actions, where some failures occur. When we develop our proof-of-burn, we have
faced with such issues and save this output to tests. So we have two of them:

* `prop_BurnValidatingInEmulatorIsWorks` -- checks that `ValidateBurn` correctly
  work in testing model.
* `prop_GetObservableStateDon'tBreakEmulator` -- we seem to have encountered a
  bug in the Plutus emulator, and here we check that our test model byepasses
  it.

# Conclusion

As a quick design, developer can use common unit tests.

But the greatest opportunities for testing is dynamic logic testing. It is
requires to write instance of `ControlMonad` (and some auxilliary code), but
then developers can write plentiful test scenarious, which allow developers to
find more errors in smart contracts.

Thus, using the Plutus platform's extensive testing capabilities, we checked the
performance of our application and identified and corrected significant bugs.

# Bibliography

<!-- vim: set textwidth=80 : -->

