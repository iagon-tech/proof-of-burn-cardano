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

<!-- TODO

6. After describing how the hard-wired test scenario works,
   please describe why actions are needed:
     a) why we use action data structure (to generate it automatically)
     b) how to generate arbitrary scenarios as action lists
     c) how do you assure that a series of actions is valid?
8. At the end you should summarize what validation criteria we have for the smart contract of PoB.

-->

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


# Comparison/Conclusion

<!-- TODO D. Finalize with advantages of random testing. -->

Thus, using the platform's extensive testing capabilities, we checked the performance of
our application and identified and corrected significant bugs.

# Bibliography

<!-- vim: set textwidth=80 : -->

