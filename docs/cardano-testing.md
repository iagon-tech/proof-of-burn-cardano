---
title: Testing smart contracts on the Cardano platform
author:
  - Dmitry A. Krylov
  - Michał J. Gajda
bibliography: cardano-testing.bib
---

# Introduction

Writing a valid smart contract requires both ability and diligence. Developers
have to take into account all possible uses and scenarios. If requirements
change, the assumptions and behavior may need revalidation. That is why
automated random testing on real-world systems is the best way to ensure the
correctness of smart contracts. The developer may implement tests sooner than a
complete proof of correctness, and random testing makes fewer assumptions than
such a proof.  This is especially important in the light of smart contracts that
have been proven correct according to specification, only to for the
specification to reveal its imperfections afterward.  Finally, well-written
random tests are a reasonable basis for proof of correctness.

The Plutus platform has a [built-in support for automatic
testing](https://playground.plutus.iohkdev.io/doc/haddock/plutus-pab/html/Plutus-PAB-Arbitrary.html),
based on a popular property testing framework
[QuickCheck](https://hackage.haskell.org/package/QuickCheck).

In this article, we show how to make an automatic test for the Plutus smart
contract using our [smart contract for "proof of
burn"](https://blog.iagon.com/iagons-solution-to-the-cardano-proof-of-burn-challenge/)
as a reference example.

The *burning* of cryptocurrencies and crypto tokens is sending them to a black
hole address: the address with no access key to retrieve funds.  After the
transaction happens, the public can verify that a burn took place. To do so, the
sender needs to share a "secret": the commitment value.
Proof-of-burn[@proof-of-burn] is a protocol proposed for assuring the burning of
funds in a way that is not censorable by the middlemen.  The burning of
blockchain funds may serve to buy back the tokens and boost valuations of the
remaining tickets.  Alternatively, it may be used as a proof of commitment in
blockchain protocols[@blockchain-protocols].  Burning in large amounts may cause
deflationary pressure since it decreases the total amount of the token in
circulation.

Plutus platform supports both standard unit tests with a fixed scenario and
random tests, which are called "property tests" because they aim at testing
properties (also called "dynamic logic tests") or laws applying to the program,
not just exact outputs.  Such tests combine user-specified and randomly
generated scenarios.  This allows covering many more scenarios as compared with
fixed tests and detecting bugs earlier.

# Unit testing

At first, the Plutus platform supports smart contract unit testing using the
`EmulatorTrace` monad. This monad allows calling smart contracts in the test
environment. This monad also simulates wallets and traces balance changes. For
example, if a smart contract sends money from one wallet to another,
`EmulatorTrace` simulates both wallets, catches money movement, and allows
developers to check final balances on these wallets.

Let us look at a code example:

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

In this test scenario, we lock 50 ADA from simulated wallet `w1` in favor of
another wallet `w2` and check that other (for example, `w3`) wallets are
unchanged.

Here we have the state validation after the test in lines 3-5, and we have the
test scenario itself in lines 8-13. All that is executed by function
`checkPredicate`. It takes the name of the test that is displayed to the user,
predicate to post-checking, and test scenario itself).

In the test scenario, we first make an instance `hndl1` of our proof-of-burn
smart contract bounded to simulated wallet `w1` (line 8), then call the `lock`
endpoint in line 10. We call this endpoint with two arguments: the address where
to send `toAddr` value `adaValueOf 50`. Then it is needed to execute changes in
the test blockchain environment, so we wait for one tick in line 11. Then we
make another smart contract instance bound to simulated wallet `w2` (line 12)
and call endpoint `redeem`.  The test scenario is complete.

Then the predicate in lines 3-5 is checked. Here we make sure that the balance
of simulated wallet `w1` decreased by 50 ADA, the balance of `w2` increased by
50 ADA and the balance of `w3` remains the same.

We provided unit tests for our proof-of-burn smart contract in
[UnitTests.hs](../test/UnitTests.hs).

# Property testing

Unit-test scenarios are hardwired (never change their behavior). However,  it is
sometimes required to check smart contract properties on a wide range of
behaviors. The Plutus platform has *property-based testing* (or *dynamic logic
testing*). This feature allows testing multiple complex tests scenarios by
randomly generating them.

For this purpose, QuickCheck[@quickcheck] supports random generation of
arbitrary input to a function. The developer can use the random generator to
create an arbitrary test scenario and then check that this scenario runs as
expected.

First, developers have to specify basic actions for interaction with the smart
contract:

``` {.haskell}
data Action POBModel
  = Lock         Wallet Wallet Ada
    -- ^ lock some value from one wallet to another
  | Redeem       Wallet
    -- ^ redeem all locked values for the specified wallet
  | Burn         Wallet ByteString Ada
    -- ^ burn some value from the wallet using commitment
  | ValidateBurn Wallet ByteString
    -- ^ check that wallet make burn with commitment
```

Proof-of-burn smart contract have four endpoints, so we introduce four actions,
one action for each endpoint. We specify arguments for each endpoint in the
corresponding action. For example, for the `lock` endpoint, we specify the
wallet to withdraw, the wallet to which funds will be sent after redeem and
value.

Now Plutus platform can generate random sequence (with specified properties)
like `[Lock w1 w2 10, Lock w3 w1 20, Redeem w2, Burn w3 "h@ck_me" 50]`.  It is
specified in `arbitraryAction` function in `ContractModel` instance using common
`QuickTest` functions:

``` {.haskell}
arbitraryAction _modelState = oneof $
  [ Lock         <$> genWallet <*> genWallet <*> genAda
  , Redeem       <$> genWallet
  , Burn         <$> genWallet <*> genByteString <*> genAda
  , ValidateBurn <$> genWallet <*> genByteString
  ]
```

Note that the current model state is passed to this function to generate actions
depending on this current state. This can be used in more complex test
scenarios. For example, if it is necessary for some smart contract to do
initialization first, we can check passed `modelState` and generate
initialization action. Then, when initialization occurs, we can generate all
other actions.

Each generated sequence runs in two processes: first is *simulation*, second is
running smart contract (under emulator in `EmulatorTrace` monad). For example,
simulated run of `Lock` action is:

``` {.haskell}
nextState (Lock wFrom wTo v) = do
  withdraw wFrom (Ada.lovelaceValueOf v)
  (pobLocks . at wTo) $~ (Just . maybe v (+v))
  wait 1
```

Here we mark that value `v` withdrawn from simulated wallet `wFrom` and mark
that is locked in favor of the wallet `wTo`.

Simultaneously, the tester also runs the smart contract for this `Lock` action:

``` {.haskell}
perform h _modelState = \case
  (Lock wFrom wTo val) -> do
    let toAddr = pubKeyHash $ walletPubKey wTo
    callEndpoint @"lock" (h $ POBKey wFrom) (toAddr, toValue val)
    waitNSlots 1
```

After each run, the platform check that the simulation results match those on
the actual run. Notably, it checks that simulated and real balances match. Our
example checks that simulated balance after `withdraw` matches with balance
after `callEndpoint @"lock"` run in `EmulatorTrace` monad.

All previously described Plutus platform testing abilities combined into one:
*dynamic logic test scenarios* which freely mix specific and random action
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

Here we execute several random actions and then check that several Adas will be
redeemed after some locks.

An essential feature of DL test script running is the output of an erroneous
action sequence. For example, in an early stage of testing our proof-of-burn
contract, we discovered a faulty sequence (which we then fixed but left on our
test list):

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

We develop some criteria presented by the respective property test listed below:

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
  with `lock` and that `burn` does not interfere with this. To do this, we first
  call `lock`, then some arbitrary actions, then `redeem`, and then check that
  redeemed expected value.
* `prop_BurnAndVerify` -- similar to the previous one, but we check that
  `lock`/`redeem` not interfere with `burn`/`validateBurn`.
* `prop_RandomActionsIsConsistent` -- pure random actions. Our testing model has
  internal assertions and must confirm all these assertions for any sequence of
  actions.

As stated earlier, dynamic logic test runner can output `DLTest` list of
actions, where some failures occur. When we develop our proof-of-burn, we have
faced such issues and save this output to tests. So we have two of them:

* `prop_BurnValidatingInEmulatorIsWorks` -- checks that `ValidateBurn` correctly
  work in the testing model.
* `prop_GetObservableStateDon'tBreakEmulator` -- we seem to have encountered a
  bug in the Plutus emulator, and here we check that our test model bypasses it.

# Conclusion

As a quick design check, a developer can use standard unit tests.

Nevertheless, property-based tests allow for much more extensive testing by
randomly generating test scenarios. It is required to write an instance of
`ControlMonad` (and some auxiliary code), but then developers can validate smart
contracts against many more test scenarios and find more errors in the smart
contract.

Using the Plutus platform's extensive testing capabilities, we checked the
performance of our application and corrected many bugs.

# Bibliography

<!-- vim: set textwidth=80 : -->

