---
title: Testing smart contracts on the Cardano platform
author:
  - Dmitry A. Krylov
  - Michał J. Gajda
bibliography: cardano-testing.bib
---

# Introduction

Writing a valid smart contract requires both ability and diligence.
Developers have to take into account all possible uses and scenarios.
Verifying that the behavior of the smart contract (i.e. "formal verification of smart contract") conforms to all the ways of working specified in the specification requires a great deal of effort.
In addition, if requirements change, the assumptions and behavior will need to be verified again.
That is why automated random testing on real-world systems is the best way to ensure the rightness of smart contracts.
Developers can implement tests faster than complete verification, and random testing makes fewer assumptions than such a verification.
Such tests can quickly uncover both problems in the smart contracts themselves and gaps in the original specification.
This is especially important in the light of smart contracts that have been proven correct according to specification, only to for the specification to reveal its imperfections afterward.
Moreover, well-written random tests are a reasonable basis for the verification of correctness.

The Plutus platform has [built-in support for automatic
testing](https://playground.plutus.iohkdev.io/doc/haddock/plutus-pab/html/Plutus-PAB-Arbitrary.html),
based on a popular property testing framework
[QuickCheck](https://hackage.haskell.org/package/QuickCheck).

This article shows how to make an automatic test for the Plutus smart contract
using our [smart contract for “proof of
burn”](https://blog.iagon.com/iagons-solution-to-the-cardano-proof-of-burn-challenge/)
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
arbitrary, QuickCheck-based [@quickcheck], tests. The latter allows to implement
*semi-random* test scenarios, which combine user-specified and randomly
generated scenarios. This allows covering many more scenarios as compared with
fixed tests and detecting bugs earlier.

Writing unit tests is good covered in many tutorials, such us "Plutus pioneer program" [@pppweek08].
In this article we show how to write dynamic logic tests.

# Dynamic logic testing

Unit-test scenarios are hardwired (never change their behavior). However, it is
sometimes required to check smart contract properties on a wide range of
behaviors. The Plutus platform has *dynamic logic testing*. This feature allows
testing multiple complex tests scenarios by randomly generating them.

For this purpose, QuickCheck [@quickcheck] supports random generation of
arbitrary input to a function. The developer can use the random generator to
create an arbitrary test scenario and then check that this scenario runs as
expected.

The testing is model based, so developer have to define instance of
`ContractModel`. Example implementation of such modal considered well
in "Plutus pioneer program" [@pppweek08].

-- TODO --

*Dynamic logic test scenarios* which freely mix specific and random action
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

* `prop_LockAndRedeem` -- here, we check that `redeem` gets all the means locked
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
  works in the testing model.
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

