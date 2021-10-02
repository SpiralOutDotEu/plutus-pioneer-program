# Week 03

> If plutus playground throws `timeout error` then increase the time out with:  
> ```nix
> plutus-playground-server -i 120s
> ```

## Intro

[plutus-ledger-api](https://playground.plutus.iohkdev.io/tutorial/haddock/plutus-ledger-api/html/Plutus-V1-Ledger-Api.html) -> [Plutus-V1-Ledger-Contexts](https://alpha.marlowe.iohkdev.io/doc/haddock/plutus-ledger-api/html/Plutus-V1-Ledger-Contexts.html)

`ScriptContext` ( TxInfo, ScriptPurpose ) 

`ScriptPurpose` - Constructors available [Minting, Spending, Rewarding, Certifying]

`TxInfo` is the view seen by validator scripts.
Contains: txInfoInputs, txInfoOutputs, txInfoFee, ...

> `1 Slot` = `1 second` now. In far future it might change.

[Plutus.V1.Ledger.Time](https://staging.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/Plutus-V1-Ledger-Time.html)


#

> ```nix
> [nix-shell:~/plutus-pioneer-program/code/week03]$ cabal repl
> import Plutus.V1.Ledger.Interval
> interval (10 :: Integer) 20
> # Returns: Interval {ivFrom = LowerBound (Finite 10) True, ivTo = UpperBound (Finite 20) True}
> member 9 $ interval (10 :: Integer) 20
> # Returns: False
> member 10 $ interval (10 :: Integer) 20
> # Returns: True
> member 21 $ from (30 :: Integer)
> # Returns: False
> member 30 $ from (30 :: Integer)
> # Returns: True
> member 999 $ from (30 :: Integer)
> # Returns: True
> intersection (interval (10 :: Integer) 20) $ interval 18 30
> # Returns: Interval {ivFrom = LowerBound (Finite 18) True, ivTo = UpperBound (Finite 20) True}
> contains (to (100 :: Integer)) $ interval 30 80
> # Returns: True
> contains (to (100 :: Integer)) $ interval 30 100
> # Returns: True
> contains (to (100 :: Integer)) $ interval 30 101
> # Returns: False
> overlaps (to (100 :: Integer)) $ interval 30 101
> # Returns: True
> ```

get pubKeyHash of the wallets

> ```nix
> :l src/Week03/Vesting.hs
> import Ledger
> import Wallet.Emulator
> pubKeyHash $ walletPubKey $ Wallet 2
> # Returns: 39f713d0a644253f04529421b9f51b9b08979d08295959c4f3990ee617f5139f
> pubKeyHash $ walletPubKey $ Wallet 3
> # Returns: dac073e0123bdea59dd9b3bda9cf6037f63aca82627d7abcd5c4ac29dd74003e
> ```

get deadline posix time

> ```nix
> import Ledger.TimeSlot
> import Data.Default
> def :: SlotConfig
> # Returns: SlotConfig {scSlotLength = 1000, scZeroSlotTime = POSIXTime {getPOSIXTime = 1596059091000}}
> slotToBeginPOSIXTime def 10
> # Returns: POSIXTime {getPOSIXTime = 1596059101000}
> slotToBeginPOSIXTime def 20
> # Returns: POSIXTime {getPOSIXTime = 1596059111000}
> ```

