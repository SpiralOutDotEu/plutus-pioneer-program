# week 02
# Gift

## shell snips

> On cabal repl
> ```bash
> cd code/week02
> import PlutusTx
> i: Data
> I 42
> :t I 42
> :set -XOverloadedStrings
> B "Haskell"
> :t B "Haskell"
>
>
> ```

> ```bash
> :l src/Week02/Gift.hs
> ()
> :t ()
> ```

## Validator 

`{-# INLINABLE mkValidator #-}` is used to make validator inlinable so that it fits in Plutus compiler

constructor parameters is Datum->Redeemer->Context and all are data here
this constructor ignores all input and returns unit

```haskell
mkValidator :: Data -> Data -> Data -> ()
mkValidator _ _ _ = ()
```

> Reload with `:r` if in cabal

## Compile validator

```haskell
validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| mkValidator ||])
```

## Create validators Hash

```haskell
valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash validator
```

## Get address in blockchain of validator

```haskell
scrAddress :: Ledger.Address
scrAddress = scriptAddress validator
```
> Running in shell:
> `Prelude Ledger.Scripts PlutusTx Week02.Gift> scrAddress` 
> returns:
> ```
>Address {addressCredential = ScriptCredential c3168d465a84b7f50c2eeb51ccacd53a305bd7883787adb54236d8d17535ca14, addressStakingCredential = Nothing}
> ```
## Brief of rest code

### Define entrypoints

```haskell
type GiftSchema =
            Endpoint "give" Integer
        .\/ Endpoint "grab" ()
```

### Define behavior of entrypoints (give(integer) & grab())

```haskell
give :: AsContractError e => Integer -> Contract w s e ()
give amount = do
    let tx = mustPayToOtherScript valHash (Datum $ Constr 0 []) $ Ada.lovelaceValueOf amount
    ledgerTx <- submitTx tx
    void $ awaitTxConfirmed $ txId ledgerTx
    logInfo @String $ printf "made a gift of %d lovelace" amount

grab :: forall w s e. AsContractError e => Contract w s e ()
grab = do
    utxos <- utxoAt scrAddress
    let orefs   = fst <$> Map.toList utxos
        lookups = Constraints.unspentOutputs utxos      <>
                  Constraints.otherScript validator
        tx :: TxConstraints Void Void
        tx      = mconcat [mustSpendScriptOutput oref $ Redeemer $ I 17 | oref <- orefs]
    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    void $ awaitTxConfirmed $ txId ledgerTx
    logInfo @String $ "collected gifts"
```

### Wrap them all together

Make `give` & `grab` available as an option and recurse back to the same (endpoint)
Assign the endpoints the previous created functions

```haskell
endpoints :: Contract () GiftSchema Text ()
endpoints = (give' `select` grab') >> endpoints
  where
    give' = endpoint @"give" >>= give
    grab' = endpoint @"grab" >>  grab
```

### this is only for the test

```haskell
mkSchemaDefinitions ''GiftSchema

mkKnownCurrencies []
```
# Burn

Import `{-# LANGUAGE OverloadedStrings   #-}` to use the byte Strings

## Validators primitives

### Ignore all Input, Accept Everything

```haskell
mkValidator _ _ _ = ()
```

### Ignore all Input, Reject Everything

```haskell
mkValidator _ _ _ = error ()

```

### Ignore all Input, Reject Everything with a message

```haskell
mkValidator _ _ _ = traceError "BURNT!"
```

# 42 

Validate only when redeemer's input is 42

## Define in validator

```haskell
mkValidator _ r _
    | r == I 42 = ()
    | otherwise = traceError "wrong redeemer
```

## Define in endpoint schema

```haskell
-- Change this
type GiftSchema =
            Endpoint "give" Integer
        .\/ Endpoint "grab" ()
-- to this
type GiftSchema =
            Endpoint "give" Integer
        .\/ Endpoint "grab" Integer

```
## Change grab function (the redeemer)

```haskell
-- Change this definition that has no parameters
grab :: forall w s e. AsContractError e => Contract w s e ()
-- to this that requires an integer
grab :: forall w s e. AsContractError e => Integer -> Contract w s e ()

-- Change
grab = do
-- to a named `n`
grab n = do
-- ...
-- and the hardcoded reference `17`
tx      = mconcat [mustSpendScriptOutput oref $ Redeemer $ I 17 | oref <- orefs
-- with the reference named `n`
tx      = mconcat [mustSpendScriptOutput oref $ Redeemer $ I n | oref <- orefs
```

The final grab function  is:

```haskell
grab :: forall w s e. AsContractError e => Integer -> Contract w s e ()
grab n = do
    utxos <- utxoAt scrAddress
    let orefs   = fst <$> Map.toList utxos
        lookups = Constraints.unspentOutputs utxos      <>
                  Constraints.otherScript validator
        tx :: TxConstraints Void Void
        tx      = mconcat [mustSpendScriptOutput oref $ Redeemer $ I n | oref <- orefs]
    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    void $ awaitTxConfirmed $ txId ledgerTx
    logInfo @String $ "collected gifts"
```

## Declare the change of the endpoint in final package

```haskell
-- change the input parameter of grab from none
grab' = endpoint @"grab" >>  grab
-- to a one that declares that expects a parameter
grab' = endpoint @"grab" >>=  grab
```

so that the final package wrap will be:

```haskell
endpoints :: Contract () GiftSchema Text ()
endpoints = (give' `select` grab') >> endpoints
  where
    give' = endpoint @"give" >>= give
    grab' = endpoint @"grab" >>= grab
```

# Typed

# Change import for typed scripts

```haskell
-- change this
import qualified Ledger.Scripts      as Scripts
-- to the typed version
import qualified Ledger.Typed.Scripts as Scripts
```
# Validator

## Change the header of validator to typed version
```haskell
-- Replace previous generic declaration
mkValidator :: Data -> Data -> Data -> () 
-- to a typed
mkValidator :: () -> Integer -> ScriptContext -> bool
```
## Change the validator function

```haskell
--the validator function:
mkValidator _ r _
    | r == I 42 = ()
    | otherwise = traceError "wrong redeemer"
-- can be replaced with:
mkValidator _ r _ = traceIfFalse "wrong redeemer" $ r == 42
```

## Define the dummy `data Typed` to wrap `Datum & Redeemer`

```haskell
data Typed
instance Scripts.ValidatorTypes Typed where
    type instance DatumType Typed = ()
    type instance RedeemerType Typed = Integer
```
## Compile as `Typed` Validator 

```haskell
typedValidator :: Scripts.TypedValidator Typed --it takes type parameter
typedValidator = Scripts.mkTypedValidator @Typed -- give Typed as argument
    $$(PlutusTx.compile [|| mkValidator ||]) -- as usual 
    $$(PlutusTx.compile [|| wrap ||]) -- wrap function
  where
    wrap = Scripts.wrapValidator @() @Integer -- wrap parameters are `()` as `the datum` & `@Integer` as `redeemer`
```
## Get the `validator` from `typedValidator`

```haskel
validator :: Validator
validator = Scripts.validatorScript typedValidator
```
## Get the `validatorHash` from `typedValidator`
```haskell
valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash typedValidator
```

## Get the `srcAddress` from `validator`
```haskell
scrAddress :: Ledger.Address
scrAddress = scriptAddress validator
```

## Complete validator will be:

```haskell
{-# INLINABLE mkValidator #-}
mkValidator :: () -> Integer -> ScriptContext -> Bool
mkValidator _ r _ = traceIfFalse "wrong redeemer" $ r == 42

data Typed
instance Scripts.ValidatorTypes Typed where
    type instance DatumType Typed = ()
    type instance RedeemerType Typed = Integer

typedValidator :: Scripts.TypedValidator Typed
typedValidator = Scripts.mkTypedValidator @Typed
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @() @Integer

validator :: Validator
validator = Scripts.validatorScript typedValidator

valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash typedValidator

scrAddress :: Ledger.Address
scrAddress = scriptAddress validator
```

## the rest remains the same


# Is Data

> in repl:
> ```nix
> :l src/Week02/Typed.hs
> import PlutusTx
> import PlutusTx.IsData.class
> :i IsData
> ....
> toData (42 :: Integer)
> -- returns: I 42
> fromData (I 42) :: Maybe Integer
> -- returns: Just 42
> fromData (List []) :: Maybe Integer
> -- returns: Nothing
> ```

## Replace the redeemer from integer to custom data type

## Create the new data type

```haskell
newtype MySillyRedeemer = MySillyRedeemer Integer
```

and convert to IsData with PlutusTx

```haskell
PlutusTx.unstableMakeIsData ''MySillyRedeemer
```
> Important: `PlutusTx.unstableMakeIsData` might break through Plutus version when more than parameters are used. For this example is no problem as it has only one parameter `Constr 0 [I 42`. On more parameter `stable` should be used instead,


## Define in validator

```haskell
-- Replace this:
mkValidator :: () -> Integer -> ScriptContext -> Bool
-- to this:
mkValidator :: () -> MySillyRedeemer -> ScriptContext -> Bool
```

```haskell
-- Replace in Typed definition
data Typed
instance Scripts.ValidatorTypes Typed where
    type instance DatumType Typed = ()
    type instance RedeemerType Typed = Integer
-- to the custom instance MySillyRedeemer
data Typed
instance Scripts.ValidatorTypes Typed where
    type instance DatumType Typed = ()
    type instance RedeemerType Typed = MySillyRedeemer
```


```haskell
-- replace in validator's compile wrap:
typedValidator :: Scripts.TypedValidator Typed
typedValidator = Scripts.mkTypedValidator @Typed
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @() @Integer
-- to a one that expects MySillyRedeemer
typedValidator :: Scripts.TypedValidator Typed
typedValidator = Scripts.mkTypedValidator @Typed
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @() @MySillyRedeemer
```

## Define in grab function

```haskell
grab n = do
    ...
    -- Replace the Integer definition:
    tx      = mconcat [mustSpendScriptOutput oref $ Redeemer $ I n | oref <- orefs]
    -- to a toData form of MySillyRedeemer
    tx      = mconcat [mustSpendScriptOutput oref $ Redeemer $ PlutusTx.toData (MySillyRedeemer n) | oref <- orefs]
    ...
```

## shell test

> ```nix
> :l src/Week02/IsData.hs
> import PlutusTx
> import PlutusTx.IsData.Class
> toData (MySillyRedeemer 42)
> --Returns: Constr 0 [I 42]
> toData (MySillyRedeemer 2)
> --Returns: Constr 0 [I 2]
```