# Nix and Plutus getting started

# add nix to bash
. /home/v-dev/.nix-profile/etc/profile.d/nix.sh

# run plutus-playground.

`In terminal window 1`

```bash
cd plutus
nix-shell
cd plutus-playground-server
plutus-playground-server
```

If it's successful, you should see `Interpreter ready`

`In terminal window 2`
```bash
cd plutus
nix-shell
cd plutus-playground-client
npm run start
```

If it's successful, you should see [wdm]: `Compiled successfully`.

You should now be able to navigate to https://localhost:8009. The browser will complain about it being a risky website, allow it.


# nix things

* collect garbage

`nix-collect-garbage -d`

or

* clean store
`nix-store --gc`

# week 01

```bash
code/week01
[nix-shell]  cabal repl
[nix-shell] import Ledger.TimeSlot
[nix-shell] slotToPOSIXTime 10 
```
returns `POSIXTime {getPOSIXTime = 1596059101}`

