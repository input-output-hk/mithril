# Mithril Era Smart Contract

**This is a work in progress** :hammer_and_wrench:

This project is a proof of concept for a smart contract in charge of storing Mithril activation eras information on the Cardano chain and retrieving them with the `cardano-cli`.

## Pre-requisites

* You need to run a Haskell toolchain (ghc, cabal), which you can easily setup with [ghcup](https://www.haskell.org/ghcup/)
* You need to have installed the [secp256k1](https://github.com/bitcoin-core/secp256k1.git) library
* You need to have a recent version of [`jq`](https://stedolan.github.io/jq/download/) running (1.6+)
* A running Cardano node running locally on the network you are targeting

## Compile the Smart Contract

Build the project:
```bash
$ cabal update && cabal build
```

Export the smart contract to Plutus Core format:
```bash
$ cabal repl
```

Then type the command `MithrilEra.Utils.writePlutusFile "assets/mithril-era.plutus"` in the terminal:
```bash
Prelude MithrilEra.Contract> MithrilEra.Utils.writePlutusFile "assets/mithril-era.plutus"
wrote validator to file assets/mithril-era.plutus
```

This will write the cbor hex encoded plutus contract to the file `assets/mithril-era.plutus`, ready to be executed on the chain.

## Prepare script activation on chain

Export the path to the Cardano node socket and the testnet magic id:
```bash
$ CARDANO_NODE_SOCKET_PATH=**PATH_TO_YOUR_NODE_SOCKET**
$ CARDANO_TESTNET_MAGIC=**YOUR_TESTNET_MAGIC**
```

Retrieve the protocol parameters of the network:
```bash
$ cardano-cli query protocol-parameters --testnet-magic $CARDANO_TESTNET_MAGIC > assets/protocol-params.json
```

Create a script address for the smart contract:
```bash
$ cardano-cli address build --payment-script-file assets/mithril-era.plutus --out-file assets/mithril-era.addr --testnet-magic $CARDANO_TESTNET_MAGIC && cat assets/mithril-era.addr
addr_test1wps0lg7nulu7ts5ujt7sng544j2mjvqv4t9ltlgz9wzgy5ct8870k%
```

Set the transaction amount used when a script transaction is made:
```bash
$ SCRIPT_TX_VALUE=10000000
```

## Prepare wallet keypairs

Create the payment keys that will be used to interact with the smart contract:
```bash
$ cardano-cli address key-gen --verification-key-file assets/payment.vkey --signing-key-file assets/payment.skey
```
Create the payment address:
```bash
$ cardano-cli address build --payment-verification-key-file assets/payment.vkey --out-file assets/payment.addr --testnet-magic $CARDANO_TESTNET_MAGIC && cat assets/payment.addr
addr_test1vrnras3jr7npqhkfj0ynpjlmpt7w6qzhypmypd7m9qh8h9geu2jx0
```

Fund the payment address written in `assets/payment.addr` with the faucet available at: https://docs.cardano.org/cardano-testnet/tools/faucet

## Init Smart Contract Datum on chain

Verify that the payment address has funds:
```bash
$ cardano-cli query utxo --address $(cat assets/payment.addr) --testnet-magic $CARDANO_TESTNET_MAGIC
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
cef4b65d996c0f7ce017044977f4ad5f4a7f123bb3414b411a2b368d2ab5ceea     0        10000000000 lovelace + TxOutDatumNone
```

And create the variable `TX_IN={TxHash}#{TxIn}` by replacing with values from the previous command:
```bash
$ TX_IN=cef4b65d996c0f7ce017044977f4ad5f4a7f123bb3414b411a2b368d2ab5ceea#0
```

Create the initial datum file:
```bash
$ cat > assets/mithril-era-datum-1.json << EOF
{
    "constructor": 0,
    "fields": [
        {
            "int": 123456
        }
    ]
}
EOF
```

Now create the transaction to init the smart contract datum:
```bash
$ cardano-cli transaction build --babbage-era --testnet-magic $CARDANO_TESTNET_MAGIC \
    --tx-in $TX_IN \
    --tx-out $(cat assets/mithril-era.addr)+$SCRIPT_TX_VALUE \
    --tx-out-inline-datum-file assets/mithril-era-datum-1.json \
    --change-address $(cat assets/payment.addr) \
    --out-file tx.raw
Estimated transaction fee: Lovelace 166381
```

Then sign the transaction:
```bash
$ cardano-cli transaction sign \
    --tx-body-file tx.raw \
    --signing-key-file assets/payment.skey \
    --testnet-magic $CARDANO_TESTNET_MAGIC \
    --out-file tx.signed
```

And submit it:
```bash
$ cardano-cli transaction submit \
    --testnet-magic $CARDANO_TESTNET_MAGIC \
    --tx-file tx.signed
Transaction successfully submitted.
```

Also get the transaction id:
```bash
$ TX_ID=$(cardano-cli transaction txid --tx-file tx.signed) && echo $TX_ID
c9b1719d77bdc2a27cbc4277070dc9eebea370c413ba5d5561f41cfed2e3cbf6
```

We need to wait a few seconds before the transaction is available and we can see the initial datum for the script address:
```bash
cardano-cli query utxo --address $(cat assets/mithril-era.addr) --testnet-magic $CARDANO_TESTNET_MAGIC | grep -e $TX_ID -e TxHash
                           TxHash                                 TxIx        Amount
c9b1719d77bdc2a27cbc4277070dc9eebea370c413ba5d5561f41cfed2e3cbf6     0        10000000 lovelace + TxOutDatumInline ReferenceTxInsScriptsInlineDatumsInBabbageEra (ScriptDataConstructor 0 [ScriptDataNumber 123456])
```

We can retrieve the initial value stored in the datum with the cardano cli:

The full utxo json representation:
```bash
cardano-cli query utxo --address $(cat assets/mithril-era.addr) --testnet-magic $CARDANO_TESTNET_MAGIC --out-file temp.json && cat temp.json | jq .
{
  "c9b1719d77bdc2a27cbc4277070dc9eebea370c413ba5d5561f41cfed2e3cbf6#0": {
    "address": "addr_test1wps0lg7nulu7ts5ujt7sng544j2mjvqv4t9ltlgz9wzgy5ct8870k",
    "datum": null,
    "inlineDatum": {
      "constructor": 0,
      "fields": [
        {
          "int": 123456
        }
      ]
    },
    "inlineDatumhash": "37354f5c0267ced294c883f7a15f6847cf29bedfa42f888480edc38164d1ff8e",
    "referenceScript": null,
    "value": {
      "lovelace": 10000000
    }
  }
}
```

Or the extracted value stored:
```bash
$ cardano-cli query utxo --address $(cat assets/mithril-era.addr) --testnet-magic $CARDANO_TESTNET_MAGIC --out-file temp.json && cat temp.json | jq '. [] | select(.inlineDatum | . != null and . != "") | .inlineDatum.fields[0].int'
123456
```

## Update Smart Contract Datum on chain

Verify that the script address has an utxo:
```bash
$ cardano-cli query utxo --address $(cat assets/mithril-era.addr) --testnet-magic $CARDANO_TESTNET_MAGIC
```

And create the variable `TX_IN={TxHash}#{TxIn}` by replacing with values from the previous command:
```bash
$ TX_IN=c9b1719d77bdc2a27cbc4277070dc9eebea370c413ba5d5561f41cfed2e3cbf6#0
```

Verify that the payment address still has funds:
```bash
$ cardano-cli query utxo --address $(cat assets/payment.addr) --testnet-magic $CARDANO_TESTNET_MAGIC
```

And create the variable `COLLATERAL_TX_IN={TxHash}#{TxIn}` by replacing with values from the previous command:
```bash
$ COLLATERAL_TX_IN=c9b1719d77bdc2a27cbc4277070dc9eebea370c413ba5d5561f41cfed2e3cbf6#1
```

Create the updated datum file:
```bash
$ cat > assets/mithril-era-datum-2.json << EOF
{
    "constructor": 0,
    "fields": [
        {
            "int": 789000
        }
    ]
}
EOF
```

And create the redeemer file:
```bash
$ cat > assets/mithril-era-redeemer-2.json << EOF
{
    "constructor": 0,
    "fields": [
        {
            "int": 10
        }
    ]
}
EOF
```

Now create the transaction to update the smart contract datum:
```bash
$ cardano-cli transaction build --babbage-era --testnet-magic 2 \
    --tx-in $TX_IN \
    --tx-in-script-file assets/mithril-era.plutus \
    --tx-in-inline-datum-present \
    --tx-in-redeemer-file assets/mithril-era-redeemer-2.json \
    --change-address $(cat assets/mithril-era.addr) \
    --tx-out $(cat assets/mithril-era.addr)+$SCRIPT_TX_VALUE \
    --tx-out-inline-datum-file assets/mithril-era-datum-2.json \
    --tx-in-collateral $COLLATERAL_TX_IN \
    --protocol-params-file assets/protocol-params.json \
    --out-file tx.raw
Estimated transaction fee: Lovelace 301834
```

Then sign the transaction:
```bash
$ cardano-cli transaction sign \
    --tx-body-file tx.raw \
    --signing-key-file assets/payment.skey \
    --testnet-magic $CARDANO_TESTNET_MAGIC \
    --out-file tx.signed
```

And submit it:
```bash
$ cardano-cli transaction submit \
    --testnet-magic $CARDANO_TESTNET_MAGIC \
    --tx-file tx.signed
Transaction successfully submitted.
```

Also get the transaction id:
```bash
$ TX_ID=$(cardano-cli transaction txid --tx-file tx.signed) && echo $TX_ID
9f77de2a26a713508e7a7767c36e2c9e385dea3ed153c5e88e93463d73156e5c
```

We need to wait a few seconds before the transaction is available and we can see the updated datum for the script address:
```bash
cardano-cli query utxo --address $(cat assets/mithril-era.addr) --testnet-magic $CARDANO_TESTNET_MAGIC | grep -e $TX_ID -e TxHash
                           TxHash                                 TxIx        Amount
9f77de2a26a713508e7a7767c36e2c9e385dea3ed153c5e88e93463d73156e5c     0        10000000 lovelace + TxOutDatumInline ReferenceTxInsScriptsInlineDatumsInBabbageEra (ScriptDataConstructor 0 [ScriptDataNumber 789000])
```

We can retrieve the updated value stored in the datum with the cardano cli:

The full utxo json representation:
```bash
cardano-cli query utxo --address $(cat assets/mithril-era.addr) --testnet-magic $CARDANO_TESTNET_MAGIC --out-file temp.json && cat temp.json | jq . 
{
  "9f77de2a26a713508e7a7767c36e2c9e385dea3ed153c5e88e93463d73156e5c#0": {
    "address": "addr_test1wps0lg7nulu7ts5ujt7sng544j2mjvqv4t9ltlgz9wzgy5ct8870k",
    "datum": null,
    "inlineDatum": {
      "constructor": 0,
      "fields": [
        {
          "int": 789000
        }
      ]
    },
    "inlineDatumhash": "fced22aa63088d8c5089f5f32b1eac1f8c2d77f16e4f61503cf310848a414b5d",
    "referenceScript": null,
    "value": {
      "lovelace": 1000000
    }
  }
}
```
Or the extracted updated value stored:
```bash
$ cardano-cli query utxo --address $(cat assets/mithril-era.addr) --testnet-magic $CARDANO_TESTNET_MAGIC --out-file temp.json && cat temp.json | jq '. [] | select(.inlineDatum | . != null and . != "") | .inlineDatum.fields[0].int'
789000
```

