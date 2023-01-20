# Mithril Era Without Smart Contract

**This is a work in progress** :hammer_and_wrench:

This project is a proof of concept for a storing Mithril activation eras information on the Cardano chain and retrieving them with the `cardano-cli`.

## Pre-requisites

* You need to have a recent version of [`jq`](https://stedolan.github.io/jq/download/) running (1.6+)
* A running Cardano node running locally on the network you are targeting

## Setup

Export the path to the Cardano node socket and the testnet magic id:
```bash
$ export CARDANO_NODE_SOCKET_PATH=**PATH_TO_YOUR_NODE_SOCKET**
$ export CARDANO_TESTNET_MAGIC=**YOUR_TESTNET_MAGIC**
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
addr_test1vpcr3he05gemue6eyy0c9clajqnnww8aa2l3jszjdlszjhq093qrn%
```

Fund the payment address written in `assets/payment.addr` with the faucet available at: https://docs.cardano.org/cardano-testnet/tools/faucet

## Write a transaction with first version of datum on chain

Verify that the payment address has funds:
```bash
$ cardano-cli query utxo --address $(cat assets/payment.addr) --testnet-magic $CARDANO_TESTNET_MAGIC
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
09d2128edd7075fcee27b680c73e324340089273735c8518102975ff930cf150     0        10000000000 lovelace + TxOutDatumNone
```

And create the variable `TX_IN={TxHash}#{TxIn}` by replacing with values from the previous command:
```bash
$ TX_IN=09d2128edd7075fcee27b680c73e324340089273735c8518102975ff930cf150#0
```

Create the initial era definition value:
```bash
$ cat > assets/mithril-era-definition-1.json << EOF
[
  {
    "era1": 123
  },
  {
    "era2": null
  }
]
EOF
```

Create the initial datum file (by hex encoding the era definition json file):
```bash
$ DATUM_VALUE_HEX_1=$(cat assets/mithril-era-definition-1.json | xxd -p -c 16000)
$ cat > assets/mithril-era-datum-1.json << EOF
{
    "constructor": 0,
    "fields": [
        {
            "bytes": "$DATUM_VALUE_HEX_1"
        }
    ]
}
EOF
```

Now create the first transaction with datum:
```bash
$ cardano-cli transaction build --babbage-era --testnet-magic $CARDANO_TESTNET_MAGIC \
    --tx-in $TX_IN \
    --tx-out $(cat assets/payment.addr)+$SCRIPT_TX_VALUE \
    --tx-out-inline-datum-file assets/mithril-era-datum-1.json \
    --change-address $(cat assets/payment.addr) \
    --out-file tx.raw
Estimated transaction fee: Lovelace 168669
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
$ cardano-cli transaction txid --tx-file tx.signed
c5eb7ad4ae8ffdd677386013f92d196a023425b73f650058e6d5b2115048ae0c
```

We need to wait a few seconds before the transaction is available and we can see the initial datum for the script address:
```bash
$ cardano-cli query utxo --address $(cat assets/payment.addr) --testnet-magic $CARDANO_TESTNET_MAGIC
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
c5eb7ad4ae8ffdd677386013f92d196a023425b73f650058e6d5b2115048ae0c     0        10000000 lovelace + TxOutDatumInline ReferenceTxInsScriptsInlineDatumsInBabbageEra (ScriptDataConstructor 0 [ScriptDataBytes "[\n  {\n    \"era1\": 123\n  },\n  {\n    \"era2\": null\n  }\n]\n"])
c5eb7ad4ae8ffdd677386013f92d196a023425b73f650058e6d5b2115048ae0c     1        9989831331 lovelace + TxOutDatumNone
```

We can retrieve the initial value stored in the datum with the cardano cli:

The full utxo json representation:
```bash
$ cardano-cli query utxo --address $(cat assets/payment.addr) --testnet-magic $CARDANO_TESTNET_MAGIC --out-file temp.json && cat temp.json | jq '. [] | select(.inlineDatum | . != null and . != "")'
{
  "address": "addr_test1vpcr3he05gemue6eyy0c9clajqnnww8aa2l3jszjdlszjhq093qrn",
  "datum": null,
  "inlineDatum": {
    "constructor": 0,
    "fields": [
      {
        "bytes": "5b0a20207b0a202020202265726131223a203132330a20207d2c0a20207b0a202020202265726132223a206e756c6c0a20207d0a5d0a"
      }
    ]
  },
  "inlineDatumhash": "6e81b3eac829c0313bcc91621b6ebc2571b1419bbdc3cb715570ca1b0cd52256",
  "referenceScript": null,
  "value": {
    "lovelace": 10000000
  }
}
```

Or the extracted value stored:
```bash
$ cardano-cli query utxo --address $(cat assets/payment.addr) --testnet-magic $CARDANO_TESTNET_MAGIC --out-file temp.json && cat temp.json | jq -r '. [] | select(.inlineDatum | . != null and . != "") | .inlineDatum.fields[0].bytes' | xxd -ps -r | jq .
[
  {
    "era1": 123
  },
  {
    "era2": null
  }
]
```

## Write a second with new version of datum on chain

Retrieve the utxo of the payment address:
```bash
$ cardano-cli query utxo --address $(cat assets/payment.addr) --testnet-magic $CARDANO_TESTNET_MAGIC
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
c5eb7ad4ae8ffdd677386013f92d196a023425b73f650058e6d5b2115048ae0c     0        10000000 lovelace + TxOutDatumInline ReferenceTxInsScriptsInlineDatumsInBabbageEra (ScriptDataConstructor 0 [ScriptDataBytes "[\n  {\n    \"era1\": 123\n  },\n  {\n    \"era2\": null\n  }\n]\n"])
c5eb7ad4ae8ffdd677386013f92d196a023425b73f650058e6d5b2115048ae0c     1        9989831331 lovelace + TxOutDatumNone
```

And create the variable `TX_IN_DATUM={TxHash}#{TxIn}` by replacing with values from the previous command (where inline datumn are available):
```bash
$ TX_IN_DATUM=c5eb7ad4ae8ffdd677386013f92d196a023425b73f650058e6d5b2115048ae0c#0
```

And create the variable `TX_IN_NO_DATUM={TxHash}#{TxIn}` by replacing with values from the previous command (where inline datumn are not available):
```bash
$ TX_IN_NO_DATUM=c5eb7ad4ae8ffdd677386013f92d196a023425b73f650058e6d5b2115048ae0c#1
```

Create the updated era definition value:
```bash
$ cat > assets/mithril-era-definition-2.json << EOF
[
  {
    "era1": 123
  },
  {
    "era2": 125
  }
]
EOF
```

Create the updated datum file (by hex encoding the era definition json file):
```bash
$ DATUM_VALUE_HEX_2=$(cat assets/mithril-era-definition-2.json | xxd -p -c 16000)
$ cat > assets/mithril-era-datum-2.json << EOF
{
    "constructor": 0,
    "fields": [
        {
            "bytes": "$DATUM_VALUE_HEX_2"
        }
    ]
}
EOF
```

Now create the first transaction with datum:
```bash
$ cardano-cli transaction build --babbage-era --testnet-magic $CARDANO_TESTNET_MAGIC \
    --tx-in $TX_IN_DATUM \
    --tx-in $TX_IN_NO_DATUM \
    --tx-out $(cat assets/payment.addr)+$SCRIPT_TX_VALUE \
    --tx-out-inline-datum-file assets/mithril-era-datum-2.json \
    --change-address $(cat assets/payment.addr) \
    --out-file tx.raw
Estimated transaction fee: Lovelace 174653
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
$ cardano-cli transaction txid --tx-file tx.signed
1fd4d3e131afe3c8b212772a3f3083d2fbc6b2a7b20e54e4ff08e001598818d8
```

We need to wait a few seconds before the transaction is available and we can see the updated datum for the script address:
```bash
$ cardano-cli query utxo --address $(cat assets/payment.addr) --testnet-magic $CARDANO_TESTNET_MAGIC
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
1fd4d3e131afe3c8b212772a3f3083d2fbc6b2a7b20e54e4ff08e001598818d8     0        10000000 lovelace + TxOutDatumInline ReferenceTxInsScriptsInlineDatumsInBabbageEra (ScriptDataConstructor 0 [ScriptDataBytes "[\n  {\n    \"era1\": 123\n  },\n  {\n    \"era2\": 125\n  }\n]\n"])
1fd4d3e131afe3c8b212772a3f3083d2fbc6b2a7b20e54e4ff08e001598818d8     1        9989656678 lovelace + TxOutDatumNone
```

We can retrieve the updated value stored in the datum with the cardano cli:

The full utxo json representation:
```bash
$ cardano-cli query utxo --address $(cat assets/payment.addr) --testnet-magic $CARDANO_TESTNET_MAGIC --out-file temp.json && cat temp.json | jq '. [] | select(.inlineDatum | . != null and . != "")'
{
  "address": "addr_test1vpcr3he05gemue6eyy0c9clajqnnww8aa2l3jszjdlszjhq093qrn",
  "datum": null,
  "inlineDatum": {
    "constructor": 0,
    "fields": [
      {
        "bytes": "5b0a20207b0a202020202265726131223a203132330a20207d2c0a20207b0a202020202265726132223a203132350a20207d0a5d0a"
      }
    ]
  },
  "inlineDatumhash": "b97cbaa0dc5b41864c83c2f625d9bc2a5f3e6b5cd5071c14a2090e630e188c80",
  "referenceScript": null,
  "value": {
    "lovelace": 10000000
  }
}
```

Or the extracted updated value stored on chain:
```bash
$ cardano-cli query utxo --address $(cat assets/payment.addr) --testnet-magic $CARDANO_TESTNET_MAGIC --out-file temp.json && cat temp.json | jq -r '. [] | select(.inlineDatum | . != null and . != "") | .inlineDatum.fields[0].bytes' | xxd -ps -r | jq .
[
  {
    "era1": 123
  },
  {
    "era2": 125
  }
]
```
