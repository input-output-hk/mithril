# Manual genesis of production Mithril network

## Configure environment variables
Export the environment variables: 
```bash
export MITHRIL_VM=**MITHRIL_VM**
export CARDANO_NETWORK=**CARDANO_NETWORK**
```

Here is an exmaple for the `release-mainnet` network:
```bash
export MITHRIL_VM=aggregator.release-mainnet.api.mithril.network
export CARDANO_NETWORK=mainnet
```

## Export the genesis payload to sign

Connect to the aggregator VM:
```bash
ssh curry@$MITHRIL_VM
```

Once connected to the aggregator VM, export the environment variables:
```bash
export CARDANO_NETWORK=**CARDANO_NETWORK**
```

And create genesis dir:
```bash
mkdir -p /home/curry/data/$CARDANO_NETWORK/mithril-aggregator/mithril/genesis
```
And connect to the aggregator container:
```bash
docker exec -it mithril-aggregator bash
```

Once connected to the aggregator container, export the genesis payload to sign:
```bash
/app/bin/mithril-aggregator -vvv genesis export --target-path /mithril-aggregator/mithril/genesis/genesis-payload-to-sign.txt
```

Then disconnect from the aggregator container:
```bash
exit
```

Then disconnect from the aggregator VM:
```bash
exit
```

## Sign the genesis payload

Once on your local machine, copy the genesis payload to sign from the aggregator VM:
```bash
scp curry@$MITHRIL_VM:/home/curry/data/$CARDANO_NETWORK/mithril-aggregator/mithril/genesis/genesis-payload-to-sign.txt .
```

Download or build the aggregator on your local machine as explained in this [documentation](https://mithril.network/doc/manual/developer-docs/nodes/mithril-aggregator#download-source)

Then, sign the payload with the genesis secret key:
```bash
./mithril-aggregator -vvv genesis sign --to-sign-payload-path genesis-payload-to-sign.txt --target-signed-payload-path genesis-payload-signed.txt --genesis-secret-key-path genesis.sk
```

## Import the signed genesis payload

Then, copy the signed genesis payload back to the aggregator VM:
```bash
scp  ./genesis-payload-signed.txt curry@$MITHRIL_VM:/home/curry/data/$CARDANO_NETWORK/mithril-aggregator/mithril/genesis/genesis-payload-signed.txt
```

Then, connect back to the aggregator VM:
```bash
ssh curry@$MITHRIL_VM
```

Export the environment variable:
```bash
export CARDANO_NETWORK=**CARDANO_NETWORK**
```

And connect back to the aggregator container:
```bash
docker exec -it mithril-aggregator bash
```

Once connected to the aggregator container, import the signed genesis payload:
```bash
/app/bin/mithril-aggregator -vvv genesis import --signed-payload-path /mithril-aggregator/mithril/genesis/genesis-payload-signed.txt
```
