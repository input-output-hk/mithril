# Recompute the certificates hashes of Mithril aggregator

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

## Make a backup of the aggregator database

Connect to the aggregator VM:
```bash
ssh curry@$MITHRIL_VM
```

Once connected to the aggregator VM, export the environment variables:
```bash
export CARDANO_NETWORK=**CARDANO_NETWORK**
```

And copy the SQLite database file `aggregator.sqlite3`:
```bash
cp /home/curry/data/$CARDANO_NETWORK/mithril-aggregator/mithril/stores/aggregator.sqlite3 cp /home/curry/data/$CARDANO_NETWORK/mithril-aggregator/mithril/stores/aggregator.sqlite3.bak.$(date +%Y-%m-%d)
```

And connect to the aggregator container:
```bash
docker exec -it mithril-aggregator bash
```

Once connected to the aggregator container, recompute the certificates hashes:
```bash
/app/bin/mithril-aggregator -vvv tools recompute-certificates-hash
```

Then disconnect from the aggregator container:
```bash
exit
```

## Restart the aggregator

Restart the aggregator to make sure that the certificate chain is valid:
```bash
docker restart mithril-aggregator
```

Make sure that the certificate chain is valid (wait for the state machiene to go into the state `READY`):
```bash
docker logs -f --tail 1000 mithril-aggregator
```

Then disconnect from the aggregator VM:
```bash
exit
```

## Rollback procedure

If the recomputation fails, you can rollback the database.

First, stop the aggregator:
```bash
docker stop mithril-aggregator
```

Then, restore the backed up database:
```bash
cp /home/curry/data/$CARDANO_NETWORK/mithril-aggregator/mithril/stores/aggregator.sqlite3.sqlite3.bak.$(date +%Y-%m-%d) cp /home/curry/data/$CARDANO_NETWORK/mithril-aggregator/mithril/stores/aggregator
```

Then, start the aggregator:
```bash
docker start mithril-aggregator
```

Make sure that the certificate chain is valid (wait for the state machiene to go into the state `READY`):
```bash
docker logs -f --tail 1000 mithril-aggregator
```

Then disconnect from the aggregator VM:
```bash
exit
```