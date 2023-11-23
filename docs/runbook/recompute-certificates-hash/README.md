# Recompute the certificates hashes of Mithril aggregator

## Configure environment variables
Export the environment variables: 
```bash
export MITHRIL_VM=**MITHRIL_VM**
export CARDANO_NETWORK=**CARDANO_NETWORK**
export MITHRIL_DISTRIBUTION_LINUX_PKG=**MITHRIL_DISTRIBUTION_LINUX_PKG**
```

Here is an exmaple for the `release-mainnet` network:
```bash
export MITHRIL_VM=aggregator.release-mainnet.api.mithril.network
export CARDANO_NETWORK=mainnet
export MITHRIL_DISTRIBUTION_LINUX_PKG=https://github.com/input-output-hk/mithril/releases/download/2342.0/mithril-2342.0-linux-x64.tar.gz
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
cp /home/curry/data/$CARDANO_NETWORK/mithril-aggregator/mithril/stores/aggregator.sqlite3 /home/curry/data/$CARDANO_NETWORK/mithril-aggregator/mithril/stores/aggregator.sqlite3.bak.$(date +%Y-%m-%d)
```

## Create a temp directory

Create a temp directory:
```bash
rm -rf /home/curry/temp
mkdir -p /home/curry/temp/config
cd /home/curry/temp
```

And download the configuration file for tools:
```bash
wget https://raw.githubusercontent.com/input-output-hk/mithril/main/docs/runbook/recompute-certificates-hash/config/tools.json -O /home/curry/temp/config/tools.json
```

## Download the pre-compiled aggregator binary

Download the mithril pre-compiled binaries package:
```bash
wget $MITHRIL_DISTRIBUTION_LINUX_PKG -O mithril-bin.tar.gz
```

Unpack the mithril aggregator binary:
```bash
tar xzf mithril-bin.tar.gz mithril-aggregator
```

Make mithril aggregator binary executable:
```bash
chmod u+x mithril-aggregator
```

Make sure you are running the expected version of the aggregator:
```bash
./mithril-aggregator --version
```

## Stop the aggregator

Stop the aggregator container:
```bash
docker stop mithril-aggregator
```

## Run the migration

Once connected to the aggregator container, recompute the certificates hashes:
```bash
DATA_STORES_DIRECTORY=/home/curry/data/$CARDANO_NETWORK/mithril-aggregator/mithril/stores/ ./mithril-aggregator --run-mode tools -vvv tools recompute-certificates-hash
```

## Restart the aggregator

Restart the aggregator to make sure that the certificate chain is valid:
```bash
docker start mithril-aggregator
```

Make sure that the certificate chain is valid (wait for the state machine to go into the `READY` state):
```bash
docker logs -f --tail 1000 mithril-aggregator
```

Then disconnect from the aggregator VM:
```bash
exit
```

## Cleanup the temp directory

Remove the temp directory:
```bash
rm -rf /home/curry/temp
```

## Rollback procedure

If the recomputation fails, you can rollback the database.

First, stop the aggregator:
```bash
docker stop mithril-aggregator
```

Then, restore the backed up database:
```bash
cp /home/curry/data/$CARDANO_NETWORK/mithril-aggregator/mithril/stores/aggregator.sqlite3.bak.$(date +%Y-%m-%d) /home/curry/data/$CARDANO_NETWORK/mithril-aggregator/mithril/stores/aggregator.sqlite3
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