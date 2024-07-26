# Mithril infrastructure

**This is a work in progress** :hammer_and_wrench:

This infrastructure creates an ad hoc Mithril network on Google Cloud platform

---

## Pre-requisites

**Install Terraform**

- Install [terraform](https://www.terraform.io/downloads) tools (latest stable version).

## Create a new environment

If you want to run this terraform automation for multiple environments from the same source, you need to create a new workspace:

- Create en env variable:

```bash
DEPLOY_ENVIRONMENT=**new-environment**
```

- Create the terraform worspace if needed:

```bash
terraform workspace new $DEPLOY_ENVIRONMENT
```

- Switch to the terraform worspace if needed:

```bash
terraform workspace select $DEPLOY_ENVIRONMENT
```

- Init terraform state for this workspace:

```bash
terraform init
```

- Create a terraform variable file (and then add the variable values that you want to set):

```bash
touch env.$DEPLOY_ENVIRONMENT.tfvars
```

:warning: You can also add a custom variable value by using the cli arg `-var "var_name=**THE_VALUE**"`

- Lint terraform deployment (optional):

```bash
terraform fmt -check
```

- Plan terraform deployment:

```bash
terraform plan --var-file=env.$DEPLOY_ENVIRONMENT.tfvars
```

- Apply terraform deployment:

```bash
terraform apply --var-file=env.$DEPLOY_ENVIRONMENT.tfvars
```

You should see this output from terraform:

```bash
aggregator_endpoint = "https://aggregator.***.api.mithril.network/aggregator"
api_subdomain = "***.api.mithril.network."
external-ip = "35.195.148.171"
google_project = "mithril"
name_servers = tolist([
  "ns-cloud-d1.googledomains.com.",
  "ns-cloud-d2.googledomains.com.",
  "ns-cloud-d3.googledomains.com.",
  "ns-cloud-d4.googledomains.com.",
])
```

You will need to update the `NS` records of the **API sub domain** given by `api_subdomain` with the name servers listed in `name_servers`.

## Destroy an environment

- Destroy terraform deployment (optional):

```bash
terraform destroy --var-file=env.$DEPLOY_ENVIRONMENT.tfvars
```

## Bootstrap a Genesis Certificate (test only)

:warning: This operation will reset and invalidate the current `Certificate Chain`

The commands to run when bootstrapping is:

- Open a bash terminal on the VM and run the following command once connected:

```bash
docker exec mithril-aggregator /app/bin/mithril-aggregator -vvv genesis bootstrap
```

## Tools

The Mithril infrastructure comes with some scripts that help handle common tasks.

### Utils

This script is mainly used by the GitHub Actions workflows in the terraform deployments.

| Script                             | Description                                                                                                        | Usage                                                                   |
| ---------------------------------- | ------------------------------------------------------------------------------------------------------------------ | ----------------------------------------------------------------------- |
| `google-credentials-public-key.sh` | Extract a public key from a GCP credentials file and adds it to an authorized `ssh_keys` file for a specific user. | `./google-credentials-public-key.sh credentials.json ssh_keys username` |

### Pool

These scripts are used to create and maintain a **Cardano Stake Pool Operator (SPO)** on a Mithril network, for **Tests only**.

Connect to the VM and select the Cardano node and the Mithril signer node:

```
# Show debug output
export DEBUG=1

# Select Cardano network
export NETWORK=preview
export NETWORK_MAGIC=2

# Select Cardano node
export CARDANO_NODE=cardano-node-relay-signer-1

# Select Mithril signer node
export SIGNER_NODE=mithril-signer-1
```

In order to create a stake pool from scratch:

- Create keys with `create-keys.sh`
- Register the stake address of the pool with `register-stake-address.sh`
- Register the stake pool with `register-stake-pool.sh`

In order to query a stake pool information:

- Query the stake pool with `query-stake-pool.sh`

In order to maintain the stake pool:

- Renew the operational certificate of the stake pool with `renew-opcert.sh`

In order to retire a stake pool:

- Retire a stake poool with `retire-stake-pool.sh`

| Script                      | Description                                                          | Usage                                                                                                                                  |
| --------------------------- | -------------------------------------------------------------------- | -------------------------------------------------------------------------------------------------------------------------------------- |
| `create-keys.sh`            | Script for creating keys for a Cardano pool (SPO)                    | `./tools/pool/create-keys.sh`                                                                                                          |
| `query-stake-pool.sh`       | Script for querying info about a Cardano pool (SPO)                  | `./tools/pool/query-stake-pool.sh`                                                                                                     |
| `register-stake-address.sh` | Script for registering stake address of a Cardano pool (SPO)         | `TX_IN=***YOUR_TX_IN*** ./tools/pool/register-stake-address.sh`                                                                        |
| `register-stake-pool.sh`    | Script for registering a Cardano stake pool (SPO)                    | `TX_IN=***YOUR_TX_IN*** SIGNER_DOMAIN=***YOUR_SIGNER_DOMAIN_NAME*** POOL_TICKER=***YOUR_TICKER*** ./tools/pool/register-stake-pool.sh` |
| `renew-opcert.sh`           | Script for renewing Operational Certificate for a Cardano pool (SPO) | `./tools/pool/renew-opcert.sh`                                                                                                         |
| `retire-stake-pool.sh`      | Script for retiring a Cardano pool (SPO)                             | `TX_IN=***YOUR_TX_IN*** VALUE_OUT=***YOUR_VALUE_OUT*** ./tools/pool/retire-stake-pool.sh`                                              |

### Genesis

These scripts are used to fast bootstrap the genesis cerificates of a Mithril network, for **Tests only**.
The principle is to antedate the databases of the signers and aggregators so that we can bootstrap then a valid genesis certificate.

:warning: This process is experimental and recommended for expert users only.

| Script                                    | Description                                                                                                                                                                                                 | Usage                                                     |
| ----------------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | --------------------------------------------------------- |
| `fast-genesis-aggregator.sh`              | Script for antedating an aggregator database to run after the first signer registration (create fake `2` previous epochs with current epoch data for `stake`, `verification_key` and `protocol_parameters`) | `./tools/genesis/fast-genesis-aggregators.sh`             |
| `fast-genesis-signer.sh`                  | Script for antedating a signer database to run after the first signer registration (create fake `2` previous epochs with current epoch data for `stake`, `protocol_initializer`)                            | `./tools/genesis/fast-genesis-signer.sh`                  |
| `update-genesis-certificate.sh`           | Script for updating the content of the genesis certificate in the aggregator database                                                                                                                       | `./tools/genesis/update-genesis-certificate.sh`           |
| `update-stake-distribution-aggregator.sh` | Script for adding a Pool Id to an existing stake distribution in the aggregator database                                                                                                                    | `./tools/genesis/update-stake-distribution-aggregator.sh` |
| `update-stake-distribution-signer.sh`     | Script for adding a Pool Id to an existing stake distribution in the signer database                                                                                                                        | `./tools/genesis/update-stake-distribution-signer.sh`     |

Here is an example process to fast bootstrap a Mithril network on preview with `2` signer nodes:

```
# Set env var
export NETWORK=preview
export NETWORK_MAGIC=2
export SIGNER_DOMAIN=dev-${NETWORK}.api.mithril.network

# Check that all cardano nodes are synced
docker exec mithril-aggregator /app/bin/cardano-cli query tip --testnet-magic $NETWORK_MAGIC
docker exec mithril-signer-1 /app/bin/cardano-cli query tip --testnet-magic $NETWORK_MAGIC
docker exec mithril-signer-2 /app/bin/cardano-cli query tip --testnet-magic $NETWORK_MAGIC

## Clean signer db
sqlite3 data/$NETWORK/mithril-signer-1/mithril/stores/signer.sqlite3 "DELETE FROM protocol_initializer; DELETE FROM stake;"
sqlite3 data/$NETWORK/mithril-signer-2/mithril/stores/signer.sqlite3 "DELETE FROM protocol_initializer; DELETE FROM stake;"

## Clean aggregator db
sqlite3 data/$NETWORK/mithril-aggregator/mithril/stores/aggregator.sqlite3 "DELETE FROM certificate; DELETE FROM single_signature; DELETE FROM verification_key; DELETE FROM pending_certificate; DELETE FROM snapshot; DELETE FROM stake;"

## Restart nodes
docker restart mithril-aggregator
docker restart mithril-signer-1
docker restart mithril-signer-2

## If with certified signer
### Create certified signer and retrieve its pool id
CARDANO_NODE=cardano-node-signer-1 SIGNER_NODE=mithril-signer-1 POOL_TICKER=MSI01 ./tools/pool/create-keys-pool.sh
CARDANO_NODE=cardano-node-signer-1 SIGNER_NODE=mithril-signer-1 POOL_TICKER=MSI01 ./tools/pool/register-stake-address.sh
CARDANO_NODE=cardano-node-signer-1 SIGNER_NODE=mithril-signer-1 POOL_TICKER=MSI01 ./tools/pool/register-stake-pool.sh

CARDANO_NODE=cardano-node-signer-2 SIGNER_NODE=mithril-signer-2 POOL_TICKER=MSI02 ./tools/pool/create-keys-pool.sh
CARDANO_NODE=cardano-node-signer-2 SIGNER_NODE=mithril-signer-2 POOL_TICKER=MSI02 ./tools/pool/register-stake-address.sh
CARDANO_NODE=cardano-node-signer-2 SIGNER_NODE=mithril-signer-2 POOL_TICKER=MSI02 ./tools/pool/register-stake-pool.sh

## If with certified signer
### Update stake distribution with new pool ids
#### This is not completely working: you will have to wait for one epoch before going to the next step
./tools/genesis/update-stake-distribution-aggregator.sh $NETWORK mithril-aggregator $(cat ./data/$NETWORK/mithril-signer-1/cardano/pool/pool_id.txt) 100000000123456
./tools/genesis/update-stake-distribution-signer.sh $NETWORK mithril-signer-1 $(cat ./data/$NETWORK/mithril-signer-1/cardano/pool/pool_id.txt) 100000000123456
./tools/genesis/update-stake-distribution-signer.sh $NETWORK mithril-signer-2 $(cat ./data/$NETWORK/mithril-signer-1/cardano/pool/pool_id.txt) 100000000123456

./tools/genesis/update-stake-distribution-aggregator.sh $NETWORK mithril-aggregator $(cat ./data/$NETWORK/mithril-signer-2/cardano/pool/pool_id.txt) 100000000123456
./tools/genesis/update-stake-distribution-signer.sh $NETWORK mithril-signer-1 $(cat ./data/$NETWORK/mithril-signer-2/cardano/pool/pool_id.txt) 100000000123456
./tools/genesis/update-stake-distribution-signer.sh $NETWORK mithril-signer-2 $(cat ./data/$NETWORK/mithril-signer-2/cardano/pool/pool_id.txt) 100000000123456

## Wait until stake distribution is updated and signer is registered
docker logs -f --tail 250 mithril-aggregators
docker logs -f --tail 250 mithril-signer-1
docker logs -f --tail 250 mithril-signer-2

## Run fast genesis scripts
./tools/genesis/fast-genesis-aggregator.sh $NETWORK mithril-aggregator
./tools/genesis/fast-genesis-signer.sh $NETWORK mithril-signer-1
./tools/genesis/fast-genesis-signer.sh $NETWORK mithril-signer-2

## Bootstrap genesis certificate
docker exec mithril-aggregator /app/bin/mithril-aggregator -vvv genesis bootstrap

## Update genesis certificate to previous epoch
### Select current genesis certificate in the aggregator database
sqlite3 data/$NETWORK/mithril-aggregator/mithril/stores/aggregator.sqlite3 "SELECT * FROM certificate;"

### Recompute the genesis certificate hash
Run the following Rust snippet (see below)

### Update the current genesis certificate of the aggregator database
./tools/genesis/update-genesis-certificate.sh $NETWORK mithril-aggregator ***YOUR_UPDATED_CERTIFICATE_HASH*** '***YOUR_UPDATED_JSON_CERTIFICATE***'
```

Snippet code to re-compute a valid genesis certificate:

```rust
### Add this test to the mithril-common/src/crypto_helper/genesis.rs file and run the test
#[test]
fn update_genesis_certificate() {
    use crate::entities::Certificate;

    let certificate_json = "***YOUR_GENESIS_CERTIFICATE***";
    let mut certificate: Certificate = serde_json::from_str(certificate_json).unwrap();
    certificate.beacon.epoch -= 1;
    certificate.hash = certificate.compute_hash();
    println!(
        "UPDATED_CERTIFICATE_JSON={}",
        serde_json::to_string(&certificate).unwrap()
    );
}
```
