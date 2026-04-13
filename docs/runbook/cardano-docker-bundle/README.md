# Create a Cardano Docker Bundle

## Introduction

This runbook describes how to build and publish a Docker image that bundles a Cardano node with the Mithril client and signer. The resulting image extends the official `ghcr.io/intersectmbo/cardano-node` image by adding statically-linked Mithril binaries, Mithril configuration files, and a custom entrypoint that starts both the Cardano node and the Mithril signer.

This runbook covers:

- Creating a GCP Artifact Registry Docker repository to host the bundle image
- Building static Mithril binaries (`mithril-client` and `mithril-signer`) for the `x86_64-unknown-linux-musl` target
- Building the Docker bundle image
- Publishing the image to the GCP Artifact Registry

## Pre-requisites

- `docker` installed
- Rust toolchain with the `x86_64-unknown-linux-musl` target installed
- [`gcloud` CLI](https://cloud.google.com/sdk/docs/install) installed and authenticated (for publishing)
- Access to the GCP project used by the Mithril infrastructure (for publishing)

## Configure environment variables

```bash
export CARDANO_NODE_VERSION=**CARDANO_NODE_VERSION**
export GCP_PROJECT_ID=**GCP_PROJECT_ID**
export GCP_REGION=europe-west1
export GCP_DOCKER_REPOSITORY=mithril-cardano-node-docker-test
export GCP_BUNDLE_IMAGE_REGISTRY=${GCP_REGION}-docker.pkg.dev/${GCP_PROJECT_ID}/${GCP_DOCKER_REPOSITORY}/cardano-node
```

Here is an example for version `10.6.3`:

```bash
export CARDANO_NODE_VERSION=10.6.3
export GCP_PROJECT_ID=iog-hydra
export GCP_REGION=europe-west1
export GCP_DOCKER_REPOSITORY=mithril-cardano-node-docker-test
export GCP_BUNDLE_IMAGE_REGISTRY=${GCP_REGION}-docker.pkg.dev/${GCP_PROJECT_ID}/${GCP_DOCKER_REPOSITORY}/cardano-node
```

And compute the short commit hash of the current state of the Mithril repository:

```bash
export MITHRIL_NODE_SHORT_COMMIT=$(git rev-parse --short HEAD)
```

## Step 1: Create a GCP Artifact Registry Docker repository (Optional)

Create a dedicated Docker repository in Artifact Registry to host the bundle image:

```bash
gcloud artifacts repositories create ${GCP_DOCKER_REPOSITORY} \
    --project=${GCP_PROJECT_ID} \
    --repository-format=docker \
    --location=${GCP_REGION} \
    --description="Repository for Cardano Mithril bundle images"
```

Make the repository publicly readable so that the infrastructure can pull the image without authentication:

```bash
gcloud artifacts repositories add-iam-policy-binding ${GCP_DOCKER_REPOSITORY} \
    --project=${GCP_PROJECT_ID} \
    --location=${GCP_REGION} \
    --member=allUsers \
    --role=roles/artifactregistry.reader
```

Verify the repository was created:

```bash
gcloud artifacts repositories describe ${GCP_DOCKER_REPOSITORY} \
    --project=${GCP_PROJECT_ID} \
    --location=${GCP_REGION}
```

## Step 2: Build the Mithril binaries

From the root of the Mithril repository, build the static Mithril client and signer binaries for the `x86_64-unknown-linux-musl` target:

```bash
cargo build --release --target x86_64-unknown-linux-musl -p mithril-signer -p mithril-client-cli
```

Verify the binaries are available:

```bash
ls -lh ../../../target/x86_64-unknown-linux-musl/release/mithril-client
ls -lh ../../../target/x86_64-unknown-linux-musl/release/mithril-signer
```

## Step 3: Build the Docker image

From the `docs/runbook/cardano-docker-bundle/` directory, build the Docker image:

```bash
docker build ../../.. -f ./docker/Dockerfile \
    -t ${GCP_BUNDLE_IMAGE_REGISTRY}:${CARDANO_NODE_VERSION}-bundle-${MITHRIL_NODE_SHORT_COMMIT} \
    --progress=plain \
    --build-arg CARDANO_NODE_VERSION=${CARDANO_NODE_VERSION}
```

> [!NOTE]
> The build context is the repository root (`../../..`) because the Dockerfile needs access to the Mithril binaries in `target/` and the configuration files in `mithril-infra/configuration/`.

Verify the image is available locally:

```bash
docker image ls ${GCP_BUNDLE_IMAGE_REGISTRY}:${CARDANO_NODE_VERSION}-bundle-${MITHRIL_NODE_SHORT_COMMIT}
```

## Step 4: Publish the image to GCP Artifact Registry (Optional)

### Authenticate Docker with the Artifact Registry

```bash
gcloud auth configure-docker ${GCP_REGION}-docker.pkg.dev
```

### Push the image to Artifact Registry

```bash
docker push ${GCP_BUNDLE_IMAGE_REGISTRY}:${CARDANO_NODE_VERSION}-bundle-${MITHRIL_NODE_SHORT_COMMIT}
```

### Verify the image is available

```bash
gcloud artifacts docker images list \
    ${GCP_REGION}-docker.pkg.dev/${GCP_PROJECT_ID}/${GCP_DOCKER_REPOSITORY} \
    --project=${GCP_PROJECT_ID}
```

## Step 5: Run the Docker image

The bundle image supports two modes of operation: **relay/passive** (default) and **block producer**. The mode is controlled by the `CARDANO_BLOCK_PRODUCER` environment variable.

The underlying Cardano node entrypoint supports three modes (see [entrypoint source](https://github.com/IntersectMBO/cardano-node/blob/master/nix/docker/context/node/bin/entrypoint)):

- **Scripts mode**: set `NETWORK` to use the default network configuration
- **Merge mode**: set `NETWORK` along with `CARDANO_CONFIG_JSON_MERGE` and/or `CARDANO_TOPOLOGY_JSON_MERGE` to deep-merge custom JSON into the default configuration
- **Custom mode**: leave `NETWORK` unset and pass `run ...` arguments directly

### Run as a relay/passive node

In relay/passive mode, only the Cardano node is started (the Mithril signer is not started). The Cardano node entrypoint uses **scripts mode** to auto-configure the node for the specified network:

```bash
docker run \
    -v cardano-node-ipc:/ipc \
    -v cardano-node-data:/data \
    -e NETWORK=preview \
    ${GCP_BUNDLE_IMAGE_REGISTRY}:${CARDANO_NODE_VERSION}-bundle-${MITHRIL_NODE_SHORT_COMMIT}
```

> [!NOTE]
> You can bind-mount a host directory for the database instead of using a Docker volume. For example, replace `-v cardano-node-data:/data` with `--mount type=bind,source="/path/to/db",target=/data/db/`.

### Run as a block producer node

In block producer mode, the Mithril signer is started alongside the Cardano node. The bundle entrypoint unsets `NETWORK` before reaching the Cardano entrypoint, which enables **custom mode**: the Cardano node is fully configured via `CARDANO_*` environment variables (see [run-node source](https://github.com/IntersectMBO/cardano-node/blob/master/nix/docker/context/node/bin/run-node)). This is well suited for `docker-compose` files where all configuration is declared as environment variables. The `NETWORK` variable is still available to the Mithril signer (which is forked before the unset).

The block producer credentials (KES key, VRF key, operational certificate) must be mounted at `/opt/cardano/config/keys/`:

```bash
docker run \
    -v cardano-node-ipc:/ipc \
    -v cardano-node-data:/data \
    -e NETWORK=preview \
    -e CARDANO_BLOCK_PRODUCER=true \
    -e CARDANO_CONFIG=/opt/cardano/config/preview/config.json \
    -e CARDANO_TOPOLOGY=/opt/cardano/config/preview/topology.json \
    --mount type=bind,source="/path/to/keys",target=/opt/cardano/config/keys \
    ${GCP_BUNDLE_IMAGE_REGISTRY}:${CARDANO_NODE_VERSION}-bundle-${MITHRIL_NODE_SHORT_COMMIT} \
    run
```

> [!NOTE]
> The key files must be named `kes.skey`, `vrf.skey`, and `node.cert` inside the mounted directory (matching the [run-node defaults](https://github.com/IntersectMBO/cardano-node/blob/6480fb75e8b042453a00f9c7b75465e4660c05fe/nix/docker/context/node/bin/run-node)). The Mithril signer automatically uses the same default key paths. To use custom paths, set the `CARDANO_SHELLEY_KES_KEY`, `CARDANO_SHELLEY_VRF_KEY`, and `CARDANO_SHELLEY_OPERATIONAL_CERTIFICATE` environment variables.

The following Cardano node environment variables are available in custom mode (all have sensible defaults in `run-node`):

| Variable                                  | Description                         | Default                                     |
| ----------------------------------------- | ----------------------------------- | ------------------------------------------- |
| `CARDANO_CONFIG`                          | Path to the node configuration file | `/opt/cardano/config/mainnet/config.json`   |
| `CARDANO_TOPOLOGY`                        | Path to the node topology file      | `/opt/cardano/config/mainnet/topology.json` |
| `CARDANO_DATABASE_PATH`                   | Path to the node database           | `/data/db`                                  |
| `CARDANO_SOCKET_PATH`                     | Path to the node IPC socket         | `/ipc/node.socket`                          |
| `CARDANO_PORT`                            | Node listening port                 | `3001`                                      |
| `CARDANO_BIND_ADDR`                       | Node bind address                   | `0.0.0.0`                                   |
| `CARDANO_BLOCK_PRODUCER`                  | Enable block producer mode          | `false`                                     |
| `CARDANO_SHELLEY_KES_KEY`                 | Path to the KES signing key         | `/opt/cardano/config/keys/kes.skey`         |
| `CARDANO_SHELLEY_VRF_KEY`                 | Path to the VRF signing key         | `/opt/cardano/config/keys/vrf.skey`         |
| `CARDANO_SHELLEY_OPERATIONAL_CERTIFICATE` | Path to the operational certificate | `/opt/cardano/config/keys/node.cert`        |

### Mithril signer configuration

The following environment variables can be used to configure the Mithril signer when running in block producer mode (`CARDANO_BLOCK_PRODUCER=true`):

| Variable                       | Description                                       | Default                                                                                     |
| ------------------------------ | ------------------------------------------------- | ------------------------------------------------------------------------------------------- |
| `NETWORK`                      | Cardano network (`preview`, `preprod`, `mainnet`) | (required)                                                                                  |
| `KES_SECRET_KEY_PATH`          | Path to the KES secret key file                   | Value of `CARDANO_SHELLEY_KES_KEY`, or `/opt/cardano/config/keys/kes.skey`                  |
| `OPERATIONAL_CERTIFICATE_PATH` | Path to the operational certificate file          | Value of `CARDANO_SHELLEY_OPERATIONAL_CERTIFICATE`, or `/opt/cardano/config/keys/node.cert` |
| `AGGREGATOR_ENDPOINT`          | URL of the Mithril aggregator                     | `https://aggregator.<mithril_network>.api.mithril.network/aggregator`                       |
| `RUN_INTERVAL`                 | Signer run interval in milliseconds               | `120000`                                                                                    |
| `DB_DIRECTORY`                 | Path to the Cardano node database                 | `/data/db`                                                                                  |
| `CARDANO_NODE_SOCKET_PATH`     | Path to the Cardano node IPC socket               | `/ipc/node.socket`                                                                          |
| `DATA_STORES_DIRECTORY`        | Path to the Mithril signer data stores            | `/data/mithril/stores`                                                                      |
| `STORE_RETENTION_LIMIT`        | Number of data stores to retain                   | `5`                                                                                         |
| `ERA_READER_ADAPTER_TYPE`      | Era reader adapter type                           | `cardano-chain`                                                                             |
| `ERA_READER_ADAPTER_PARAMS`    | Era reader adapter parameters (JSON)              | Auto-configured from bundled configuration files                                            |
| `SIGNER_MAX_RETRIES`           | Maximum number of signer startup retries          | `5000`                                                                                      |

> [!TIP]
> The Mithril signer logs are written to `/data/mithril/signer.log` inside the container. To inspect them, run:
>
> ```bash
> docker exec <container_id> cat /data/mithril/signer.log
> ```
>
> If the `/data` directory is mounted as a volume, the log file is also accessible directly from the host.
