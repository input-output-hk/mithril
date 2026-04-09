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

## Step 4: Publish the image to GCP Artifact Registry

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
