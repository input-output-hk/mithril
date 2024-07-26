# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

As a minor extension, we have adopted a slightly different versioning convention for the Mithril distributions (<https://mithril.network/doc/adr/3#decision>)

## Mithril Distribution [XXXX] - UNRELEASED

- Crates versions:

| Crate | Version |
| ----- | ------- |
| N/A   | `-`     |

## Mithril Distribution [2430.0] - UNRELEASED

- `mithril-aggregator` node produces artifact for different signed entity types in parallel.

- Fix `Agency is theirs` error in the `ChainReader` when the underlying `Chain sync` client does not have agency.

- Support for `Cardano node` `9.1.0` in the signer and the aggregator.

- Support better disk configuration in terraform deployments with the CI/CD workflows.

- **UNSTABLE** Cardano transactions certification:

  - Make Cardano transaction signing settings configurable via the CD.

- Crates versions:

| Crate | Version |
| ----- | ------- |
| N/A   | `-`     |

## Mithril Distribution [2428.0] - 2024-07-15

- Provide a feature to the `mithril-client` crate to allow selection of the TLS implementation used by the dependent `reqwest` crate.

- Implement a reset mechanism for mutable resources returned to a pool (`ResourcePool`) to keep it in a consistent state.

- Implement a lock mechanism on `SignedEntityType` to prevent concurrent work on a same entity type.

- Extended CI build and test steps for MacOS `arm64` runners and include pre-built binaries for MacOS `arm64` in the releases.

- Add a regularly run upkeep task to the `mithril-aggregator` and `mithril-signer` to clean up stale data and optimize their databases.

- Support for `Cardano node` `9.0.0` in the signer and the aggregator.

- Refactor the Cardano node configuration in the infrastructure.

- Add prettier configuration to standardize the code formatting in the repository.

- Field `beacon` becomes optional in `CertificatePendingMessage` response of `/certificate-pending` route.

- **UNSTABLE** Cardano transactions certification:

  - Optimize the performances of the computation of the proof with a Merkle map.
  - Handle rollback events from the Cardano chain by removing stale data.
  - Preload Cardano transactions and Block Range Roots at signer & aggregator startup.
  - Chunk the Cardano transactions import in `mithril-signer` to reduce disk footprint by running the pruning process more frequently.
  - Add a database connection pool on the Cardano transaction repository for increased performances of the prover.
  - Import Cardano transactions with Chain Sync mini protocol and Pallas chain reader.
  - Avoid aggregator and signer being blocked when importing the Cardano transactions.

- Crates versions:

| Crate               | Version   |
| ------------------- | --------- |
| mithril-aggregator  | `0.5.40`  |
| mithril-client      | `0.8.7`   |
| mithril-client-cli  | `0.9.6`   |
| mithril-client-wasm | `0.3.7`   |
| mithril-common      | `0.4.29`  |
| mithril-signer      | `0.2.161` |
| mithril-stm         | `0.3.24`  |

## Mithril Distribution [2423.0] - 2024-06-12

- **BREAKING** changes in Mithril client CLI:

  - The deprecated `snapshot` command is removed from the Mithril client CLI
  - Use the `cardano-db snapshot` command instead.

- Update website and explorer user interface to use the new mithril logo.

- **UNSTABLE** Cardano transactions certification:

  - Support computation of the Cardano Transactions signature and proving with the pre-computed Block Range Merkle Roots retrieved from the database.

  - Prune Cardano Transactions from the signer database after the Block Range Merkle Roots have been computed.

  - Implement a Chain Reader which retrieves blocks from the Cardano chain with Pallas through the `chainsync` mini-protocol.

  - Implement a Resource Pool and use it for caching Block Range Merkle maps used by the Cardano transactions prover and improving the throughput.

  - Change the beacon of the Cardano Transactions to a block number instead of an immutable file number.

- Crates versions:

| Crate               | Version   |
| ------------------- | --------- |
| mithril-aggregator  | `0.5.16`  |
| mithril-client      | `0.8.3`   |
| mithril-client-cli  | `0.9.2`   |
| mithril-client-wasm | `0.3.3`   |
| mithril-common      | `0.4.13`  |
| mithril-signer      | `0.2.141` |
| mithril-stm         | `0.3.22`  |

## Mithril Distribution [2418.1] - 2024-05-13

- **BREAKING** changes in Mithril client CLI:

  - Certificate chain structure has been modified to remove coupling with immutable file number.
  - Client needs to be updated to verify certificate chain.

- Support incremental import for Cardano Transactions instead of scanning the whole immutable database for every signing round.

- Chain observers support the retrieval of the current Cardano chain point.

- Deprecate `portable` feature of `mithril-stm` and `mithril-client`:

  - Instead, always enable BLST `portable` feature in `mithril-stm` for runtime check of intel ADX instruction set.
  - `portable` feature now has no effect and should be removed from crate dependencies.
  - Removed it from all other crates (including `mithril-common`).

- Switched memory allocator to `jemallocator` on signer and aggregator to avoid memory fragmentation when signing transactions (which lead to RES memory not being properly returned to the OS).

- Crates versions:

| Crate               | Version   |
| ------------------- | --------- |
| mithril-aggregator  | `0.5.0`   |
| mithril-client      | `0.8.0`   |
| mithril-client-cli  | `0.8.0`   |
| mithril-client-wasm | `0.3.0`   |
| mithril-common      | `0.4.0`   |
| mithril-signer      | `0.2.130` |
| mithril-stm         | `0.3.19`  |

## Mithril Distribution [2412.0] - 2024-03-26

- **GitHub release**: <https://github.com/input-output-hk/mithril/releases/tag/2412.0>

- _DEPRECATED_ the `snapshot` command in the Mithril client CLI:

  - Renamed to `cardano-db snapshot`.
  - Will be **removed** in **2** distributions.

- Support for `Prometheus` endpoint for metrics in signer ([setup guide](https://mithril.network/doc/next/manual/getting-started/run-signer-node#activate-prometheus-endpoint)).

- Full support for chain observer with `Pallas` in signer and aggregator.

- Support for `Cardano node` `8.9.0` in the signer and the aggregator.

- Bug fixes and performance improvements.

- Crates versions:

| Crate               | Version   |
| ------------------- | --------- |
| mithril-aggregator  | `0.4.49`  |
| mithril-client      | `0.6.9`   |
| mithril-client-cli  | `0.7.6`   |
| mithril-client-wasm | `0.2.5`   |
| mithril-common      | `0.3.21`  |
| mithril-signer      | `0.2.116` |
| mithril-stm         | `0.3.17`  |
