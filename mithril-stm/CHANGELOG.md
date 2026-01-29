# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## 0.9.11 (01-29-2026)

### Added

- Removed `Ok()` wrapper during conversion from `KeyRegistration` to `MerkleTree`.

## 0.9.10 (01-26-2026)

### Added

- `MerkleTreeSnarkLeaf` is added.

## 0.9.9 (01-23-2026)

### Changed

- Refactored `AggregateVerificationKey` for better SNARK support.

## 0.9.8 (01-23-2026)

### Changed

- `KeyRegistration` functionality is updated to support different types of registrations.

## 0.9.7 (01-23-2026)

### Added

- Implemented `Digest` trait for the Poseidon hash in a new `hash` module.

## 0.9.6 (01-23-2026)

### Added

- Integrated the Halo2 certificate circuit prototype behind the `future_snark` feature.

## 0.9.5 (01-19-2026)

### Changed

- Changed backend for Jubjub and Poseidon to Midnight ZK in `unique_schnorr_signature`.

## 0.9.4 (01-14-2026)

### Changed

- Renamed `schnorr_signature` module to `unique_schnorr_signature`.

## 0.9.3 (01-13-2026)

### Added

- Golden test for `avk` is added.

## 0.9.2 (01-09-2026)

### Added

- Golden test for closed key registration is added.

## 0.9.1 (01-07-2026)

### Added

- `SingleSignatureForConcatenation` is added.

## 0.9.0 (01-05-2026)

### Removed

- Removed the deprecated structures and functions in version `0.5.0`.

## 0.8.4 (12-30-2025)

### Added

- Golden tests added for `SingleSignature`.

## 0.8.3 (12-23-2025)

### Changed

- The `MerkleTreeLeaf` struct is converted to a trait to support multiple leaf types.

## 0.8.2 (12-23-2025)

### Added

- Golden tests added for `MerkleTreeBatchCommitment`.

## 0.8.1 (12-22-2025)

### Added

- Jubjub wrapper is added for `schnorr_signature` module.

## 0.8.0 (12-17-2025)

### Changed

- The `D: Digest` generic is changed with a `D: MembershipDigest` trait supporting multiple hash functions for different proof systems.

## 0.7.0 (12-16-2025)

### Removed

- Basic verifier functionality is removed.

## 0.6.4 (12-12-2025)

### Changed

- Error types in the Stm library moved to corresponding sub-modules.

## 0.6.2 (11-27-2025)

### Changed

- Stm library re-organized for SNARK-friendliness.

## 0.6.1 (11-27-2025)

### Added

- Added Schnorr signature modules.

## 0.6.0 (11-19-2025)

### Changed

- Stm error handling is done with `anyhow`.

## 0.5.5 (10-13-2025)

### Fixed

- Fixed compilation issues with `rug` when targeting `musl` environment.

## 0.5.4 (10-07-2025)

### Added

- Added golden tests for JSON (de)serialization.

## 0.5.3 (10-07-2025)

### Added

- Added support for multiple aggregate signature proof systems.

## 0.5.0 (09-09-2025)

### Changed

- Bumped new minor version for release following deprecations of structure names and functions.

## 0.4.9 (07-07-2025)

### Changed

- Function names are changed.

## 0.4.8 (02-07-2025)

### Deprecated

- Old struct names are deprecated.

## 0.4.6 (27-06-2025)

### Changed

- Struct names are changed.

## 0.4.5 (23-06-2025)

### Changed

- `gen` will become a keyword of the language with Rust 2024 so, `SigningKey::gen` is renamed as `SigningKey::generate`.

### Removed

- Deprecated `batch-verify-aggregates` feature is removed.

## 0.4.4 (12-06-2025)

### Changed

- Fixed the `from_bytes` implementations to avoid panics with safe slice range gets.
- Fixed the `to_bytes`/`from_bytes` implementations of the `StmAggrSig` to support variable length signatures.

### Added

- Integration tests for to ensure correct serialization and deserialization to bytes.

## 0.4.3 (11-06-2025)

### Added

- Added a `parameter` module and `StmParameters` functionality covered by that.

### Changed

- Moved doc test to `lib.rs`.

## 0.4.2 (04-06-2025)

### Added

- Added a `aggregate_signature` module and `StmAggrSig`, `StmAggrVerificationKey`, `StmClerk` and `CoreVerifier` functionality covered by its submodules.

## 0.4.1 (04-06-2025)

### Added

- Added a `single_signature` module and `StmSig` and `StmSigRegParty` functionality covered by its submodules.

## 0.4.0 (15-05-2025)

### Added

- Added a `participant` module and `StmInitializer` and `StmSigner` functionality covered by its submodules.

### Changed

- STM module visibilities are changed.

## 0.3.44 (28-04-2025)

- Removed the build script and deprecated `batch-verify-aggregate` feature as the code behind this feature is now
  compatible with WASM.

## 0.3.43 (05-04-2025)

### Added

- Added a `bls_multi_signature` module and multi-signature functionality covered by its submodules.

## 0.3.41 (20-03-2025)

### Added

- Added a `merkle_tree` module and Merkle tree functionality covered by its submodules:
  - Module `leaf`: Includes structure `MTLeaf` and its implementation.
  - Module `path`: Includes structures `Path` and `BatchPath` and their implementations.
  - Module `commitment`: Includes structures `MerkleTreeCommitment` and `MerkleTreeCommitmentBatchCompat` and their implementations.
  - Module `tree`: Includes structure `MerkleTree` and its implementation.

## 0.3.40 (18-03-2025)

### Changed

- Moved implementation blocks under their respective structures.
- Ordered property tests.

## 0.3.18 (11-04-2024)

- Deprecate `portable` feature:
  - Instead, always enable BLST `portable` feature for runtime check of intel ADX instruction set.
  - `portable` feature now has no effect and should be removed from crate dependencies.

## 0.3.7 (10-10-2023)

### Added

- Implemented a build script that automatically detects that WASM is not targeted, and automatically activates the feature `batch-verify-aggregates` (which enables batch verification of signatures).

## 0.3.0 (10-08-2023)

### Added

- Added `Coreverifier` struct and its functionalities to cover signature procedure for a full node.
- Adapted existing functionality to inherit from a more generic structure `Coreverifier`.
- Added tests for core verification.

## 0.2.5 (15-03-2023)

### Added

- Included helper functions for unsafe code
- Added tests for batch verification

## 0.2.1 (04-01-2023)

### Added

- Batch verification for `StmAggrSig`.

## 0.2.0 (16-12-2022)

### Changed

- Adapted the `Signature` struct, so that it does not contain the verification key and
  the stake, as these values are not required.

## 0.1.0 (05-12-2022)

Initial release.
