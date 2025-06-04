# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

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
