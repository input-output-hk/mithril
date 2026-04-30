# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## 0.10.13 (05-01-2026)

### Added

- Added golden test to detect when a change in any of the circuits happens.
- Added functions to compute the verification keys for recursive and non-recursive circuits.
- Added a runbook explaining the steps to follow to update the verification keys when a change happens.

## 0.10.12 (04-30-2026)

### Added

- Fixed the flakiness in some of the circuit tests by making the generation of the srs more stable for access by multiple threads.

## 0.10.11 (04-30-2026)

### Added

- Added Layer B state transition tests for the `halo2_ivc` prototype, covering positive and negative transition checks for genesis, same-epoch, and next-epoch steps, including fast asset-based verifier rejections and slow MockProver constraint checks for linkage and hash consistency rules.

## 0.10.10 (04-28-2026)

### Changed

- Moved some tests in slow modules and changed some parameters to make the SNARK tests faster

## 0.10.9 (04-28-2026)

### Changed

- Split slow `MockProver` tests into `mod slow` submodules in the `halo2_ivc` test suite and registered `circuits::halo2_ivc` in the CI slow-test filter, preventing slow tests from triggering on unrelated file changes.

## 0.10.8 (04-28-2026)

### Added

- Added Layer A encoding and transition tests for the `halo2_ivc` prototype, covering preimage byte-range extraction, state public input ordering, genesis state initialisation, accumulator and verifying key serialisation round-trips, tampered-public-input rejection, and MockProver constraint checks for wrong preimage bytes.

## 0.10.6 (04-23-2026)

### Added

- Updated the creation of merkle path during witness generation to use a constant length for the path by completing it with `0` padding.
- Updated the non-recursive circuit to ignore `0` values during the check of the merkle path to exclude the padding from the computation of the root.

## 0.10.5 (04-23-2026)

### Added

- Added Layer D golden tests for the `halo2_ivc` prototype, covering committed asset readers, deterministic asset generators, and a positive golden suite validating stored recursive proofs, the genesis base case, same-epoch and next-epoch recursive steps, and a chained-flow replay check.

## 0.10.4 (04-22-2026)

### Changed

- Enhanced the index selection mechanism of snark proof system.

## 0.10.1 (04-09-2026)

### Changed

- Reduced the `lower_than` constraint bit-length from 32 bits to 16 bits in the non-recursive circuit implementation.

## 0.10.0 (04-09-2026)

### Changed

- Refactored the bytes codec for all types with cbor bytes encoding and decoding, and added support for backward/forward compatibility.

## 0.9.38 (04-09-2026)

### Added

- Added a feature-gated Halo2 IVC prototype module.

## 0.9.37 (04-01-2026)

### Added

- Added a fix for the encoded message received not matching the expected length
- Added a way to save and load the srs and to cache the circuit verification key (only for testing purposes)

## 0.9.36 (03-27-2026)

### Added

- Added support for SNARK in aggregate signature proof systems.

## 0.9.35 (03-26-2026)

### Changed

- Changed proptest range for `phi_f` to not use floats anymore as it was cause severe slowdowns in the computations.

## 0.9.34 (03-24-2026)

### Changed

- Added a verify functionnality to the SnarkProof and a wrapper for MidnightVK to make handling the circuit verification key easier.

## 0.9.33 (03-20-2026)

### Changed

- Switched the Halo2 SNARK transcript from Blake2b to Poseidon in STM proof generation and circuit tests.

## 0.9.32 (03-19-2026)

### Changed

- Created a `SnarkProver` and `SnarkProof` structures in `proof_system/halo2_snark/proof.rs` that are responsible for creating a snark proof.
- Created a `unsafe_helpers.rs` file that handles the unsafe snark setup (`srs`, `circuit` `verification_key`, `proving_key`) that will be replaced later.

## 0.9.31 (03-19-2026)

### Changed

- Refactored the Halo2 STM circuit to clarify module boundaries, keep `circuit.rs` focused on orchestration, split gadget logic into dedicated modules, introduce explicit witness/instance types, and add focused unit tests for adapters and gadgets.

## 0.9.30 (03-16-2026)

### Added

- Added preparation of SNARK prover input.

## 0.9.29 (03-13-2026)

### Changed

- Completed the Halo2 STM naming alignment across circuit types, gadgets, and golden tests, including clearer circuit witness naming and constraint-oriented lottery gadget semantics.

## 0.9.28 (03-12-2026)

### Changed

- Replaced the default lottery target value used to create `ClosedRegistrationEntry` by the actual computation.

## 0.9.27 (03-11-2026)

### Changed

- Introduced circuit-local Halo2 types in `circuits/halo2/types.rs` with shared `CircuitBaseField`, `CircuitBase`, and `CircuitCurve`.
- Replaced duplicated local `F`/`C` aliases with shared circuit types across Halo2 circuit, gadgets, and golden helper code.
- Standardized Halo2 conversion paths using `From`/`Into` implementations for circuit/domain field wrappers.
- Added `circuits/halo2/adapters.rs` to convert STM Merkle paths into Halo2 witness paths for circuit consumption.
- Removed `circuits/halo2/utils/mod.rs` and inlined field-limb split logic into `circuits/halo2/gadgets.rs`.
- Unified synthesis error mapping through `to_synthesis_error` in `circuits/halo2/errors.rs`.

## 0.9.26 (03-09-2026)

### Changed

- Updated the circuit creation function to ensure the types coming from the STM protocol are compatible with the circuit types.

## 0.9.25 (03-06-2026)

### Changed

- Removed panic-based failure paths from the Halo2 SNARK circuit prototype and switched to structured `Result`-based error handling.
- Replaced `assert!(self.quorum < self.num_lotteries)` with explicit validation returning a typed circuit error.
- Added pre-validation guards to reject malformed witnesses and inconsistent Merkle path shapes before Midnight stdlib panics.
- Reworked remaining failure paths in `circuits/halo2/circuit.rs` and `circuits/halo2/gadgets.rs` to return structured errors.
- Updated Halo2 negative tests to assert structured errors rather than using `#[should_panic]`.

## 0.9.24 (03-04-2026)

### Added

- Added SNARK compatibility for signer and single signature.

## 0.9.23 (03-03-2026)

### Changed

- Renamed `DST_SIGNATURE` to `DOMAIN_SEPARATION_TAG_SIGNATURE` in STM signature scheme exports and usage chain.
- Updated Halo2 circuit code to use STM-level `DOMAIN_SEPARATION_TAG_LOTTERY` and `DOMAIN_SEPARATION_TAG_SIGNATURE` directly.

### Removed

- Removed `circuits/halo2/constants.rs` as DST constants are now sourced from STM signature scheme constants.

### Added

- Added Halo2 DST alignment unit tests for:
  - collision guard (`signature DST != lottery DST`),
  - reference signature DST formula check vs STM computation,
  - reference lottery DST formula check vs STM computation.

## 0.9.22 (03-03-2026)

### Added

- Added negative tests for `SingleSignature::verify` to reject invalid inputs (wrong verification key, out-of-bounds index, wrong message, and mismatched registration).

## 0.9.20 (03-02-2026)

### Changed

- Changed the number of iterations of the Taylor expansions used to compute the lottery target value.

## 0.9.19 (02-25-2026)

### Fixed

- Flakiness in the CI due to non deterministic test SRS generation.

## 0.9.18 (02-20-2026)

### Added

- Added a `from_raw` conversion function for `BaseFieldElement` that applies modulus reduction to the input.

## 0.9.17 (02-19-2026)

### Added

- Added the computation of the lottery target value for SNARK.
- Switched back-end for computation from rug to num-integer.

## 0.9.16 (02-18-2026)

### Changed

- Switched Halo2 circuit and golden helpers to reuse existing STM types and implementations.
- Applied internal refactoring and cleanup in Halo2 witness and helper layers.

### Removed

- Removed the `circuits/halo2/off-circuit` module.

## 0.9.15 (02-13-2026)

### Added

- Added SNARK compatibility for key registration.

## 0.9.14 (02-11-2026)

### Added

- Fixed `hash_to_projective_point` function and Schnorr signature implementation to match the circuit.

## 0.9.13 (02-06-2026)

### Added

- Added golden tests for the Halo2 STM circuit.

## 0.9.12 (02-02-2026)

### Added

- Added `halo2_snark` module for `proof_system` and updated `tree.rs` with new leaf type.

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
