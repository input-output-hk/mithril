# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

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