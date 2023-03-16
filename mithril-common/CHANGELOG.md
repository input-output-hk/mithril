# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## 0.2.20 (15-03-2023)
Initial CHANGELOG registry.
### Added
- New version of KES signature, which requires buffer allocation prior to key generation.
- Added wrapper struct to KES signature to allow copying the data. The new version of KES does not allow it anymore.
