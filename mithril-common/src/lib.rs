#![warn(missing_docs)]

//! Shared datatypes and traits used by Mithril rust projects
//!
//! Provide:
//! - A way to store data with the [store] types
//! - [Digester][digesters] to compute mithril digest from a Cardano database
//! - Helpers for the [Mithril STM](https://mithril.network/rust-doc/mithril_stm/index.html)
//! lib with the [crypto_helper].
//! - A [certificate chain] used to validate the Certificate Chain created by an aggregator
//! - The [entities] used by, and exchanged between, the aggregator, signers and client.

pub mod api;
mod beacon_provider;
pub mod certificate_chain;
pub mod chain_observer;
pub mod crypto_helper;
pub mod database;
pub mod digesters;
pub mod entities;
pub mod era;
pub mod messages;
pub mod sqlite;
pub mod store;
pub mod test_utils;

use std::error::Error;

pub use beacon_provider::{BeaconProvider, BeaconProviderError, BeaconProviderImpl};
pub use entities::{CardanoNetwork, MagicId};

use lazy_static::lazy_static;
use semver::{Version, VersionReq};

/// Generic error type
pub type StdError = Box<dyn Error + Sync + Send>;

/// Mithril API protocol version
/// this is the same as the one in openapi.yml file.
/// If you want to update this version to reflect changes in the protocol,
/// please also update the entry in the openapi.yml
pub const MITHRIL_API_VERSION: &str = "0.1.1";

/// Mithril API protocol version header name
pub const MITHRIL_API_VERSION_HEADER: &str = "mithril-api-version";

/// Mithril Signer node version header name
pub const MITHRIL_SIGNER_VERSION_HEADER: &str = "signer-node-version";

lazy_static! {
    /// The [SemVer version requirement][semver::VersionReq] associated with the [MITHRIL_API_VERSION].
    ///
    /// A beta version (0.x.y) will allow all versions within the same major & minor.
    /// A stable version (>=1.x.y) will allow all versions within the same major.
    pub static ref MITHRIL_API_VERSION_REQUIREMENT: VersionReq =
        build_requirement_from_version(&Version::parse(MITHRIL_API_VERSION).unwrap());
}

fn build_requirement_from_version(version: &Version) -> VersionReq {
    let mut req_version = version.clone();
    req_version.patch = 0;

    if version.major > 0 {
        req_version.minor = 0;
    }

    VersionReq::parse(&req_version.to_string()).unwrap()
}

#[cfg(test)]
mod test {
    use crate::{
        build_requirement_from_version, MITHRIL_API_VERSION, MITHRIL_API_VERSION_REQUIREMENT,
    };
    use semver::{Version, VersionReq};

    const API_SPEC_FILE: &str = "../openapi.yaml";

    fn assert_versions_matches(versions: &[&str], requirement: &VersionReq) {
        for string in versions {
            let version = Version::parse(string).unwrap();
            assert!(
                requirement.matches(&version),
                "Version {} did not match requirement: {}",
                &version,
                requirement
            );
        }
    }

    fn assert_versions_dont_matches(versions: &[&str], requirement: &VersionReq) {
        for string in versions {
            let version = Version::parse(string).unwrap();
            assert!(
                !requirement.matches(&version),
                "Did not expect that version {} match requirement: {}",
                &version,
                requirement
            );
        }
    }

    #[test]
    fn test_semver_requirement_matching() {
        let beta_requirement = build_requirement_from_version(&Version::parse("0.2.4").unwrap());
        assert_versions_matches(&["0.2.0", "0.2.4", "0.2.5", "0.2.99"], &beta_requirement);
        assert_versions_dont_matches(&["0.1.10", "0.3.0", "1.0.0"], &beta_requirement);

        let stable_requirement = build_requirement_from_version(&Version::parse("2.1.4").unwrap());
        assert_versions_matches(
            &["2.0.0", "2.1.0", "2.1.4", "2.1.5", "2.12.8"],
            &stable_requirement,
        );
        assert_versions_dont_matches(&["0.0.0", "1.11.9", "3.0.0"], &stable_requirement);
    }

    #[test]
    fn requirement_parsed_from_api_version_should_match_said_api_version() {
        let api_version = Version::parse(MITHRIL_API_VERSION).unwrap();
        assert!(MITHRIL_API_VERSION_REQUIREMENT.matches(&api_version));
    }

    #[test]
    fn api_version_constant_should_match_version_in_openapi_yaml() {
        let yaml_spec = std::fs::read_to_string(API_SPEC_FILE).unwrap();
        let openapi: serde_json::Value = serde_yaml::from_str(&yaml_spec).unwrap();
        let openapi_version = openapi["info"]["version"].as_str().unwrap();

        assert_eq!(
            openapi_version, MITHRIL_API_VERSION,
            "MITHRIL_API_VERSION constant should always be synced with openapi.yaml version"
        );
    }
}
