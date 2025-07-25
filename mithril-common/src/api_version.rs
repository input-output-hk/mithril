//! API Version provider service
include!(concat!(env!("OUT_DIR"), "/open_api.rs"));
use anyhow::anyhow;
use semver::{Version, VersionReq};
use std::collections::HashMap;
use std::sync::Arc;

use crate::StdResult;

/// API Version provider
#[derive(Clone)]
pub struct APIVersionProvider {
    alternate_file_discriminator: Arc<dyn ApiVersionDiscriminantSource>,
    open_api_versions: HashMap<OpenAPIFileName, Version>,
}

/// Trait to get the discriminant that identifies the alternate `openapi` file to use first if found,
/// in place of the default `openapi.yml` file.
#[cfg_attr(test, mockall::automock)]
pub trait ApiVersionDiscriminantSource: Send + Sync {
    /// Get the discriminant that identifies the alternate `openapi` file
    fn get_discriminant(&self) -> String;
}

impl APIVersionProvider {
    /// Version provider factory
    pub fn new(era_checker: Arc<dyn ApiVersionDiscriminantSource>) -> Self {
        Self {
            alternate_file_discriminator: era_checker,
            open_api_versions: get_open_api_versions_mapping(),
        }
    }

    /// Compute the current api version
    pub fn compute_current_version(&self) -> StdResult<Version> {
        let discriminant = self.alternate_file_discriminator.get_discriminant();
        let open_api_spec_file_name_default = "openapi.yaml";
        let open_api_spec_file_name_era = &format!("openapi-{discriminant}.yaml");
        let open_api_version = self.open_api_versions.get(open_api_spec_file_name_era).unwrap_or(
            self.open_api_versions
                .get(open_api_spec_file_name_default)
                .ok_or_else(|| anyhow!("Missing default API version"))?,
        );

        Ok(open_api_version.clone())
    }

    /// Compute the current api version requirement
    pub fn compute_current_version_requirement(&self) -> StdResult<VersionReq> {
        let version = &self.compute_current_version()?;
        let version_req = if version.major > 0 {
            format!("={}", version.major)
        } else {
            format!("={}.{}", version.major, version.minor)
        };

        Ok(VersionReq::parse(&version_req)?)
    }

    /// Compute all the sorted list of all versions
    pub fn compute_all_versions_sorted() -> Vec<Version> {
        let mut versions: Vec<Version> = get_open_api_versions_mapping().into_values().collect();
        versions.sort();
        versions
    }

    /// Update open api versions. Test only
    pub fn update_open_api_versions(
        &mut self,
        open_api_versions: HashMap<OpenAPIFileName, Version>,
    ) {
        self.open_api_versions = open_api_versions;
    }
}

#[cfg(test)]
mod test {
    use crate::test::double::DummyApiVersionDiscriminantSource;

    use super::*;

    #[test]
    fn test_compute_current_version_default() {
        let discriminant_source = DummyApiVersionDiscriminantSource::default();
        let mut version_provider = APIVersionProvider::new(Arc::new(discriminant_source));
        let mut open_api_versions = HashMap::new();
        open_api_versions.insert("openapi.yaml".to_string(), Version::new(1, 2, 3));
        version_provider.update_open_api_versions(open_api_versions);
        let api_version_provider = Arc::new(version_provider);

        assert_eq!(
            "1.2.3".to_string(),
            api_version_provider.compute_current_version().unwrap().to_string()
        )
    }

    #[test]
    fn test_compute_current_version_era_specific() {
        let discriminant_source = DummyApiVersionDiscriminantSource::new("dummy");
        let mut version_provider = APIVersionProvider::new(Arc::new(discriminant_source));
        let mut open_api_versions = HashMap::new();
        open_api_versions.insert("openapi.yaml".to_string(), Version::new(1, 2, 3));
        open_api_versions.insert("openapi-dummy.yaml".to_string(), Version::new(2, 1, 0));
        version_provider.update_open_api_versions(open_api_versions);
        let api_version_provider = Arc::new(version_provider);

        assert_eq!(
            "2.1.0".to_string(),
            api_version_provider.compute_current_version().unwrap().to_string()
        )
    }

    #[test]
    fn test_compute_current_version_requirement_beta() {
        let discriminant_source = DummyApiVersionDiscriminantSource::default();
        let mut version_provider = APIVersionProvider::new(Arc::new(discriminant_source));
        let mut open_api_versions = HashMap::new();
        open_api_versions.insert("openapi.yaml".to_string(), Version::new(0, 2, 3));
        version_provider.update_open_api_versions(open_api_versions);
        let api_version_provider = Arc::new(version_provider);

        assert_eq!(
            "=0.2".to_string(),
            api_version_provider
                .compute_current_version_requirement()
                .unwrap()
                .to_string()
        )
    }

    #[test]
    fn test_compute_current_version_requirement_stable() {
        let discriminant_source = DummyApiVersionDiscriminantSource::default();
        let mut version_provider = APIVersionProvider::new(Arc::new(discriminant_source));
        let mut open_api_versions = HashMap::new();
        open_api_versions.insert("openapi.yaml".to_string(), Version::new(3, 2, 1));
        version_provider.update_open_api_versions(open_api_versions);
        let api_version_provider = Arc::new(version_provider);

        assert_eq!(
            "=3".to_string(),
            api_version_provider
                .compute_current_version_requirement()
                .unwrap()
                .to_string()
        )
    }

    #[test]
    fn test_compute_all_versions_sorted() {
        let all_versions_sorted = APIVersionProvider::compute_all_versions_sorted();

        assert!(!all_versions_sorted.is_empty());
    }
}
