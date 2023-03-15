//! API Version provider service
include!(concat!(env!("OUT_DIR"), "/open_api.rs"));
use semver::{Error as SemVerError, Version, VersionReq};
use serde_yaml::Error as SerdeYamlError;
use std::collections::HashMap;
use std::sync::Arc;
use thiserror::Error;

use crate::era::EraChecker;

/// API Version provider error
#[derive(Debug, Error)]
pub enum APIVersionProviderError {
    /// Semver parse error
    #[error("Semver parse error: {0:?}")]
    SemverParse(#[from] SemVerError),

    /// Yaml parse error
    #[error("Spec file parse error: {0:?}")]
    SpecParse(#[from] SerdeYamlError),

    /// Spec file io error
    #[error("Spec file IO error: {0}")]
    SpecFileIO(String),

    /// Missing default api version
    #[error("Missing default API version")]
    MissingDefault(),
}

/// API Version provider
#[derive(Clone)]
pub struct APIVersionProvider {
    era_checker: Arc<EraChecker>,
    open_api_versions: HashMap<OpenAPIFileName, OpenAPIVersionRaw>,
}

impl APIVersionProvider {
    /// Version provider factory
    pub fn new(era_checker: Arc<EraChecker>) -> Self {
        Self {
            era_checker,
            open_api_versions: get_open_api_versions_mapping(),
        }
    }

    /// Compute the current api version
    pub fn compute_current_version(&self) -> Result<Version, APIVersionProviderError> {
        let current_era = self.era_checker.current_era();
        let open_api_spec_file_name_default = "openapi.yaml";
        let open_api_spec_file_name_era = &format!("openapi-{current_era}.yaml");
        let open_api_version_raw = self
            .open_api_versions
            .get(open_api_spec_file_name_era)
            .unwrap_or(
                self.open_api_versions
                    .get(open_api_spec_file_name_default)
                    .ok_or_else(APIVersionProviderError::MissingDefault)?,
            );

        Version::parse(open_api_version_raw).map_err(APIVersionProviderError::SemverParse)
    }

    /// Compute the current api version requirement
    pub fn compute_current_version_requirement(
        &self,
    ) -> Result<VersionReq, APIVersionProviderError> {
        let version = &self.compute_current_version()?;
        let version_req = if version.major > 0 {
            format!("={}", version.major)
        } else {
            format!("={}.{}", version.major, version.minor)
        };

        Ok(VersionReq::parse(&version_req)?)
    }

    /// Update open api versions. Test only
    #[cfg(any(test, feature = "test_only"))]
    pub fn update_open_api_versions(
        &mut self,
        open_api_versions: HashMap<OpenAPIFileName, OpenAPIVersionRaw>,
    ) {
        self.open_api_versions = open_api_versions;
    }
}

#[cfg(test)]
mod test {
    use std::{collections::HashMap, sync::Arc};

    use crate::{
        api::version::APIVersionProvider,
        entities::Epoch,
        era::{EraChecker, SupportedEra},
    };

    #[test]
    fn test_compute_current_version_default() {
        let era_checker = EraChecker::new(SupportedEra::dummy(), Epoch(1));
        let mut version_provider = APIVersionProvider::new(Arc::new(era_checker));
        let mut open_api_versions = HashMap::new();
        open_api_versions.insert("openapi.yaml".to_string(), "1.2.3".to_string());
        version_provider.update_open_api_versions(open_api_versions);
        let api_version_provider = Arc::new(version_provider);

        assert_eq!(
            "1.2.3".to_string(),
            api_version_provider
                .compute_current_version()
                .unwrap()
                .to_string()
        )
    }

    #[test]
    fn test_compute_current_version_era_specific() {
        let era_checker = EraChecker::new(SupportedEra::dummy(), Epoch(1));
        let mut version_provider = APIVersionProvider::new(Arc::new(era_checker));
        let mut open_api_versions = HashMap::new();
        open_api_versions.insert("openapi.yaml".to_string(), "1.2.3".to_string());
        open_api_versions.insert(
            format!("openapi-{}.yaml", SupportedEra::dummy()),
            "2.1.0".to_string(),
        );
        version_provider.update_open_api_versions(open_api_versions);
        let api_version_provider = Arc::new(version_provider);

        assert_eq!(
            "2.1.0".to_string(),
            api_version_provider
                .compute_current_version()
                .unwrap()
                .to_string()
        )
    }

    #[test]
    fn test_compute_current_version_requirement_beta() {
        let era_checker = EraChecker::new(SupportedEra::dummy(), Epoch(1));
        let mut version_provider = APIVersionProvider::new(Arc::new(era_checker));
        let mut open_api_versions = HashMap::new();
        open_api_versions.insert("openapi.yaml".to_string(), "0.2.3".to_string());
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
        let era_checker = EraChecker::new(SupportedEra::dummy(), Epoch(1));
        let mut version_provider = APIVersionProvider::new(Arc::new(era_checker));
        let mut open_api_versions = HashMap::new();
        open_api_versions.insert("openapi.yaml".to_string(), "3.2.1".to_string());
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
}
