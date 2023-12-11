use serde::Deserialize;
use slog::Logger;
use std::{collections::HashMap, sync::Arc};
use thiserror::Error;

use mithril_common::{api_version::APIVersionProvider, StdResult};

use crate::http_client::{AggregatorClient, AggregatorHTTPClient};

/// Configuration error
#[derive(Debug, Error)]
pub enum ConfigError {
    /// Error raised when a required parameter is not present.
    #[error("Parameter '{0}' is mandatory.")]
    Required(String),
}

/// Configuration parameters holder
#[derive(Debug, Default, PartialEq, Deserialize)]
#[serde(default)]
pub struct ConfigParameters {
    parameters: HashMap<String, String>,
}

impl ConfigParameters {
    /// Constructor
    pub fn new(parameters: HashMap<String, String>) -> Self {
        Self { parameters }
    }

    /// Useful constructor for testing
    pub fn build(parameters: &[(&str, &str)]) -> Self {
        let parameters = parameters
            .iter()
            .map(|(k, v)| (k.to_string(), v.to_string()))
            .collect();

        Self::new(parameters)
    }

    /// Add or replace a parameter in the holder
    pub fn add_parameter(&mut self, name: &str, value: &str) -> &mut Self {
        let _ = self.parameters.insert(name.to_string(), value.to_string());

        self
    }

    /// Fetch a parameter from the holder.
    pub fn get(&self, name: &str) -> Option<String> {
        self.parameters.get(name).cloned()
    }

    /// Fetch a parameter from the holder. If the parameter is not set, the
    /// given default value is returned instead.
    pub fn get_or(&self, name: &str, default: &str) -> String {
        self.get(name).unwrap_or(default.to_string())
    }

    /// Fetch a parameter from the holder. If the parameter is not set, an error
    /// is raised.
    pub fn require(&self, name: &str) -> Result<String, ConfigError> {
        self.get(name)
            .ok_or_else(|| ConfigError::Required(name.to_string()))
    }
}

/// Dependencies builder
pub struct DependenciesBuilder {
    /// Configuration
    pub config: Arc<ConfigParameters>,

    /// HTTP Aggregator client
    pub aggregator_client: Option<Arc<dyn AggregatorClient>>,
}

impl DependenciesBuilder {
    /// Constructor
    pub fn new(config: Arc<ConfigParameters>) -> Self {
        Self {
            config,
            aggregator_client: None,
        }
    }

    async fn build_logger(&mut self) -> StdResult<Logger> {
        Ok(slog_scope::logger())
    }

    /// Return an instance of the logger. Since the logger is a singleton it is
    /// provider directly by its own library.
    pub async fn get_logger(&mut self) -> StdResult<Logger> {
        self.build_logger().await
    }

    async fn build_aggregator_client(&mut self) -> StdResult<Arc<dyn AggregatorClient>> {
        let client = AggregatorHTTPClient::new(
            &self.config.require("aggregator_endpoint")?,
            APIVersionProvider::compute_all_versions_sorted()?,
        );

        Ok(Arc::new(client))
    }

    /// Get a clone of the [AggregatorClient] dependency
    pub async fn get_aggregator_client(&mut self) -> StdResult<Arc<dyn AggregatorClient>> {
        if self.aggregator_client.is_none() {
            self.aggregator_client = Some(self.build_aggregator_client().await?);
        }

        Ok(self.aggregator_client.as_ref().cloned().unwrap())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_config_constructor() {
        let config = ConfigParameters::build(&[("pika", "chu")]);

        assert_eq!(
            ConfigParameters {
                parameters: [("pika".to_string(), "chu".to_string())]
                    .into_iter()
                    .collect()
            },
            config
        );
    }
    #[test]
    fn test_config_set() {
        let mut config = ConfigParameters::default();
        config.add_parameter("pika", "chu");

        assert_eq!(
            ConfigParameters {
                parameters: [("pika".to_string(), "chu".to_string())]
                    .into_iter()
                    .collect()
            },
            config
        );
    }

    #[test]
    fn test_config_get() {
        let mut config = ConfigParameters::default();
        config.add_parameter("pika", "chu");

        assert_eq!("chu".to_string(), config.get("pika").unwrap());
        assert!(config.get("whatever").is_none());
    }

    #[test]
    fn test_config_default() {
        let mut config = ConfigParameters::default();
        config.add_parameter("pika", "chu");

        assert_eq!("chu".to_string(), config.get("pika").unwrap());
        assert_eq!("default".to_string(), config.get_or("whatever", "default"));
    }

    #[test]
    fn test_config_require() {
        let mut config = ConfigParameters::default();
        config.add_parameter("pika", "chu");

        assert_eq!("chu".to_string(), config.require("pika").unwrap());
        config.require("whatever").unwrap_err();
    }
}
