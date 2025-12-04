use std::{collections::HashMap, time::Duration};

use anyhow::Context;
use reqwest::Client;
use serde::{Deserialize, Serialize};

use mithril_common::{StdResult, entities::MithrilNetwork};

use crate::{AggregatorDiscoverer, AggregatorEndpoint};

const DEFAULT_REMOTE_NETWORKS_CONFIG_URL: &str =
    "https://raw.githubusercontent.com/input-output-hk/mithril/main/networks.json";

/// Representation of the networks configuration file.
#[derive(Debug, Clone, Serialize, Deserialize)]
struct NetworksConfigMessage {
    #[serde(flatten)]
    pub networks: HashMap<String, NetworkEnvironmentMessage>,
}

/// Representation of a network environment in the networks configuration file.
#[derive(Debug, Clone, Serialize, Deserialize)]
struct NetworkEnvironmentMessage {
    #[serde(rename = "mithril-networks")]
    pub mithril_networks: Vec<HashMap<String, MithrilNetworkMessage>>,
}

/// Representation of a Mithril network in the networks configuration file.
#[derive(Debug, Clone, Serialize, Deserialize)]
struct MithrilNetworkMessage {
    pub aggregators: Vec<AggregatorMessage>,
}

/// Representation of an aggregator in the networks configuration file.
#[derive(Debug, Clone, Serialize, Deserialize)]
struct AggregatorMessage {
    pub url: String,
}

/// An implementation of the [AggregatorDiscoverer] trait which discovers aggregators from remote networks configuration.
///
/// The reference file is the `networks.json` file hosted in the Mithril GitHub repository.
pub struct HttpConfigAggregatorDiscoverer {
    configuration_file_url: String,
}

impl HttpConfigAggregatorDiscoverer {
    const HTTP_TIMEOUT: Duration = Duration::from_secs(10);

    /// Creates a new `HttpConfigAggregatorDiscoverer` instance with the configuration file URL.
    pub fn new(configuration_file_url: &str) -> Self {
        Self {
            configuration_file_url: configuration_file_url.to_string(),
        }
    }

    /// Builds a reqwest HTTP client.
    fn build_client(&self) -> StdResult<Client> {
        Ok(Client::builder().timeout(Self::HTTP_TIMEOUT).build()?)
    }
}

impl Default for HttpConfigAggregatorDiscoverer {
    fn default() -> Self {
        Self::new(DEFAULT_REMOTE_NETWORKS_CONFIG_URL)
    }
}

#[async_trait::async_trait]
impl AggregatorDiscoverer<AggregatorEndpoint> for HttpConfigAggregatorDiscoverer {
    async fn get_available_aggregators(
        &self,
        network: MithrilNetwork,
    ) -> StdResult<Box<dyn Iterator<Item = AggregatorEndpoint>>> {
        let client = self.build_client()?;
        let networks_configuration_response: NetworksConfigMessage = client
            .get(&self.configuration_file_url)
            .send()
            .await
            .with_context(|| {
                format!(
                    "AggregatorDiscovererHttpConfig failed retrieving configuration file from {}",
                    &self.configuration_file_url
                )
            })?
            .json::<NetworksConfigMessage>()
            .await
            .with_context(|| {
                format!(
                    "AggregatorDiscovererHttpConfig failed parsing configuration file from {}",
                    &self.configuration_file_url
                )
            })?;
        let aggregator_endpoints = networks_configuration_response
            .networks
            .values()
            .flat_map(|env| &env.mithril_networks)
            .flat_map(|network_map| network_map.iter())
            .filter(|(name, _)| *name == network.name())
            .flat_map(|(_, network)| &network.aggregators)
            .map(|aggregator_msg| AggregatorEndpoint::new(aggregator_msg.url.clone()))
            .collect::<Vec<_>>();

        Ok(Box::new(aggregator_endpoints.into_iter()))
    }
}

#[cfg(test)]
mod tests {
    use httpmock::MockServer;

    use super::*;

    const TEST_NETWORKS_CONFIG_JSON_SUCCESS: &str = r#"
    {
        "devnet": {
            "mithril-networks": [
                {
                    "release-devnet": {
                        "aggregators": [
                            { "url": "https://release-devnet-aggregator1" },
                            { "url": "https://release-devnet-aggregator2" }
                        ]
                    }
                }
            ]
        },
        "testnet": {
            "mithril-networks": [
                {
                    "preview-testnet": {
                        "aggregators": [
                            { "url": "https://preview-testnet-aggregator1" },
                            { "url": "https://preview-testnet-aggregator2" }
                        ]
                    }
                }
            ]
        }
    }"#;

    const TEST_NETWORKS_CONFIG_JSON_FAILURE: &str = r#"
    {
        {"}
    }"#;

    fn create_server_and_discoverer(content: &str) -> (MockServer, HttpConfigAggregatorDiscoverer) {
        let size = content.len() as u64;
        let server = MockServer::start();
        server.mock(|when, then| {
            when.method(httpmock::Method::GET).path("/networks.json");
            then.status(200)
                .body(content)
                .header(reqwest::header::CONTENT_LENGTH.as_str(), size.to_string());
        });
        let configuration_file_url = format!("{}{}", server.url("/"), "networks.json");
        let discoverer = HttpConfigAggregatorDiscoverer::new(&configuration_file_url);

        (server, discoverer)
    }

    #[tokio::test]
    async fn get_available_aggregators_success() {
        let content = TEST_NETWORKS_CONFIG_JSON_SUCCESS;
        let (_server, discoverer) = create_server_and_discoverer(content);
        let aggregators = discoverer
            .get_available_aggregators(MithrilNetwork::new("release-devnet".into()))
            .await
            .unwrap();

        assert_eq!(
            vec![
                AggregatorEndpoint::new("https://release-devnet-aggregator1".into()),
                AggregatorEndpoint::new("https://release-devnet-aggregator2".into()),
            ],
            aggregators.collect::<Vec<_>>()
        );

        let mut aggregators = discoverer
            .get_available_aggregators(MithrilNetwork::new("unknown".into()))
            .await
            .unwrap();

        assert!(aggregators.next().is_none());
    }

    #[tokio::test]
    async fn get_available_aggregators_failure() {
        let content = TEST_NETWORKS_CONFIG_JSON_FAILURE;
        let (_server, discoverer) = create_server_and_discoverer(content);
        let result = discoverer
            .get_available_aggregators(MithrilNetwork::new("release-devnet".into()))
            .await;
        assert!(
            result.is_err(),
            "The retrieval of the aggregators should fail"
        );
    }
}
