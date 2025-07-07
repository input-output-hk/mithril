//! A client to retrieve the current Mithril era.
//!
//! In order to do so it defines a [MithrilEraClient] which exposes the following features:
//! - [fetch_current][MithrilEraClient::fetch_current]: fetch the current Mithril era using its [EraFetcher]
//!
//! This module defines the following components:
//!
//! - [EraFetcher]: defines an interface to retrieve the current Mithril era
//! - [AggregatorHttpEraFetcher]: an implementation of [EraFetcher] using an HTTP call to the aggregator
//! - [FetchedEra]: a wrapper around a raw era string that provides a conversion to [SupportedEra]
//!
//! # Retrieve the current Mithril era
//!
//! To get [SupportedEra] using the [ClientBuilder][crate::client::ClientBuilder].
//!
//! ```no_run
//! # async fn run() -> mithril_client::MithrilResult<()> {
//! use mithril_client::ClientBuilder;
//!
//! let client = ClientBuilder::aggregator("YOUR_AGGREGATOR_ENDPOINT", "YOUR_GENESIS_VERIFICATION_KEY").build()?;
//!
//! let fetched_era = client.mithril_era_client().fetch_current().await?;
//! match fetched_era.to_supported_era() {
//!     Ok(supported_era) => println!("Current Mithril era: {supported_era}"),
//!     Err(era) => println!(
//!         "Warning: Unsupported era '{}', the aggregator might not be compatible with this client version. Consider upgrading.\nFetched era: {era}",
//!         fetched_era.era
//!     ),
//! }
//! # Ok(())
//! # }
//! ```

use std::sync::Arc;

use anyhow::Context;
use async_trait::async_trait;
use serde::{Deserialize, Serialize};

use mithril_common::entities::SupportedEra;

use crate::{
    MithrilResult,
    aggregator_client::{AggregatorClient, AggregatorRequest},
};

/// Client for retrieving the current Mithril era.
pub struct MithrilEraClient {
    era_fetcher: Arc<dyn EraFetcher>,
}

impl MithrilEraClient {
    /// Constructs a new [MithrilEraClient].
    pub fn new(era_fetcher: Arc<dyn EraFetcher>) -> Self {
        Self { era_fetcher }
    }

    /// Fetches the current Mithril era.
    pub async fn fetch_current(&self) -> MithrilResult<FetchedEra> {
        self.era_fetcher.fetch_current_era().await
    }
}

/// Wrapper around a raw Mithril era string.
#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct FetchedEra {
    /// Mithril era.
    pub era: String,
}

impl FetchedEra {
    /// Attempts to convert the internal Mithril era string to a [SupportedEra] enum variant.
    pub fn to_supported_era(&self) -> MithrilResult<SupportedEra> {
        self.era
            .parse::<SupportedEra>()
            .with_context(|| format!("Unknown supported era: {}", self.era))
    }
}

/// Trait for retrieving the current Mithril era.
#[cfg_attr(target_family = "wasm", async_trait(?Send))]
#[cfg_attr(not(target_family = "wasm"), async_trait)]
pub trait EraFetcher: Send + Sync {
    /// Fetch the current Mithril era.
    async fn fetch_current_era(&self) -> MithrilResult<FetchedEra>;
}

/// An implementation of [EraFetcher] that retrieves the current Mithril era
/// by performing an HTTP request to the aggregator's `/status` endpoint.
pub struct AggregatorHttpEraFetcher {
    aggregator_client: Arc<dyn AggregatorClient>,
}

impl AggregatorHttpEraFetcher {
    /// Constructs a new [AggregatorHttpEraFetcher].
    pub fn new(aggregator_client: Arc<dyn AggregatorClient>) -> Self {
        Self { aggregator_client }
    }
}

#[cfg_attr(target_family = "wasm", async_trait(?Send))]
#[cfg_attr(not(target_family = "wasm"), async_trait)]
impl EraFetcher for AggregatorHttpEraFetcher {
    async fn fetch_current_era(&self) -> MithrilResult<FetchedEra> {
        #[derive(serde::Deserialize)]
        struct Era {
            mithril_era: String,
        }

        let response = self
            .aggregator_client
            .get_content(AggregatorRequest::Status)
            .await
            .with_context(|| "Failed to fetch the aggregator status route")?;

        let era = serde_json::from_str::<Era>(&response)
            .with_context(|| "Failed to parse aggregator status message as JSON value")?
            .mithril_era;

        Ok(FetchedEra { era })
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::{
        entities::{Epoch, ProtocolParameters, SupportedEra},
        messages::AggregatorStatusMessage,
    };
    use mockall::predicate::eq;

    use crate::aggregator_client::MockAggregatorClient;

    use super::*;

    fn dummy_aggregator_status_message() -> AggregatorStatusMessage {
        AggregatorStatusMessage {
            epoch: Epoch(48),
            cardano_era: "conway".to_string(),
            cardano_network: "mainnet".to_string(),
            mithril_era: SupportedEra::Pythagoras,
            cardano_node_version: "1.2.3".to_string(),
            aggregator_node_version: "4.5.6".to_string(),
            protocol_parameters: ProtocolParameters {
                k: 5,
                m: 100,
                phi_f: 0.65,
            },
            next_protocol_parameters: ProtocolParameters {
                k: 50,
                m: 1000,
                phi_f: 0.65,
            },
            total_signers: 1234,
            total_next_signers: 56789,
            total_stakes_signers: 123456789,
            total_next_stakes_signers: 987654321,
            total_cardano_spo: 7777,
            total_cardano_stake: 888888888,
        }
    }

    #[tokio::test]
    async fn fetch_current_era_should_return_mithril_era_from_aggregator_status() {
        let aggregator_status_message = AggregatorStatusMessage {
            mithril_era: SupportedEra::Pythagoras,
            ..dummy_aggregator_status_message()
        };
        let mock_aggregator_client = {
            let mut mock_client = MockAggregatorClient::new();
            mock_client
                .expect_get_content()
                .with(eq(AggregatorRequest::Status))
                .return_once(move |_| {
                    Ok(serde_json::to_string(&aggregator_status_message).unwrap())
                });

            Arc::new(mock_client)
        };
        let era_fetcher = AggregatorHttpEraFetcher::new(mock_aggregator_client);

        let fetched_era = era_fetcher.fetch_current_era().await.unwrap();

        assert_eq!(
            {
                FetchedEra {
                    era: SupportedEra::Pythagoras.to_string(),
                }
            },
            fetched_era
        );
    }

    #[tokio::test]
    async fn fetch_current_era_returns_error_if_response_is_invalid_json() {
        let mock_aggregator_client = {
            let mut mock_client = MockAggregatorClient::new();
            mock_client
                .expect_get_content()
                .with(eq(AggregatorRequest::Status))
                .return_once(|_| Ok("invalid_json".to_string()));

            Arc::new(mock_client)
        };
        let era_fetcher = AggregatorHttpEraFetcher::new(mock_aggregator_client);

        era_fetcher
            .fetch_current_era()
            .await
            .expect_err("Expected an error due to invalid JSON response");
    }

    #[tokio::test]
    async fn fetch_current_era_returns_error_if_mithril_era_field_is_missing() {
        let mut response_json = serde_json::json!(dummy_aggregator_status_message());
        response_json.as_object_mut().unwrap().remove("mithril_era");
        let mock_aggregator_client = {
            let mut mock_client = MockAggregatorClient::new();
            mock_client
                .expect_get_content()
                .with(eq(AggregatorRequest::Status))
                .return_once(move |_| Ok(response_json.to_string()));

            Arc::new(mock_client)
        };
        let era_fetcher = AggregatorHttpEraFetcher::new(mock_aggregator_client);

        era_fetcher.fetch_current_era().await.expect_err(
            "Expected an error due to missing 'mithril_era' field in aggregator status",
        );
    }

    #[test]
    fn to_supported_era_should_return_enum_variant_for_known_era() {
        let fetched_era = FetchedEra {
            era: SupportedEra::Pythagoras.to_string(),
        };

        let supported_era = fetched_era.to_supported_era().unwrap();

        assert_eq!(SupportedEra::Pythagoras, supported_era);
    }

    #[test]
    fn to_supported_era_returns_error_for_unsupported_era() {
        let fetched_era = FetchedEra {
            era: "unsupported_era".to_string(),
        };

        fetched_era
            .to_supported_era()
            .expect_err("Expected an error when converting an unsupported era to 'SupportedEra'");
    }
}
