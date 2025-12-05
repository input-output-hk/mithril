//! A client to retrieve the current Mithril era.
//!
//! In order to do so it defines a [MithrilEraClient] which exposes the following features:
//! - [fetch_current][MithrilEraClient::fetch_current]: fetch the current Mithril era using its [EraFetcher]
//!
//! This module defines the following components:
//!
//! - [EraFetcher]: defines an interface to retrieve the current Mithril era, an implementation over
//!   [mithril_aggregator_client::AggregatorHttpClient] is provided
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

use crate::MithrilResult;

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

#[cfg(test)]
mod tests {
    use mithril_common::entities::SupportedEra;

    use super::*;

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
