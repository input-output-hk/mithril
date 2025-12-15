use std::collections::HashMap;
use std::sync::Arc;
use std::time::Duration;

use anyhow::Context;
use mithril_common::StdResult;
use mithril_common::api_version::APIVersionProvider;
use reqwest::{Client, IntoUrl, Url};
use slog::{Logger, o};

use crate::client::AggregatorHttpClient;

/// A builder of [AggregatorHttpClient]
pub struct AggregatorClientBuilder {
    aggregator_url_result: reqwest::Result<Url>,
    api_version_provider: Option<Arc<APIVersionProvider>>,
    additional_headers: Option<HashMap<String, String>>,
    timeout_duration: Option<Duration>,
    #[cfg(not(target_family = "wasm"))]
    relay_endpoint: Option<String>,
    logger: Option<Logger>,
}

impl AggregatorClientBuilder {
    /// Constructs a new `AggregatorClientBuilder`.
    //
    // This is the same as `AggregatorClient::builder()`.
    pub fn new<U: IntoUrl>(aggregator_url: U) -> Self {
        Self {
            aggregator_url_result: aggregator_url.into_url(),
            api_version_provider: None,
            additional_headers: None,
            timeout_duration: None,
            #[cfg(not(target_family = "wasm"))]
            relay_endpoint: None,
            logger: None,
        }
    }

    /// Set the [Logger] to use.
    pub fn with_logger(mut self, logger: Logger) -> Self {
        self.logger = Some(logger);
        self
    }

    /// Set the [APIVersionProvider] to use.
    pub fn with_api_version_provider(
        mut self,
        api_version_provider: Arc<APIVersionProvider>,
    ) -> Self {
        self.api_version_provider = Some(api_version_provider);
        self
    }

    /// Set a timeout to enforce on each request
    pub fn with_timeout(mut self, timeout: Duration) -> Self {
        self.timeout_duration = Some(timeout);
        self
    }

    /// Add a set of http headers that will be sent on client requests
    pub fn with_headers(mut self, custom_headers: HashMap<String, String>) -> Self {
        self.additional_headers = Some(custom_headers);
        self
    }

    /// Set the address of the relay
    ///
    /// _Not available on wasm platforms_
    #[cfg(not(target_family = "wasm"))]
    pub fn with_relay_endpoint(mut self, relay_endpoint: Option<String>) -> Self {
        self.relay_endpoint = relay_endpoint;
        self
    }

    /// Returns an [AggregatorHttpClient] based on the builder configuration
    pub fn build(self) -> StdResult<AggregatorHttpClient> {
        let aggregator_endpoint =
            enforce_trailing_slash(self.aggregator_url_result.with_context(
                || "Invalid aggregator endpoint, it must be a correctly formed url",
            )?);
        let logger = self.logger.unwrap_or_else(|| Logger::root(slog::Discard, o!()));
        let api_version_provider = self.api_version_provider.unwrap_or_default();
        let additional_headers = self.additional_headers.unwrap_or_default();
        #[cfg(not(target_family = "wasm"))]
        let mut client_builder = Client::builder();
        #[cfg(target_family = "wasm")]
        let client_builder = Client::builder();

        #[cfg(not(target_family = "wasm"))]
        if let Some(relay_endpoint) = self.relay_endpoint {
            use reqwest::Proxy;

            client_builder = client_builder
                .proxy(Proxy::all(relay_endpoint).with_context(|| "Relay proxy creation failed")?)
        }

        Ok(AggregatorHttpClient {
            aggregator_endpoint,
            api_version_provider,
            additional_headers: (&additional_headers)
                .try_into()
                .with_context(|| format!("Invalid headers: '{additional_headers:?}'"))?,
            timeout_duration: self.timeout_duration,
            client: client_builder
                .build()
                .with_context(|| "HTTP client creation failed")?,
            logger,
        })
    }
}

fn enforce_trailing_slash(url: Url) -> Url {
    // Trailing slash is significant because url::join
    // (https://docs.rs/url/latest/url/struct.Url.html#method.join) will remove
    // the 'path' part of the url if it doesn't end with a trailing slash.
    if url.as_str().ends_with('/') {
        url
    } else {
        let mut url = url.clone();
        url.set_path(&format!("{}/", url.path()));
        url
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn enforce_trailing_slash_for_aggregator_url() {
        let url_without_trailing_slash = Url::parse("http://localhost:8080").unwrap();
        let url_with_trailing_slash = Url::parse("http://localhost:8080/").unwrap();

        assert_eq!(
            url_with_trailing_slash,
            enforce_trailing_slash(url_without_trailing_slash.clone())
        );
        assert_eq!(
            url_with_trailing_slash,
            enforce_trailing_slash(url_with_trailing_slash.clone())
        );
    }
}
