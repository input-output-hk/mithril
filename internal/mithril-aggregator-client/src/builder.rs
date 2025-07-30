use anyhow::Context;
use reqwest::{IntoUrl, Url};
use slog::{Logger, o};

use mithril_common::StdResult;
use mithril_common::api_version::APIVersionProvider;

use crate::client::AggregatorClient;

/// A builder of [AggregatorClient]
pub struct AggregatorClientBuilder {
    aggregator_url_result: reqwest::Result<Url>,
    api_version_provider: Option<APIVersionProvider>,
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
            logger: None,
        }
    }

    /// Set the [Logger] to use.
    pub fn with_logger(mut self, logger: Logger) -> Self {
        self.logger = Some(logger);
        self
    }

    /// Set the [APIVersionProvider] to use.
    pub fn with_api_version_provider(mut self, api_version_provider: APIVersionProvider) -> Self {
        self.api_version_provider = Some(api_version_provider);
        self
    }

    /// Returns an [AggregatorClient] based on the builder configuration
    pub fn build(self) -> StdResult<AggregatorClient> {
        let aggregator_endpoint =
            enforce_trailing_slash(self.aggregator_url_result.with_context(
                || "Invalid aggregator endpoint, it must be a correctly formed url",
            )?);
        let logger = self.logger.unwrap_or_else(|| Logger::root(slog::Discard, o!()));
        let api_version_provider = self.api_version_provider.unwrap_or_default();

        Ok(AggregatorClient {
            aggregator_endpoint,
            api_version_provider,
            client: reqwest::Client::new(),
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
