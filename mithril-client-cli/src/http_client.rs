use anyhow::anyhow;
use async_recursion::async_recursion;
use async_trait::async_trait;
use reqwest::{Client, Response, StatusCode};
use semver::Version;
use slog_scope::debug;
use std::sync::Arc;
use thiserror::Error;
use tokio::sync::RwLock;

#[cfg(test)]
use mockall::automock;

use mithril_common::{StdError, MITHRIL_API_VERSION_HEADER};

/// Error tied with the Aggregator client
#[derive(Error, Debug)]
pub enum AggregatorHTTPClientError {
    /// Error raised when querying the aggregator returned a 5XX error.
    #[error("remote server technical error")]
    RemoteServerTechnical(#[source] StdError),

    /// Error raised when querying the aggregator returned a 4XX error.
    #[error("remote server logical error")]
    RemoteServerLogical(#[source] StdError),

    /// Error raised when the server API version mismatch the client API version.
    #[error("API version mismatch")]
    ApiVersionMismatch(#[source] StdError),

    /// HTTP subsystem error
    #[error("HTTP subsystem error")]
    SubsystemError(#[source] StdError),
}

/// API that defines a client for the Aggregator
#[async_trait]
pub trait AggregatorClient: Sync + Send {
    /// Post information to the Aggregator, the URL is a relative path for a resource
    async fn post_content(
        &self,
        url: &str,
        json: &str,
    ) -> Result<String, AggregatorHTTPClientError>;
}

/// Responsible of HTTP transport and API version check.
pub struct AggregatorHTTPClient {
    aggregator_endpoint: String,
    api_versions: Arc<RwLock<Vec<Version>>>,
}

impl AggregatorHTTPClient {
    /// AggregatorHTTPClient factory
    pub fn new(aggregator_endpoint: &str, api_versions: Vec<Version>) -> Self {
        debug!("New AggregatorHTTPClient created");
        Self {
            aggregator_endpoint: aggregator_endpoint.to_owned(),
            api_versions: Arc::new(RwLock::new(api_versions)),
        }
    }

    /// Computes the current api version
    async fn compute_current_api_version(&self) -> Option<Version> {
        self.api_versions.read().await.first().cloned()
    }

    /// Discards the current api version
    /// It discards the current version if and only if there is at least 2 versions available
    async fn discard_current_api_version(&self) -> Option<Version> {
        if self.api_versions.read().await.len() < 2 {
            return None;
        }
        if let Some(current_api_version) = self.compute_current_api_version().await {
            let mut api_versions = self.api_versions.write().await;
            if let Some(index) = api_versions
                .iter()
                .position(|value| *value == current_api_version)
            {
                api_versions.remove(index);
                return Some(current_api_version);
            }
        }
        None
    }

    /// Issue a POST HTTP request.
    #[async_recursion]
    async fn post(&self, url: &str, json: &str) -> Result<Response, AggregatorHTTPClientError> {
        debug!("POST url='{url}' json='{json}'.");
        let request_builder = Client::new().post(url.to_owned()).body(json.to_owned());
        let current_api_version = self
            .compute_current_api_version()
            .await
            .unwrap()
            .to_string();
        debug!("Prepare request with version: {current_api_version}");
        let request_builder =
            request_builder.header(MITHRIL_API_VERSION_HEADER, current_api_version);

        let response = request_builder.send().await.map_err(|e| {
            AggregatorHTTPClientError::SubsystemError(
                anyhow!(e).context(format!("Error while POSTing data '{json}' to URL='{url}'.")),
            )
        })?;

        match response.status() {
            StatusCode::OK | StatusCode::CREATED => Ok(response),
            StatusCode::PRECONDITION_FAILED => {
                if self.discard_current_api_version().await.is_some()
                    && !self.api_versions.read().await.is_empty()
                {
                    return self.post(url, json).await;
                }

                Err(self.handle_api_error(&response).await)
            }
            StatusCode::NOT_FOUND => Err(AggregatorHTTPClientError::RemoteServerLogical(anyhow!(
                "Url='{url} not found"
            ))),
            status_code => Err(AggregatorHTTPClientError::RemoteServerTechnical(anyhow!(
                "Unhandled error {status_code}"
            ))),
        }
    }

    /// API version error handling
    async fn handle_api_error(&self, response: &Response) -> AggregatorHTTPClientError {
        if let Some(version) = response.headers().get(MITHRIL_API_VERSION_HEADER) {
            AggregatorHTTPClientError::ApiVersionMismatch(anyhow!(
                "server version: '{}', signer version: '{}'",
                version.to_str().unwrap(),
                self.compute_current_api_version().await.unwrap()
            ))
        } else {
            AggregatorHTTPClientError::ApiVersionMismatch(anyhow!(
                "version precondition failed, sent version '{}'.",
                self.compute_current_api_version().await.unwrap()
            ))
        }
    }
}

#[cfg_attr(test, automock)]
#[async_trait]
impl AggregatorClient for AggregatorHTTPClient {
    async fn post_content(
        &self,
        url: &str,
        json: &str,
    ) -> Result<String, AggregatorHTTPClientError> {
        let url = format!("{}/{}", self.aggregator_endpoint.trim_end_matches('/'), url);
        let response = self.post(&url, json).await?;

        response.text().await.map_err(|e| {
            AggregatorHTTPClientError::SubsystemError(
                anyhow!(e).context("Could not find a text body in the response."),
            )
        })
    }
}
