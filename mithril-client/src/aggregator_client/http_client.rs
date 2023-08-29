use anyhow::anyhow;
use async_recursion::async_recursion;
use async_trait::async_trait;
use futures::StreamExt;
use reqwest::{Client, Response, StatusCode};
use semver::Version;
use slog_scope::debug;
use std::{path::Path, sync::Arc};
use thiserror::Error;
use tokio::sync::RwLock;

#[cfg(test)]
use mockall::automock;

use mithril_common::{StdError, StdResult, MITHRIL_API_VERSION_HEADER};

use crate::utils::{DownloadProgressReporter, SnapshotUnpacker};

/// Error tied with the Aggregator client
#[derive(Error, Debug)]
pub enum AggregatorHTTPClientError {
    /// Error raised when querying the aggregator returned a 5XX error.
    #[error("remote server technical error: '{0}'")]
    RemoteServerTechnical(String),

    /// Error raised when querying the aggregator returned a 4XX error.
    #[error("remote server logical error: '{0}'")]
    RemoteServerLogical(String),

    /// Error raised when the aggregator can't be reached.
    #[error("remote server unreachable: '{0}'")]
    RemoteServerUnreachable(String),

    /// Error raised when the server API version mismatch the client API version.
    #[error("API version mismatch: {0}")]
    ApiVersionMismatch(String),

    /// HTTP subsystem error
    #[error("HTTP subsystem error: {message} ({error}).")]
    SubsystemError {
        /// Error context
        message: String,

        /// Nested error
        error: StdError,
    },
}

/// API that defines a client for the Aggregator
#[async_trait]
pub trait AggregatorClient: Sync + Send {
    /// Get the content back from the Aggregator, the URL is a relative path for a resource
    async fn get_content(&self, url: &str) -> Result<String, AggregatorHTTPClientError>;

    /// Download and unpack large archives on the disk
    async fn download_unpack(
        &self,
        url: &str,
        target_dir: &Path,
        progress_reporter: DownloadProgressReporter,
    ) -> Result<(), AggregatorHTTPClientError>;

    /// Test if the given URL points to a valid location & existing content.
    async fn probe(&self, url: &str) -> Result<(), AggregatorHTTPClientError>;
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

    /// Perform a HTTP GET request on the Aggregator and return the given JSON
    #[async_recursion]
    async fn get(&self, url: &str) -> Result<Response, AggregatorHTTPClientError> {
        debug!("GET url='{url}'.");
        let request_builder = Client::new().get(url.to_owned());
        let current_api_version = self
            .compute_current_api_version()
            .await
            .unwrap()
            .to_string();
        debug!("Prepare request with version: {current_api_version}");
        let request_builder =
            request_builder.header(MITHRIL_API_VERSION_HEADER, current_api_version);
        let response = request_builder.send().await.map_err(|e| {
            AggregatorHTTPClientError::SubsystemError {
                message: format!(
                    "Cannot perform a GET against the Aggregator HTTP server (url='{url}')"
                ),
                error: e.into(),
            }
        })?;

        match response.status() {
            StatusCode::OK => Ok(response),
            StatusCode::PRECONDITION_FAILED => {
                if self.discard_current_api_version().await.is_some()
                    && !self.api_versions.read().await.is_empty()
                {
                    return self.get(url).await;
                }

                Err(self.handle_api_error(&response).await)
            }
            StatusCode::NOT_FOUND => Err(AggregatorHTTPClientError::RemoteServerLogical(format!(
                "Url='{url} not found"
            ))),
            status_code => Err(AggregatorHTTPClientError::RemoteServerTechnical(format!(
                "Unhandled error {status_code}"
            ))),
        }
    }

    /// API version error handling
    async fn handle_api_error(&self, response: &Response) -> AggregatorHTTPClientError {
        if let Some(version) = response.headers().get(MITHRIL_API_VERSION_HEADER) {
            AggregatorHTTPClientError::ApiVersionMismatch(format!(
                "server version: '{}', signer version: '{}'",
                version.to_str().unwrap(),
                self.compute_current_api_version().await.unwrap()
            ))
        } else {
            AggregatorHTTPClientError::ApiVersionMismatch(format!(
                "version precondition failed, sent version '{}'.",
                self.compute_current_api_version().await.unwrap()
            ))
        }
    }
}

#[cfg_attr(test, automock)]
#[async_trait]
impl AggregatorClient for AggregatorHTTPClient {
    async fn get_content(&self, url: &str) -> Result<String, AggregatorHTTPClientError> {
        let url = format!("{}/{}", self.aggregator_endpoint.trim_end_matches('/'), url);
        let response = self.get(&url).await?;
        let content = format!("{response:?}");

        response
            .text()
            .await
            .map_err(|e| AggregatorHTTPClientError::SubsystemError {
                message: format!("Could not find a JSON body in the response '{content}'."),
                error: e.into(),
            })
    }

    async fn download_unpack(
        &self,
        url: &str,
        target_dir: &Path,
        progress_reporter: DownloadProgressReporter,
    ) -> Result<(), AggregatorHTTPClientError> {
        if !target_dir.is_dir() {
            Err(AggregatorHTTPClientError::SubsystemError {
                message: "Download-Unpack: prerequisite error".to_string(),
                error: anyhow!(
                    "target path is not a directory or does not exist: `{}`",
                    target_dir.display()
                ),
            })?;
        }

        let mut downloaded_bytes: u64 = 0;
        let mut remote_stream = self.get(url).await?.bytes_stream();
        let (sender, receiver) = flume::bounded(5);

        let dest_dir = target_dir.to_path_buf();
        let unpack_thread = tokio::task::spawn_blocking(move || -> StdResult<()> {
            let unpacker = SnapshotUnpacker;
            unpacker.unpack_snapshot(receiver, &dest_dir)
        });

        while let Some(item) = remote_stream.next().await {
            let chunk = item.map_err(|e| {
                AggregatorHTTPClientError::RemoteServerTechnical(format!(
                    "Download: Could not read from byte stream: {e}"
                ))
            })?;

            sender.send_async(chunk.to_vec()).await.map_err(|e| {
                AggregatorHTTPClientError::SubsystemError {
                    message: format!("Download: could not write {} bytes to stream.", chunk.len()),
                    error: e.into(),
                }
            })?;

            downloaded_bytes += chunk.len() as u64;
            progress_reporter.report(downloaded_bytes);
        }

        drop(sender); // Signal EOF
        unpack_thread
            .await
            .map_err(|join_error| AggregatorHTTPClientError::SubsystemError {
                message: format!(
                    "Unpack: panic while unpacking to dir '{}'",
                    target_dir.display()
                ),
                error: join_error.into(),
            })?
            .map_err(|unpack_error| AggregatorHTTPClientError::SubsystemError {
                message: format!("Unpack: could not unpack to dir '{}'", target_dir.display()),
                error: unpack_error,
            })?;

        Ok(())
    }

    async fn probe(&self, url: &str) -> Result<(), AggregatorHTTPClientError> {
        debug!("HEAD url='{url}'.");
        let request_builder = Client::new().head(url.to_owned());
        let response = request_builder.send().await.map_err(|e| {
            AggregatorHTTPClientError::SubsystemError {
                message: format!("Cannot perform a HEAD for url='{url}'"),
                error: e.into(),
            }
        })?;
        match response.status() {
            StatusCode::OK => Ok(()),
            StatusCode::NOT_FOUND => Err(AggregatorHTTPClientError::RemoteServerLogical(format!(
                "Url='{url} not found"
            ))),
            status_code => Err(AggregatorHTTPClientError::RemoteServerTechnical(format!(
                "Unhandled error {status_code}"
            ))),
        }
    }
}
