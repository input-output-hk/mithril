use anyhow::{anyhow, Context};
use async_recursion::async_recursion;
use async_trait::async_trait;
use futures::StreamExt;
use reqwest::{Response, StatusCode, Url};
use semver::Version;
use slog_scope::debug;
use std::{path::Path, sync::Arc};
use thiserror::Error;
use tokio::sync::RwLock;

#[cfg(test)]
use mockall::automock;

use mithril_common::MITHRIL_API_VERSION_HEADER;

use crate::common::CompressionAlgorithm;
use crate::utils::SnapshotUnpacker;
use crate::{MithrilError, MithrilResult};

/// Error tied with the Aggregator client
#[derive(Error, Debug)]
pub enum AggregatorClientError {
    /// Error raised when querying the aggregator returned a 5XX error.
    #[error("remote server technical error")]
    RemoteServerTechnical(#[source] MithrilError),

    /// Error raised when querying the aggregator returned a 4XX error.
    #[error("remote server logical error")]
    RemoteServerLogical(#[source] MithrilError),

    /// Error raised when the server API version mismatch the client API version.
    #[error("API version mismatch")]
    ApiVersionMismatch(#[source] MithrilError),

    /// HTTP subsystem error
    #[error("HTTP subsystem error")]
    SubsystemError(#[source] MithrilError),
}

/// All operations that can be asked to an [AggregatorClient].
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum AggregatorRequest {
    /// What can be read from an [AggregatorClient].
    Read(AggregatorReadRequest),
    /// What can be downloaded from an [AggregatorClient].
    Download(AggregatorDownloadRequest),
}

impl AggregatorRequest {
    /// Get the request route relative to the aggregator root url.
    pub fn route(&self) -> String {
        match self {
            AggregatorRequest::Read(request) => request.route(),
            AggregatorRequest::Download(request) => request.route(),
        }
    }
}

/// What can be read from an [AggregatorClient].
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum AggregatorReadRequest {
    /// Get a specific [certificate][crate::MithrilCertificate] from the aggregator
    GetCertificate {
        /// Hash of the certificate to retrieve
        hash: String,
    },
    /// Lists the aggregator [certificates][crate::MithrilCertificate]
    ListCertificates,
    /// Get a specific [mithril stake distribution][crate::MithrilStakeDistribution] from the aggregator
    GetMithrilStakeDistribution {
        /// Hash of the mithril stake distribution to retrieve
        hash: String,
    },
    /// Lists the aggregator [mithril stake distribution][crate::MithrilStakeDistribution]
    ListMithrilStakeDistributions,
    /// Get a specific [snapshot][crate::Snapshot] from the aggregator
    GetSnapshot {
        /// Digest of the snapshot to retrieve
        digest: String,
    },
    /// Lists the aggregator [snapshots][crate::Snapshot]
    ListSnapshots,
}

impl AggregatorReadRequest {
    /// Get the request route relative to the aggregator root url.
    pub fn route(&self) -> String {
        match self {
            AggregatorReadRequest::GetCertificate { hash } => {
                format!("certificate/{hash}")
            }
            AggregatorReadRequest::ListCertificates => "certificates/".to_string(),
            AggregatorReadRequest::GetMithrilStakeDistribution { hash } => {
                format!("artifact/mithril-stake-distribution/{hash}")
            }
            AggregatorReadRequest::ListMithrilStakeDistributions => {
                "artifact/mithril-stake-distributions".to_string()
            }
            AggregatorReadRequest::GetSnapshot { digest } => {
                format!("artifact/snapshot/{}", digest)
            }
            AggregatorReadRequest::ListSnapshots => "artifact/snapshots".to_string(),
        }
    }
}

/// What can be downloaded from an [AggregatorClient].
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum AggregatorDownloadRequest {
    /// Ask to download a snapshot from the given location
    Snapshot {
        /// Location of the snapshot
        location: String,
    },
}

impl AggregatorDownloadRequest {
    /// Get the request route relative to the aggregator root url.
    pub fn route(&self) -> String {
        match self {
            AggregatorDownloadRequest::Snapshot { location } => location.to_string(),
        }
    }
}

/// API that defines a client for the Aggregator
#[async_trait]
pub trait AggregatorClient: Sync + Send {
    /// Get the content back from the Aggregator
    async fn get_content(
        &self,
        request: AggregatorReadRequest,
    ) -> Result<String, AggregatorClientError>;

    /// Download and unpack large archives on the disk
    async fn download_unpack(
        &self,
        request: AggregatorDownloadRequest,
        target_dir: &Path,
        compression_algorithm: CompressionAlgorithm,
    ) -> Result<(), AggregatorClientError>;

    /// Test if the given endpoint is a valid location for the aggregator & has existing content.
    async fn probe(&self, request: AggregatorRequest) -> Result<(), AggregatorClientError>;
}

/// Responsible of HTTP transport and API version check.
pub struct AggregatorHTTPClient {
    http_client: reqwest::Client,
    aggregator_url: Url,
    api_versions: Arc<RwLock<Vec<Version>>>,
}

impl AggregatorHTTPClient {
    /// AggregatorHTTPClient factory
    pub fn new(aggregator_endpoint: Url, api_versions: Vec<Version>) -> MithrilResult<Self> {
        debug!("New AggregatorHTTPClient created");
        let http_client = reqwest::ClientBuilder::new()
            .build()
            .with_context(|| "Building http client for Aggregator client failed")?;

        Ok(Self {
            http_client,
            aggregator_url: aggregator_endpoint,
            api_versions: Arc::new(RwLock::new(api_versions)),
        })
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
    async fn get(&self, url: Url) -> Result<Response, AggregatorClientError> {
        debug!("GET url='{url}'.");
        let request_builder = self.http_client.get(url.clone());
        let current_api_version = self
            .compute_current_api_version()
            .await
            .unwrap()
            .to_string();
        debug!("Prepare request with version: {current_api_version}");
        let request_builder =
            request_builder.header(MITHRIL_API_VERSION_HEADER, current_api_version);
        let response = request_builder.send().await.map_err(|e| {
            AggregatorClientError::SubsystemError(anyhow!(e).context(format!(
                "Cannot perform a GET against the Aggregator HTTP server (url='{url}')"
            )))
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
            StatusCode::NOT_FOUND => Err(AggregatorClientError::RemoteServerLogical(anyhow!(
                "Url='{url} not found"
            ))),
            status_code => Err(AggregatorClientError::RemoteServerTechnical(anyhow!(
                "Unhandled error {status_code}"
            ))),
        }
    }

    /// API version error handling
    async fn handle_api_error(&self, response: &Response) -> AggregatorClientError {
        if let Some(version) = response.headers().get(MITHRIL_API_VERSION_HEADER) {
            AggregatorClientError::ApiVersionMismatch(anyhow!(
                "server version: '{}', signer version: '{}'",
                version.to_str().unwrap(),
                self.compute_current_api_version().await.unwrap()
            ))
        } else {
            AggregatorClientError::ApiVersionMismatch(anyhow!(
                "version precondition failed, sent version '{}'.",
                self.compute_current_api_version().await.unwrap()
            ))
        }
    }

    fn get_url_for_endpoint(&self, endpoint: &str) -> Result<Url, AggregatorClientError> {
        self.aggregator_url
            .join(endpoint)
            .with_context(|| {
                format!(
                    "Invalid url when joining given endpoint, '{endpoint}', to aggregator url '{}'",
                    self.aggregator_url
                )
            })
            .map_err(AggregatorClientError::SubsystemError)
    }
}

#[cfg_attr(test, automock)]
#[async_trait]
impl AggregatorClient for AggregatorHTTPClient {
    async fn get_content(
        &self,
        request: AggregatorReadRequest,
    ) -> Result<String, AggregatorClientError> {
        let response = self
            .get(self.get_url_for_endpoint(&request.route())?)
            .await?;
        let content = format!("{response:?}");

        response.text().await.map_err(|e| {
            AggregatorClientError::SubsystemError(anyhow!(e).context(format!(
                "Could not find a JSON body in the response '{content}'."
            )))
        })
    }

    async fn download_unpack(
        &self,
        request: AggregatorDownloadRequest,
        target_dir: &Path,
        compression_algorithm: CompressionAlgorithm,
    ) -> Result<(), AggregatorClientError> {
        if !target_dir.is_dir() {
            Err(AggregatorClientError::SubsystemError(
                anyhow!("target path is not a directory or does not exist: `{target_dir:?}`")
                    .context("Download-Unpack: prerequisite error"),
            ))?;
        }

        let mut downloaded_bytes: u64 = 0;
        let mut remote_stream = self
            .get(self.get_url_for_endpoint(&request.route())?)
            .await?
            .bytes_stream();
        let (sender, receiver) = flume::bounded(5);

        let dest_dir = target_dir.to_path_buf();
        let unpack_thread = tokio::task::spawn_blocking(move || -> MithrilResult<()> {
            let unpacker = SnapshotUnpacker;
            unpacker.unpack_snapshot(receiver, compression_algorithm, &dest_dir)
        });

        while let Some(item) = remote_stream.next().await {
            let chunk = item.map_err(|e| {
                AggregatorClientError::RemoteServerTechnical(anyhow!(
                    "Download: Could not read from byte stream: {e}"
                ))
            })?;

            sender.send_async(chunk.to_vec()).await.map_err(|e| {
                AggregatorClientError::SubsystemError(anyhow!(e).context(format!(
                    "Download: could not write {} bytes to stream.",
                    chunk.len()
                )))
            })?;

            downloaded_bytes += chunk.len() as u64;
            // todo: report download progress here
        }

        drop(sender); // Signal EOF
        unpack_thread
            .await
            .map_err(|join_error| {
                AggregatorClientError::SubsystemError(anyhow!(join_error).context(format!(
                    "Unpack: panic while unpacking to dir '{}'",
                    target_dir.display()
                )))
            })?
            .map_err(|unpack_error| {
                AggregatorClientError::SubsystemError(anyhow!(unpack_error).context(format!(
                    "Unpack: could not unpack to dir '{}'",
                    target_dir.display()
                )))
            })?;

        Ok(())
    }

    async fn probe(&self, request: AggregatorRequest) -> Result<(), AggregatorClientError> {
        let endpoint = request.route();
        debug!("HEAD url='{endpoint}'.");
        let url = self.get_url_for_endpoint(&endpoint)?;
        let request_builder = self.http_client.head(url.to_owned());
        let response = request_builder.send().await.map_err(|e| {
            AggregatorClientError::SubsystemError(
                anyhow!(e).context("Cannot perform a HEAD for url='{url}'"),
            )
        })?;
        match response.status() {
            StatusCode::OK => Ok(()),
            StatusCode::NOT_FOUND => Err(AggregatorClientError::RemoteServerLogical(anyhow!(
                "Url='{url} not found"
            ))),
            status_code => Err(AggregatorClientError::RemoteServerTechnical(anyhow!(
                "Unhandled error {status_code}"
            ))),
        }
    }
}
