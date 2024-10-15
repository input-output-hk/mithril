//! Mechanisms to exchange data with an Aggregator.
//!
//! The [AggregatorClient] trait abstracts how the communication with an Aggregator
//! is done.
//! The clients that need to communicate only need to define their request using the
//! [AggregatorRequest] enum.
//!
//! An implementation using HTTP is available: [AggregatorHTTPClient].

use anyhow::{anyhow, Context};
use async_recursion::async_recursion;
use async_trait::async_trait;
use reqwest::header::{HeaderMap, HeaderName, HeaderValue};
use reqwest::{Response, StatusCode, Url};
use semver::Version;
use slog::{debug, Logger};
use std::collections::HashMap;
use std::sync::Arc;
use thiserror::Error;
use tokio::sync::RwLock;

use mithril_common::entities::{ClientError, ServerError};
use mithril_common::MITHRIL_API_VERSION_HEADER;

#[cfg(feature = "unstable")]
use crate::common::Epoch;
use crate::{MithrilError, MithrilResult};

/// Error tied with the Aggregator client
#[derive(Error, Debug)]
pub enum AggregatorClientError {
    /// Error raised when querying the aggregator returned a 5XX error.
    #[error("Internal error of the Aggregator")]
    RemoteServerTechnical(#[source] MithrilError),

    /// Error raised when querying the aggregator returned a 4XX error.
    #[error("Invalid request to the Aggregator")]
    RemoteServerLogical(#[source] MithrilError),

    /// Error raised when the server API version mismatch the client API version.
    #[error("API version mismatch")]
    ApiVersionMismatch(#[source] MithrilError),

    /// HTTP subsystem error
    #[error("HTTP subsystem error")]
    SubsystemError(#[source] MithrilError),
}

/// What can be read from an [AggregatorClient].
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum AggregatorRequest {
    /// Get a specific [certificate][crate::MithrilCertificate] from the aggregator
    GetCertificate {
        /// Hash of the certificate to retrieve
        hash: String,
    },
    /// Lists the aggregator [certificates][crate::MithrilCertificate]
    ListCertificates,
    /// Get a specific [Mithril stake distribution][crate::MithrilStakeDistribution] from the aggregator
    GetMithrilStakeDistribution {
        /// Hash of the Mithril stake distribution to retrieve
        hash: String,
    },
    /// Lists the aggregator [Mithril stake distribution][crate::MithrilStakeDistribution]
    ListMithrilStakeDistributions,
    /// Get a specific [snapshot][crate::Snapshot] from the aggregator
    GetSnapshot {
        /// Digest of the snapshot to retrieve
        digest: String,
    },
    /// Lists the aggregator [snapshots][crate::Snapshot]
    ListSnapshots,

    /// Increments the aggregator snapshot download statistics
    IncrementSnapshotStatistic {
        /// Snapshot as HTTP request body
        snapshot: String,
    },

    /// Get proofs that the given set of Cardano transactions is included in the global Cardano transactions set
    GetTransactionsProofs {
        /// Hashes of the transactions to get proofs for.
        transactions_hashes: Vec<String>,
    },

    /// Get a specific [Cardano transaction snapshot][crate::CardanoTransactionSnapshot]
    GetCardanoTransactionSnapshot {
        /// Hash of the Cardano transaction snapshot to retrieve
        hash: String,
    },

    /// Lists the aggregator [Cardano transaction snapshot][crate::CardanoTransactionSnapshot]
    ListCardanoTransactionSnapshots,

    /// Get a specific [Cardano stake distribution][crate::CardanoStakeDistribution] from the aggregator by hash
    #[cfg(feature = "unstable")]
    GetCardanoStakeDistribution {
        /// Hash of the Cardano stake distribution to retrieve
        hash: String,
    },

    /// Get a specific [Cardano stake distribution][crate::CardanoStakeDistribution] from the aggregator by epoch
    #[cfg(feature = "unstable")]
    GetCardanoStakeDistributionByEpoch {
        /// Epoch at the end of which the Cardano stake distribution is computed by the Cardano node
        epoch: Epoch,
    },

    /// Lists the aggregator [Cardano stake distribution][crate::CardanoStakeDistribution]
    #[cfg(feature = "unstable")]
    ListCardanoStakeDistributions,
}

impl AggregatorRequest {
    /// Get the request route relative to the aggregator root endpoint.
    pub fn route(&self) -> String {
        match self {
            AggregatorRequest::GetCertificate { hash } => {
                format!("certificate/{hash}")
            }
            AggregatorRequest::ListCertificates => "certificates".to_string(),
            AggregatorRequest::GetMithrilStakeDistribution { hash } => {
                format!("artifact/mithril-stake-distribution/{hash}")
            }
            AggregatorRequest::ListMithrilStakeDistributions => {
                "artifact/mithril-stake-distributions".to_string()
            }
            AggregatorRequest::GetSnapshot { digest } => {
                format!("artifact/snapshot/{}", digest)
            }
            AggregatorRequest::ListSnapshots => "artifact/snapshots".to_string(),
            AggregatorRequest::IncrementSnapshotStatistic { snapshot: _ } => {
                "statistics/snapshot".to_string()
            }
            AggregatorRequest::GetTransactionsProofs {
                transactions_hashes,
            } => format!(
                "proof/cardano-transaction?transaction_hashes={}",
                transactions_hashes.join(",")
            ),
            AggregatorRequest::GetCardanoTransactionSnapshot { hash } => {
                format!("artifact/cardano-transaction/{hash}")
            }
            AggregatorRequest::ListCardanoTransactionSnapshots => {
                "artifact/cardano-transactions".to_string()
            }
            #[cfg(feature = "unstable")]
            AggregatorRequest::GetCardanoStakeDistribution { hash } => {
                format!("artifact/cardano-stake-distribution/{hash}")
            }
            #[cfg(feature = "unstable")]
            AggregatorRequest::GetCardanoStakeDistributionByEpoch { epoch } => {
                format!("artifact/cardano-stake-distribution/epoch/{epoch}")
            }
            #[cfg(feature = "unstable")]
            AggregatorRequest::ListCardanoStakeDistributions => {
                "artifact/cardano-stake-distributions".to_string()
            }
        }
    }

    /// Get the request body to send to the aggregator
    pub fn get_body(&self) -> Option<String> {
        match self {
            AggregatorRequest::IncrementSnapshotStatistic { snapshot } => {
                Some(snapshot.to_string())
            }
            _ => None,
        }
    }
}

/// API that defines a client for the Aggregator
#[cfg_attr(target_family = "wasm", async_trait(?Send))]
#[cfg_attr(not(target_family = "wasm"), async_trait)]
pub trait AggregatorClient: Sync + Send {
    /// Get the content back from the Aggregator
    async fn get_content(
        &self,
        request: AggregatorRequest,
    ) -> Result<String, AggregatorClientError>;

    /// Post information to the Aggregator
    async fn post_content(
        &self,
        request: AggregatorRequest,
    ) -> Result<String, AggregatorClientError>;
}

/// Responsible for HTTP transport and API version check.
pub struct AggregatorHTTPClient {
    http_client: reqwest::Client,
    aggregator_endpoint: Url,
    api_versions: Arc<RwLock<Vec<Version>>>,
    logger: Logger,
    http_headers: HeaderMap,
}

impl AggregatorHTTPClient {
    /// Constructs a new `AggregatorHTTPClient`
    pub fn new(
        aggregator_endpoint: Url,
        api_versions: Vec<Version>,
        logger: Logger,
        custom_headers: Option<HashMap<String, String>>,
    ) -> MithrilResult<Self> {
        let http_client = reqwest::ClientBuilder::new()
            .build()
            .with_context(|| "Building http client for Aggregator client failed")?;

        // Trailing slash is significant because url::join
        // (https://docs.rs/url/latest/url/struct.Url.html#method.join) will remove
        // the 'path' part of the url if it doesn't end with a trailing slash.
        let aggregator_endpoint = if aggregator_endpoint.as_str().ends_with('/') {
            aggregator_endpoint
        } else {
            let mut url = aggregator_endpoint.clone();
            url.set_path(&format!("{}/", aggregator_endpoint.path()));
            url
        };

        let mut http_headers = HeaderMap::new();
        if let Some(headers) = custom_headers {
            for (key, value) in headers.iter() {
                http_headers.insert(
                    HeaderName::from_bytes(key.as_bytes())?,
                    HeaderValue::from_str(value)?,
                );
            }
        }

        Ok(Self {
            http_client,
            aggregator_endpoint,
            api_versions: Arc::new(RwLock::new(api_versions)),
            logger,
            http_headers,
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
    #[cfg_attr(target_family = "wasm", async_recursion(?Send))]
    #[cfg_attr(not(target_family = "wasm"), async_recursion)]
    async fn get(&self, url: Url) -> Result<Response, AggregatorClientError> {
        debug!(self.logger, "GET url='{url}'.");
        let request_builder = self.http_client.get(url.clone());
        let current_api_version = self
            .compute_current_api_version()
            .await
            .unwrap()
            .to_string();
        debug!(
            self.logger,
            "Prepare request with version: {current_api_version}"
        );
        let request_builder = request_builder
            .header(MITHRIL_API_VERSION_HEADER, current_api_version)
            .headers(self.http_headers.clone());

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

                Err(self.handle_api_error(response.headers()).await)
            }
            StatusCode::NOT_FOUND => Err(Self::not_found_error(url)),
            status_code if status_code.is_client_error() => {
                Err(Self::remote_logical_error(response).await)
            }
            _ => Err(Self::remote_technical_error(response).await),
        }
    }

    #[cfg_attr(target_family = "wasm", async_recursion(?Send))]
    #[cfg_attr(not(target_family = "wasm"), async_recursion)]
    async fn post(&self, url: Url, json: &str) -> Result<Response, AggregatorClientError> {
        debug!(self.logger, "POST url='{url}'"; "json" => json);
        let request_builder = self.http_client.post(url.to_owned()).body(json.to_owned());
        let current_api_version = self
            .compute_current_api_version()
            .await
            .unwrap()
            .to_string();
        debug!(
            self.logger,
            "Prepare request with version: {current_api_version}"
        );
        let request_builder = request_builder
            .header(MITHRIL_API_VERSION_HEADER, current_api_version)
            .headers(self.http_headers.clone());

        let response = request_builder.send().await.map_err(|e| {
            AggregatorClientError::SubsystemError(
                anyhow!(e).context("Error while POSTing data '{json}' to URL='{url}'."),
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

                Err(self.handle_api_error(response.headers()).await)
            }
            StatusCode::NOT_FOUND => Err(Self::not_found_error(url)),
            status_code if status_code.is_client_error() => {
                Err(Self::remote_logical_error(response).await)
            }
            _ => Err(Self::remote_technical_error(response).await),
        }
    }

    fn get_url_for_route(&self, endpoint: &str) -> Result<Url, AggregatorClientError> {
        self.aggregator_endpoint
            .join(endpoint)
            .with_context(|| {
                format!(
                    "Invalid url when joining given endpoint, '{endpoint}', to aggregator url '{}'",
                    self.aggregator_endpoint
                )
            })
            .map_err(AggregatorClientError::SubsystemError)
    }

    /// API version error handling
    async fn handle_api_error(&self, response_header: &HeaderMap) -> AggregatorClientError {
        if let Some(version) = response_header.get(MITHRIL_API_VERSION_HEADER) {
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

    fn not_found_error(url: Url) -> AggregatorClientError {
        AggregatorClientError::RemoteServerLogical(anyhow!("Url='{url}' not found"))
    }

    async fn remote_logical_error(response: Response) -> AggregatorClientError {
        let status_code = response.status();
        let client_error = response
            .json::<ClientError>()
            .await
            .unwrap_or(ClientError::new(
                format!("Unhandled error {status_code}"),
                "",
            ));

        AggregatorClientError::RemoteServerLogical(anyhow!("{client_error}"))
    }

    async fn remote_technical_error(response: Response) -> AggregatorClientError {
        let status_code = response.status();
        let server_error = response
            .json::<ServerError>()
            .await
            .unwrap_or(ServerError::new(format!("Unhandled error {status_code}")));

        AggregatorClientError::RemoteServerTechnical(anyhow!("{server_error}"))
    }
}

#[cfg_attr(test, mockall::automock)]
#[cfg_attr(target_family = "wasm", async_trait(?Send))]
#[cfg_attr(not(target_family = "wasm"), async_trait)]
impl AggregatorClient for AggregatorHTTPClient {
    async fn get_content(
        &self,
        request: AggregatorRequest,
    ) -> Result<String, AggregatorClientError> {
        let response = self.get(self.get_url_for_route(&request.route())?).await?;
        let content = format!("{response:?}");

        response.text().await.map_err(|e| {
            AggregatorClientError::SubsystemError(anyhow!(e).context(format!(
                "Could not find a JSON body in the response '{content}'."
            )))
        })
    }

    async fn post_content(
        &self,
        request: AggregatorRequest,
    ) -> Result<String, AggregatorClientError> {
        let response = self
            .post(
                self.get_url_for_route(&request.route())?,
                &request.get_body().unwrap_or_default(),
            )
            .await?;

        response.text().await.map_err(|e| {
            AggregatorClientError::SubsystemError(
                anyhow!(e).context("Could not find a text body in the response."),
            )
        })
    }
}

#[cfg(test)]
mod tests {
    use httpmock::MockServer;
    use reqwest::header::{HeaderName, HeaderValue};
    use std::collections::HashMap;

    use mithril_common::api_version::APIVersionProvider;
    use mithril_common::entities::{ClientError, ServerError};

    use super::*;

    macro_rules! assert_error_eq {
        ($left:expr, $right:expr) => {
            assert_eq!(format!("{:?}", &$left), format!("{:?}", &$right),);
        };
    }

    fn setup_client(
        server_url: &str,
        api_versions: Vec<Version>,
        custom_headers: Option<HashMap<String, String>>,
    ) -> AggregatorHTTPClient {
        AggregatorHTTPClient::new(
            Url::parse(server_url).unwrap(),
            api_versions,
            crate::test_utils::test_logger(),
            custom_headers,
        )
        .expect("building aggregator http client should not fail")
    }

    fn setup_server_and_client() -> (MockServer, AggregatorHTTPClient) {
        let server = MockServer::start();
        let client = setup_client(
            &server.url(""),
            APIVersionProvider::compute_all_versions_sorted().unwrap(),
            None,
        );
        (server, client)
    }

    fn setup_server_and_client_with_custom_headers(
        custom_headers: HashMap<String, String>,
    ) -> (MockServer, AggregatorHTTPClient) {
        let server = MockServer::start();
        let client = setup_client(
            &server.url(""),
            APIVersionProvider::compute_all_versions_sorted().unwrap(),
            Some(custom_headers),
        );
        (server, client)
    }

    fn mithril_api_version_headers(version: &str) -> HeaderMap {
        HeaderMap::from_iter([(
            HeaderName::from_static(MITHRIL_API_VERSION_HEADER),
            HeaderValue::from_str(version).unwrap(),
        )])
    }

    #[test]
    fn always_append_trailing_slash_at_build() {
        for (expected, url) in [
            ("http://www.test.net/", "http://www.test.net/"),
            ("http://www.test.net/", "http://www.test.net"),
            (
                "http://www.test.net/aggregator/",
                "http://www.test.net/aggregator/",
            ),
            (
                "http://www.test.net/aggregator/",
                "http://www.test.net/aggregator",
            ),
        ] {
            let url = Url::parse(url).unwrap();
            let client =
                AggregatorHTTPClient::new(url, vec![], crate::test_utils::test_logger(), None)
                    .expect("building aggregator http client should not fail");

            assert_eq!(expected, client.aggregator_endpoint.as_str());
        }
    }

    #[test]
    fn deduce_routes_from_request() {
        assert_eq!(
            "certificate/abc".to_string(),
            AggregatorRequest::GetCertificate {
                hash: "abc".to_string()
            }
            .route()
        );

        assert_eq!(
            "artifact/mithril-stake-distribution/abc".to_string(),
            AggregatorRequest::GetMithrilStakeDistribution {
                hash: "abc".to_string()
            }
            .route()
        );

        assert_eq!(
            "artifact/mithril-stake-distribution/abc".to_string(),
            AggregatorRequest::GetMithrilStakeDistribution {
                hash: "abc".to_string()
            }
            .route()
        );

        assert_eq!(
            "artifact/mithril-stake-distributions".to_string(),
            AggregatorRequest::ListMithrilStakeDistributions.route()
        );

        assert_eq!(
            "artifact/snapshot/abc".to_string(),
            AggregatorRequest::GetSnapshot {
                digest: "abc".to_string()
            }
            .route()
        );

        assert_eq!(
            "artifact/snapshots".to_string(),
            AggregatorRequest::ListSnapshots.route()
        );

        assert_eq!(
            "statistics/snapshot".to_string(),
            AggregatorRequest::IncrementSnapshotStatistic {
                snapshot: "abc".to_string()
            }
            .route()
        );

        assert_eq!(
            "proof/cardano-transaction?transaction_hashes=abc,def,ghi,jkl".to_string(),
            AggregatorRequest::GetTransactionsProofs {
                transactions_hashes: vec![
                    "abc".to_string(),
                    "def".to_string(),
                    "ghi".to_string(),
                    "jkl".to_string()
                ]
            }
            .route()
        );

        assert_eq!(
            "artifact/cardano-transaction/abc".to_string(),
            AggregatorRequest::GetCardanoTransactionSnapshot {
                hash: "abc".to_string()
            }
            .route()
        );

        assert_eq!(
            "artifact/cardano-transactions".to_string(),
            AggregatorRequest::ListCardanoTransactionSnapshots.route()
        );

        #[cfg(feature = "unstable")]
        {
            assert_eq!(
                "artifact/cardano-stake-distribution/abc".to_string(),
                AggregatorRequest::GetCardanoStakeDistribution {
                    hash: "abc".to_string()
                }
                .route()
            );

            assert_eq!(
                "artifact/cardano-stake-distribution/epoch/123".to_string(),
                AggregatorRequest::GetCardanoStakeDistributionByEpoch { epoch: Epoch(123) }.route()
            );

            assert_eq!(
                "artifact/cardano-stake-distributions".to_string(),
                AggregatorRequest::ListCardanoStakeDistributions.route()
            );
        }
    }

    #[tokio::test]
    async fn test_client_handle_4xx_errors() {
        let client_error = ClientError::new("label", "message");

        let (aggregator, client) = setup_server_and_client();
        aggregator.mock(|_when, then| {
            then.status(StatusCode::IM_A_TEAPOT.as_u16())
                .json_body_obj(&client_error);
        });

        let expected_error = AggregatorClientError::RemoteServerLogical(anyhow!("{client_error}"));

        let get_content_error = client
            .get_content(AggregatorRequest::ListCertificates)
            .await
            .unwrap_err();
        assert_error_eq!(get_content_error, expected_error);

        let post_content_error = client
            .post_content(AggregatorRequest::ListCertificates)
            .await
            .unwrap_err();
        assert_error_eq!(post_content_error, expected_error);
    }

    #[tokio::test]
    async fn test_client_handle_404_not_found_error() {
        let client_error = ClientError::new("label", "message");

        let (aggregator, client) = setup_server_and_client();
        aggregator.mock(|_when, then| {
            then.status(StatusCode::NOT_FOUND.as_u16())
                .json_body_obj(&client_error);
        });

        let expected_error = AggregatorHTTPClient::not_found_error(
            Url::parse(&format!(
                "{}/{}",
                aggregator.base_url(),
                AggregatorRequest::ListCertificates.route()
            ))
            .unwrap(),
        );

        let get_content_error = client
            .get_content(AggregatorRequest::ListCertificates)
            .await
            .unwrap_err();
        assert_error_eq!(get_content_error, expected_error);

        let post_content_error = client
            .post_content(AggregatorRequest::ListCertificates)
            .await
            .unwrap_err();
        assert_error_eq!(post_content_error, expected_error);
    }

    #[tokio::test]
    async fn test_client_handle_5xx_errors() {
        let server_error = ServerError::new("message");

        let (aggregator, client) = setup_server_and_client();
        aggregator.mock(|_when, then| {
            then.status(StatusCode::INTERNAL_SERVER_ERROR.as_u16())
                .json_body_obj(&server_error);
        });

        let expected_error =
            AggregatorClientError::RemoteServerTechnical(anyhow!("{server_error}"));

        let get_content_error = client
            .get_content(AggregatorRequest::ListCertificates)
            .await
            .unwrap_err();
        assert_error_eq!(get_content_error, expected_error);

        let post_content_error = client
            .post_content(AggregatorRequest::ListCertificates)
            .await
            .unwrap_err();
        assert_error_eq!(post_content_error, expected_error);
    }

    #[tokio::test]
    async fn test_client_handle_412_api_version_mismatch_with_version_in_response_header() {
        let version = "0.0.0";

        let (aggregator, client) = setup_server_and_client();
        aggregator.mock(|_when, then| {
            then.status(StatusCode::PRECONDITION_FAILED.as_u16())
                .header(MITHRIL_API_VERSION_HEADER, version);
        });

        let expected_error = client
            .handle_api_error(&mithril_api_version_headers(version))
            .await;

        let get_content_error = client
            .get_content(AggregatorRequest::ListCertificates)
            .await
            .unwrap_err();
        assert_error_eq!(get_content_error, expected_error);

        let post_content_error = client
            .post_content(AggregatorRequest::ListCertificates)
            .await
            .unwrap_err();
        assert_error_eq!(post_content_error, expected_error);
    }

    #[tokio::test]
    async fn test_client_handle_412_api_version_mismatch_without_version_in_response_header() {
        let (aggregator, client) = setup_server_and_client();
        aggregator.mock(|_when, then| {
            then.status(StatusCode::PRECONDITION_FAILED.as_u16());
        });

        let expected_error = client.handle_api_error(&HeaderMap::new()).await;

        let get_content_error = client
            .get_content(AggregatorRequest::ListCertificates)
            .await
            .unwrap_err();
        assert_error_eq!(get_content_error, expected_error);

        let post_content_error = client
            .post_content(AggregatorRequest::ListCertificates)
            .await
            .unwrap_err();
        assert_error_eq!(post_content_error, expected_error);
    }

    #[tokio::test]
    async fn test_client_can_fallback_to_a_second_version_when_412_api_version_mismatch() {
        let bad_version = "0.0.0";
        let good_version = "1.0.0";

        let aggregator = MockServer::start();
        let client = setup_client(
            &aggregator.url(""),
            vec![
                Version::parse(bad_version).unwrap(),
                Version::parse(good_version).unwrap(),
            ],
            None,
        );
        aggregator.mock(|when, then| {
            when.header(MITHRIL_API_VERSION_HEADER, bad_version);
            then.status(StatusCode::PRECONDITION_FAILED.as_u16())
                .header(MITHRIL_API_VERSION_HEADER, bad_version);
        });
        aggregator.mock(|when, then| {
            when.header(MITHRIL_API_VERSION_HEADER, good_version);
            then.status(StatusCode::OK.as_u16());
        });

        assert_eq!(
            client.compute_current_api_version().await,
            Some(Version::parse(bad_version).unwrap()),
            "Bad version should be tried first"
        );

        client
            .get_content(AggregatorRequest::ListCertificates)
            .await
            .expect("should have run with a fallback version");

        client
            .post_content(AggregatorRequest::ListCertificates)
            .await
            .expect("should have run with a fallback version");
    }

    #[tokio::test]
    async fn test_client_with_custom_headers() {
        let mut http_headers = HashMap::new();
        http_headers.insert("Custom-Header".to_string(), "CustomValue".to_string());
        http_headers.insert("Another-Header".to_string(), "AnotherValue".to_string());
        let (aggregator, client) = setup_server_and_client_with_custom_headers(http_headers);
        aggregator.mock(|when, then| {
            when.header("Custom-Header", "CustomValue")
                .header("Another-Header", "AnotherValue");
            then.status(StatusCode::OK.as_u16()).body("ok");
        });

        client
            .get_content(AggregatorRequest::ListCertificates)
            .await
            .expect("GET request should succeed");

        client
            .post_content(AggregatorRequest::ListCertificates)
            .await
            .expect("GET request should succeed");
    }
}
