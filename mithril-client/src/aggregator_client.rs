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
use slog::{debug, error, warn, Logger};
use std::collections::HashMap;
use std::sync::Arc;
use thiserror::Error;
use tokio::sync::RwLock;

use mithril_common::entities::{ClientError, ServerError};
use mithril_common::logging::LoggerExtensions;
use mithril_common::messages::CardanoDatabaseImmutableFilesRestoredMessage;
use mithril_common::MITHRIL_API_VERSION_HEADER;

use crate::common::Epoch;
use crate::{MithrilError, MithrilResult};

const API_VERSION_MISMATCH_WARNING_MESSAGE: &str =
    "OpenAPI version may be incompatible, please update Mithril client library to the latest version.";

/// Error tied with the Aggregator client
#[derive(Error, Debug)]
pub enum AggregatorClientError {
    /// Error raised when querying the aggregator returned a 5XX error.
    #[error("Internal error of the Aggregator")]
    RemoteServerTechnical(#[source] MithrilError),

    /// Error raised when querying the aggregator returned a 4XX error.
    #[error("Invalid request to the Aggregator")]
    RemoteServerLogical(#[source] MithrilError),

    /// HTTP subsystem error
    #[error("HTTP subsystem error")]
    SubsystemError(#[source] MithrilError),
}

/// What can be read from an [AggregatorClient].
#[derive(Debug, Clone, Eq, PartialEq)]
#[cfg_attr(test, derive(strum::EnumIter))]
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

    /// Get a specific [Cardano database snapshot][crate::CardanoDatabaseSnapshot] from the aggregator
    GetCardanoDatabaseSnapshot {
        /// Hash of the snapshot to retrieve
        hash: String,
    },

    /// Lists the aggregator [Cardano database snapshots][crate::CardanoDatabaseSnapshot]
    ListCardanoDatabaseSnapshots,

    /// Increments the aggregator Cardano database snapshot immutable files restored statistics
    IncrementCardanoDatabaseImmutablesRestoredStatistic {
        /// Number of immutable files restored
        number_of_immutables: u64,
    },

    /// Increments the aggregator Cardano database snapshot ancillary files restored statistics
    IncrementCardanoDatabaseAncillaryStatistic,

    /// Increments the aggregator Cardano database snapshot complete restoration statistics
    IncrementCardanoDatabaseCompleteRestorationStatistic,

    /// Increments the aggregator Cardano database snapshot partial restoration statistics
    IncrementCardanoDatabasePartialRestorationStatistic,

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
    GetCardanoStakeDistribution {
        /// Hash of the Cardano stake distribution to retrieve
        hash: String,
    },

    /// Get a specific [Cardano stake distribution][crate::CardanoStakeDistribution] from the aggregator by epoch
    GetCardanoStakeDistributionByEpoch {
        /// Epoch at the end of which the Cardano stake distribution is computed by the Cardano node
        epoch: Epoch,
    },

    /// Lists the aggregator [Cardano stake distribution][crate::CardanoStakeDistribution]
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
                format!("artifact/snapshot/{digest}")
            }
            AggregatorRequest::ListSnapshots => "artifact/snapshots".to_string(),
            AggregatorRequest::IncrementSnapshotStatistic { snapshot: _ } => {
                "statistics/snapshot".to_string()
            }
            AggregatorRequest::GetCardanoDatabaseSnapshot { hash } => {
                format!("artifact/cardano-database/{hash}")
            }
            AggregatorRequest::ListCardanoDatabaseSnapshots => {
                "artifact/cardano-database".to_string()
            }
            AggregatorRequest::IncrementCardanoDatabaseImmutablesRestoredStatistic {
                number_of_immutables: _,
            } => "statistics/cardano-database/immutable-files-restored".to_string(),
            AggregatorRequest::IncrementCardanoDatabaseAncillaryStatistic => {
                "statistics/cardano-database/ancillary-files-restored".to_string()
            }
            AggregatorRequest::IncrementCardanoDatabaseCompleteRestorationStatistic => {
                "statistics/cardano-database/complete-restoration".to_string()
            }
            AggregatorRequest::IncrementCardanoDatabasePartialRestorationStatistic => {
                "statistics/cardano-database/partial-restoration".to_string()
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
            AggregatorRequest::GetCardanoStakeDistribution { hash } => {
                format!("artifact/cardano-stake-distribution/{hash}")
            }
            AggregatorRequest::GetCardanoStakeDistributionByEpoch { epoch } => {
                format!("artifact/cardano-stake-distribution/epoch/{epoch}")
            }
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
            AggregatorRequest::IncrementCardanoDatabaseImmutablesRestoredStatistic {
                number_of_immutables,
            } => serde_json::to_string(&CardanoDatabaseImmutableFilesRestoredMessage {
                nb_immutable_files: *number_of_immutables as u32,
            })
            .ok(),
            _ => None,
        }
    }
}

/// API that defines a client for the Aggregator
#[cfg_attr(test, mockall::automock)]
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
            logger: logger.new_with_component_name::<Self>(),
            http_headers,
        })
    }

    /// Computes the current api version
    async fn compute_current_api_version(&self) -> Option<Version> {
        self.api_versions.read().await.first().cloned()
    }

    /// Perform a HTTP GET request on the Aggregator and return the given JSON
    #[cfg_attr(target_family = "wasm", async_recursion(?Send))]
    #[cfg_attr(not(target_family = "wasm"), async_recursion)]
    async fn get(&self, url: Url) -> Result<Response, AggregatorClientError> {
        debug!(self.logger, "GET url='{url}'.");
        let request_builder = self.http_client.get(url.clone());
        let current_api_version = self.compute_current_api_version().await.unwrap().to_string();
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
            StatusCode::OK => {
                self.warn_if_api_version_mismatch(&response).await;

                Ok(response)
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
        let current_api_version = self.compute_current_api_version().await.unwrap().to_string();
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
            StatusCode::OK | StatusCode::CREATED => {
                self.warn_if_api_version_mismatch(&response).await;

                Ok(response)
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

    fn not_found_error(url: Url) -> AggregatorClientError {
        AggregatorClientError::RemoteServerLogical(anyhow!("Url='{url}' not found"))
    }

    async fn remote_logical_error(response: Response) -> AggregatorClientError {
        let status_code = response.status();
        let client_error = response.json::<ClientError>().await.unwrap_or(ClientError::new(
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

    /// Check API version mismatch and log a warning if the aggregator's version is more recent.
    async fn warn_if_api_version_mismatch(&self, response: &Response) {
        let aggregator_version = response
            .headers()
            .get(MITHRIL_API_VERSION_HEADER)
            .and_then(|v| v.to_str().ok())
            .and_then(|s| Version::parse(s).ok());

        let client_version = self.compute_current_api_version().await;

        match (aggregator_version, client_version) {
            (Some(aggregator), Some(client)) if client < aggregator => {
                warn!(self.logger, "{}", API_VERSION_MISMATCH_WARNING_MESSAGE;
                    "aggregator_version" => %aggregator,
                    "client_version" => %client,
                );
            }
            (Some(_), None) => {
                error!(
                    self.logger,
                    "Failed to compute the current client API version"
                );
            }
            _ => {}
        }
    }
}

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
    use std::collections::HashMap;
    use strum::IntoEnumIterator;

    use mithril_common::api_version::APIVersionProvider;
    use mithril_common::entities::{ClientError, ServerError};

    use crate::test_utils::TestLogger;

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
            TestLogger::stdout(),
            custom_headers,
        )
        .expect("building aggregator http client should not fail")
    }

    fn setup_server_and_client() -> (MockServer, AggregatorHTTPClient) {
        let server = MockServer::start();
        let client = setup_client(
            &server.url(""),
            APIVersionProvider::compute_all_versions_sorted(),
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
            APIVersionProvider::compute_all_versions_sorted(),
            Some(custom_headers),
        );
        (server, client)
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
            let client = AggregatorHTTPClient::new(url, vec![], TestLogger::stdout(), None)
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
            "artifact/cardano-database/abc".to_string(),
            AggregatorRequest::GetCardanoDatabaseSnapshot {
                hash: "abc".to_string()
            }
            .route()
        );

        assert_eq!(
            "artifact/cardano-database".to_string(),
            AggregatorRequest::ListCardanoDatabaseSnapshots.route()
        );

        assert_eq!(
            "statistics/cardano-database/immutable-files-restored".to_string(),
            AggregatorRequest::IncrementCardanoDatabaseImmutablesRestoredStatistic {
                number_of_immutables: 58
            }
            .route()
        );

        assert_eq!(
            "statistics/cardano-database/ancillary-files-restored".to_string(),
            AggregatorRequest::IncrementCardanoDatabaseAncillaryStatistic.route()
        );

        assert_eq!(
            "statistics/cardano-database/complete-restoration".to_string(),
            AggregatorRequest::IncrementCardanoDatabaseCompleteRestorationStatistic.route()
        );

        assert_eq!(
            "statistics/cardano-database/partial-restoration".to_string(),
            AggregatorRequest::IncrementCardanoDatabasePartialRestorationStatistic.route()
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

    #[test]
    fn deduce_body_from_request() {
        fn that_should_not_have_body(req: &AggregatorRequest) -> bool {
            !matches!(
                req,
                AggregatorRequest::IncrementSnapshotStatistic { .. }
                    | AggregatorRequest::IncrementCardanoDatabaseImmutablesRestoredStatistic { .. }
            )
        }

        assert_eq!(
            Some(r#"{"key":"value"}"#.to_string()),
            AggregatorRequest::IncrementSnapshotStatistic {
                snapshot: r#"{"key":"value"}"#.to_string()
            }
            .get_body()
        );

        assert_eq!(
            Some(
                serde_json::to_string(&CardanoDatabaseImmutableFilesRestoredMessage {
                    nb_immutable_files: 432,
                })
                .unwrap()
            ),
            AggregatorRequest::IncrementCardanoDatabaseImmutablesRestoredStatistic {
                number_of_immutables: 432,
            }
            .get_body()
        );

        for req_that_should_not_have_body in
            AggregatorRequest::iter().filter(that_should_not_have_body)
        {
            assert_eq!(None, req_that_should_not_have_body.get_body());
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

    #[tokio::test]
    async fn test_client_sends_accept_encoding_header_with_correct_values() {
        let (aggregator, client) = setup_server_and_client();
        aggregator.mock(|when, then| {
            when.matches(|req| {
                let headers = req.headers.clone().expect("HTTP headers not found");
                let accept_encoding_header = headers
                    .iter()
                    .find(|(name, _values)| name.to_lowercase() == "accept-encoding")
                    .expect("Accept-Encoding header not found");

                let header_value = accept_encoding_header.clone().1;
                ["gzip", "br", "deflate", "zstd"]
                    .iter()
                    .all(|&value| header_value.contains(value))
            });

            then.status(200).body("ok");
        });

        client
            .get_content(AggregatorRequest::ListCertificates)
            .await
            .expect("GET request should succeed with Accept-Encoding header");
    }

    #[tokio::test]
    async fn test_client_with_custom_headers_sends_accept_encoding_header_with_correct_values() {
        let mut http_headers = HashMap::new();
        http_headers.insert("Custom-Header".to_string(), "CustomValue".to_string());
        let (aggregator, client) = setup_server_and_client_with_custom_headers(http_headers);
        aggregator.mock(|when, then| {
            when.matches(|req| {
                let headers = req.headers.clone().expect("HTTP headers not found");
                let accept_encoding_header = headers
                    .iter()
                    .find(|(name, _values)| name.to_lowercase() == "accept-encoding")
                    .expect("Accept-Encoding header not found");

                let header_value = accept_encoding_header.clone().1;
                ["gzip", "br", "deflate", "zstd"]
                    .iter()
                    .all(|&value| header_value.contains(value))
            });

            then.status(200).body("ok");
        });

        client
            .get_content(AggregatorRequest::ListCertificates)
            .await
            .expect("GET request should succeed with Accept-Encoding header");
    }

    mod warn_if_api_version_mismatch {
        use http::response::Builder as HttpResponseBuilder;

        use mithril_common::test_utils::MemoryDrainForTestInspector;

        use super::*;

        fn build_fake_response_with_header<K: Into<String>, V: Into<String>>(
            key: K,
            value: V,
        ) -> Response {
            HttpResponseBuilder::new()
                .header(key.into(), value.into())
                .body("whatever")
                .unwrap()
                .into()
        }

        fn assert_api_version_warning_logged<A: Into<String>, S: Into<String>>(
            log_inspector: &MemoryDrainForTestInspector,
            aggregator_version: A,
            client_version: S,
        ) {
            assert!(log_inspector.contains_log(API_VERSION_MISMATCH_WARNING_MESSAGE));
            assert!(log_inspector
                .contains_log(&format!("aggregator_version={}", aggregator_version.into())));
            assert!(
                log_inspector.contains_log(&format!("client_version={}", client_version.into()))
            );
        }

        #[tokio::test]
        async fn test_logs_warning_when_aggregator_api_version_is_newer() {
            let aggregator_version = "2.0.0";
            let client_version = "1.0.0";
            let (logger, log_inspector) = TestLogger::memory();
            let mut client = setup_client(
                "http://whatever",
                vec![Version::parse(client_version).unwrap()],
                None,
            );
            client.logger = logger;
            let response =
                build_fake_response_with_header(MITHRIL_API_VERSION_HEADER, aggregator_version);

            assert!(
                Version::parse(aggregator_version).unwrap()
                    > Version::parse(client_version).unwrap()
            );

            client.warn_if_api_version_mismatch(&response).await;

            assert_api_version_warning_logged(&log_inspector, aggregator_version, client_version);
        }

        #[tokio::test]
        async fn test_no_warning_logged_when_versions_match() {
            let version = "1.0.0";
            let (logger, log_inspector) = TestLogger::memory();
            let mut client = setup_client(
                "http://whatever",
                vec![Version::parse(version).unwrap()],
                None,
            );
            client.logger = logger;
            let response = build_fake_response_with_header(MITHRIL_API_VERSION_HEADER, version);

            client.warn_if_api_version_mismatch(&response).await;

            assert!(!log_inspector.contains_log(API_VERSION_MISMATCH_WARNING_MESSAGE));
        }

        #[tokio::test]
        async fn test_no_warning_logged_when_aggregator_api_version_is_older() {
            let aggregator_version = "1.0.0";
            let client_version = "2.0.0";
            let (logger, log_inspector) = TestLogger::memory();
            let mut client = setup_client(
                "http://whatever",
                vec![Version::parse(client_version).unwrap()],
                None,
            );
            client.logger = logger;
            let response =
                build_fake_response_with_header(MITHRIL_API_VERSION_HEADER, aggregator_version);

            assert!(
                Version::parse(aggregator_version).unwrap()
                    < Version::parse(client_version).unwrap()
            );

            client.warn_if_api_version_mismatch(&response).await;

            assert!(!log_inspector.contains_log(API_VERSION_MISMATCH_WARNING_MESSAGE));
        }

        #[tokio::test]
        async fn test_does_not_log_or_fail_when_header_is_missing() {
            let (logger, log_inspector) = TestLogger::memory();
            let mut client = setup_client(
                "http://whatever",
                APIVersionProvider::compute_all_versions_sorted(),
                None,
            );
            client.logger = logger;
            let response =
                build_fake_response_with_header("NotMithrilAPIVersionHeader", "whatever");

            client.warn_if_api_version_mismatch(&response).await;

            assert!(!log_inspector.contains_log(API_VERSION_MISMATCH_WARNING_MESSAGE));
        }

        #[tokio::test]
        async fn test_does_not_log_or_fail_when_header_is_not_a_version() {
            let (logger, log_inspector) = TestLogger::memory();
            let mut client = setup_client(
                "http://whatever",
                APIVersionProvider::compute_all_versions_sorted(),
                None,
            );
            client.logger = logger;
            let response =
                build_fake_response_with_header(MITHRIL_API_VERSION_HEADER, "not_a_version");

            client.warn_if_api_version_mismatch(&response).await;

            assert!(!log_inspector.contains_log(API_VERSION_MISMATCH_WARNING_MESSAGE));
        }

        #[tokio::test]
        async fn test_logs_error_when_client_version_cannot_be_computed() {
            let (logger, log_inspector) = TestLogger::memory();
            let mut client = setup_client("http://whatever", vec![], None);
            client.logger = logger;
            let response = build_fake_response_with_header(MITHRIL_API_VERSION_HEADER, "1.0.0");

            client.warn_if_api_version_mismatch(&response).await;

            assert!(!log_inspector.contains_log(API_VERSION_MISMATCH_WARNING_MESSAGE));
        }

        #[tokio::test]
        async fn test_client_get_log_warning_if_api_version_mismatch() {
            let aggregator_version = "2.0.0";
            let client_version = "1.0.0";
            let (server, mut client) = setup_server_and_client();
            let (logger, log_inspector) = TestLogger::memory();
            client.api_versions =
                Arc::new(RwLock::new(vec![Version::parse(client_version).unwrap()]));
            client.logger = logger;
            server.mock(|_, then| {
                then.status(StatusCode::OK.as_u16())
                    .header(MITHRIL_API_VERSION_HEADER, aggregator_version);
            });

            assert!(
                Version::parse(aggregator_version).unwrap()
                    > Version::parse(client_version).unwrap()
            );

            client.get(Url::parse(&server.base_url()).unwrap()).await.unwrap();

            assert_api_version_warning_logged(&log_inspector, aggregator_version, client_version);
        }

        #[tokio::test]
        async fn test_client_post_log_warning_if_api_version_mismatch() {
            let aggregator_version = "2.0.0";
            let client_version = "1.0.0";
            let (server, mut client) = setup_server_and_client();
            let (logger, log_inspector) = TestLogger::memory();
            client.api_versions =
                Arc::new(RwLock::new(vec![Version::parse(client_version).unwrap()]));
            client.logger = logger;
            server.mock(|_, then| {
                then.status(StatusCode::OK.as_u16())
                    .header(MITHRIL_API_VERSION_HEADER, aggregator_version);
            });

            assert!(
                Version::parse(aggregator_version).unwrap()
                    > Version::parse(client_version).unwrap()
            );

            client
                .post(Url::parse(&server.base_url()).unwrap(), "whatever")
                .await
                .unwrap();

            assert_api_version_warning_logged(&log_inspector, aggregator_version, client_version);
        }
    }
}
