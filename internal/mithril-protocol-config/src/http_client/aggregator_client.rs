use anyhow::anyhow;
use async_trait::async_trait;
use reqwest::header::{self, HeaderValue};
use reqwest::{self, Client, Proxy, RequestBuilder, Response, StatusCode};
use semver::Version;
use slog::{Logger, debug, error, warn};
use std::{io, sync::Arc, time::Duration};
use thiserror::Error;

use mithril_common::{
    MITHRIL_API_VERSION_HEADER, MITHRIL_SIGNER_VERSION_HEADER, StdError,
    api_version::APIVersionProvider,
    entities::{ClientError, ServerError},
    logging::LoggerExtensions,
    messages::{AggregatorFeaturesMessage, EpochSettingsMessage},
};

const JSON_CONTENT_TYPE: HeaderValue = HeaderValue::from_static("application/json");

const API_VERSION_MISMATCH_WARNING_MESSAGE: &str =
    "OpenAPI version may be incompatible, please update your Mithril node to the latest version.";

/// Error structure for the Aggregator Client.
#[derive(Error, Debug)]
pub enum AggregatorClientError {
    /// The aggregator host has returned a technical error.
    #[error("remote server technical error")]
    RemoteServerTechnical(#[source] StdError),

    /// The aggregator host responded it cannot fulfill our request.
    #[error("remote server logical error")]
    RemoteServerLogical(#[source] StdError),

    /// Could not reach aggregator.
    #[error("remote server unreachable")]
    RemoteServerUnreachable(#[source] StdError),

    /// Unhandled status code
    #[error("unhandled status code: {0}, response text: {1}")]
    UnhandledStatusCode(StatusCode, String),

    /// Could not parse response.
    #[error("json parsing failed")]
    JsonParseFailed(#[source] StdError),

    /// Mostly network errors.
    #[error("Input/Output error")]
    IOError(#[from] io::Error),

    /// HTTP client creation error
    #[error("HTTP client creation failed")]
    HTTPClientCreation(#[source] StdError),

    /// Proxy creation error
    #[error("proxy creation failed")]
    ProxyCreation(#[source] StdError),
}

impl AggregatorClientError {
    /// Create an `AggregatorClientError` from a response.
    ///
    /// This method is meant to be used after handling domain-specific cases leaving only
    /// 4xx or 5xx status codes.
    /// Otherwise, it will return an `UnhandledStatusCode` error.
    pub async fn from_response(response: Response) -> Self {
        let error_code = response.status();

        if error_code.is_client_error() {
            let root_cause = Self::get_root_cause(response).await;
            Self::RemoteServerLogical(anyhow!(root_cause))
        } else if error_code.is_server_error() {
            let root_cause = Self::get_root_cause(response).await;
            Self::RemoteServerTechnical(anyhow!(root_cause))
        } else {
            let response_text = response.text().await.unwrap_or_default();
            Self::UnhandledStatusCode(error_code, response_text)
        }
    }

    async fn get_root_cause(response: Response) -> String {
        let error_code = response.status();
        let canonical_reason = error_code.canonical_reason().unwrap_or_default().to_lowercase();
        let is_json = response
            .headers()
            .get(header::CONTENT_TYPE)
            .is_some_and(|ct| JSON_CONTENT_TYPE == ct);

        if is_json {
            let json_value: serde_json::Value = response.json().await.unwrap_or_default();

            if let Ok(client_error) = serde_json::from_value::<ClientError>(json_value.clone()) {
                format!(
                    "{}: {}: {}",
                    canonical_reason, client_error.label, client_error.message
                )
            } else if let Ok(server_error) =
                serde_json::from_value::<ServerError>(json_value.clone())
            {
                format!("{}: {}", canonical_reason, server_error.message)
            } else if json_value.is_null() {
                canonical_reason.to_string()
            } else {
                format!("{canonical_reason}: {json_value}")
            }
        } else {
            let response_text = response.text().await.unwrap_or_default();
            format!("{canonical_reason}: {response_text}")
        }
    }
}

/// Trait for mocking and testing a `AggregatorClient`
#[cfg_attr(test, mockall::automock)]
#[async_trait]
pub trait AggregatorClient: Sync + Send {
    /// Retrieves epoch settings from the aggregator
    async fn retrieve_epoch_settings(
        &self,
    ) -> Result<Option<EpochSettingsMessage>, AggregatorClientError>;

    /// Retrieves aggregator features message from the aggregator
    async fn retrieve_aggregator_features(
        &self,
    ) -> Result<AggregatorFeaturesMessage, AggregatorClientError>;
}

/// AggregatorHTTPClient is a http client for an aggregator
pub struct AggregatorHTTPClient {
    aggregator_endpoint: String,
    relay_endpoint: Option<String>,
    api_version_provider: Arc<APIVersionProvider>,
    timeout_duration: Option<Duration>,
    logger: Logger,
}

impl AggregatorHTTPClient {
    /// AggregatorHTTPClient factory
    pub fn new(
        aggregator_endpoint: String,
        relay_endpoint: Option<String>,
        api_version_provider: Arc<APIVersionProvider>,
        timeout_duration: Option<Duration>,
        logger: Logger,
    ) -> Self {
        let logger = logger.new_with_component_name::<Self>();
        debug!(logger, "New AggregatorHTTPClient created");
        Self {
            aggregator_endpoint,
            relay_endpoint,
            api_version_provider,
            timeout_duration,
            logger,
        }
    }

    fn prepare_http_client(&self) -> Result<Client, AggregatorClientError> {
        let client = match &self.relay_endpoint {
            Some(relay_endpoint) => Client::builder()
                .proxy(
                    Proxy::all(relay_endpoint)
                        .map_err(|e| AggregatorClientError::ProxyCreation(anyhow!(e)))?,
                )
                .build()
                .map_err(|e| AggregatorClientError::HTTPClientCreation(anyhow!(e)))?,
            None => Client::new(),
        };

        Ok(client)
    }

    /// Forge a client request adding protocol version in the headers.
    pub fn prepare_request_builder(&self, request_builder: RequestBuilder) -> RequestBuilder {
        let request_builder = request_builder
            .header(
                MITHRIL_API_VERSION_HEADER,
                self.api_version_provider
                    .compute_current_version()
                    .unwrap()
                    .to_string(),
            )
            .header(MITHRIL_SIGNER_VERSION_HEADER, env!("CARGO_PKG_VERSION"));

        if let Some(duration) = self.timeout_duration {
            request_builder.timeout(duration)
        } else {
            request_builder
        }
    }

    /// Check API version mismatch and log a warning if the aggregator's version is more recent.
    fn warn_if_api_version_mismatch(&self, response: &Response) {
        let aggregator_version = response
            .headers()
            .get(MITHRIL_API_VERSION_HEADER)
            .and_then(|v| v.to_str().ok())
            .and_then(|s| Version::parse(s).ok());

        let signer_version = self.api_version_provider.compute_current_version();

        match (aggregator_version, signer_version) {
            (Some(aggregator), Ok(signer)) if signer < aggregator => {
                warn!(self.logger, "{}", API_VERSION_MISMATCH_WARNING_MESSAGE;
                    "aggregator_version" => %aggregator,
                    "signer_version" => %signer,
                );
            }
            (Some(_), Err(error)) => {
                error!(
                    self.logger,
                    "Failed to compute the current signer API version";
                    "error" => error.to_string()
                );
            }
            _ => {}
        }
    }
}

#[async_trait]
impl AggregatorClient for AggregatorHTTPClient {
    async fn retrieve_epoch_settings(
        &self,
    ) -> Result<Option<EpochSettingsMessage>, AggregatorClientError> {
        debug!(self.logger, "Retrieve epoch settings");
        let url = format!("{}/epoch-settings", self.aggregator_endpoint);
        let response = self
            .prepare_request_builder(self.prepare_http_client()?.get(url.clone()))
            .send()
            .await;

        match response {
            Ok(response) => match response.status() {
                StatusCode::OK => {
                    self.warn_if_api_version_mismatch(&response);
                    match response.json::<EpochSettingsMessage>().await {
                        Ok(message) => Ok(Some(message)),
                        Err(err) => Err(AggregatorClientError::JsonParseFailed(anyhow!(err))),
                    }
                }
                _ => Err(AggregatorClientError::from_response(response).await),
            },
            Err(err) => Err(AggregatorClientError::RemoteServerUnreachable(anyhow!(err))),
        }
    }

    async fn retrieve_aggregator_features(
        &self,
    ) -> Result<AggregatorFeaturesMessage, AggregatorClientError> {
        debug!(self.logger, "Retrieve aggregator features message");
        let url = format!("{}/", self.aggregator_endpoint);
        let response = self
            .prepare_request_builder(self.prepare_http_client()?.get(url.clone()))
            .send()
            .await;

        match response {
            Ok(response) => match response.status() {
                StatusCode::OK => {
                    self.warn_if_api_version_mismatch(&response);

                    Ok(response
                        .json::<AggregatorFeaturesMessage>()
                        .await
                        .map_err(|e| AggregatorClientError::JsonParseFailed(anyhow!(e)))?)
                }
                _ => Err(AggregatorClientError::from_response(response).await),
            },
            Err(err) => Err(AggregatorClientError::RemoteServerUnreachable(anyhow!(err))),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use http::response::Builder as HttpResponseBuilder;
    use httpmock::prelude::*;
    use semver::Version;
    use serde_json::json;

    use mithril_common::test::{
        double::{Dummy, DummyApiVersionDiscriminantSource},
        logging::MemoryDrainForTestInspector,
    };

    use crate::test_tools::TestLogger;

    use super::*;

    macro_rules! assert_is_error {
        ($error:expr, $error_type:pat) => {
            assert!(
                matches!($error, $error_type),
                "Expected {} error, got '{:?}'.",
                stringify!($error_type),
                $error
            );
        };
    }

    fn setup_client<U: Into<String>>(server_url: U) -> AggregatorHTTPClient {
        let discriminant_source = DummyApiVersionDiscriminantSource::new("dummy");
        let api_version_provider = APIVersionProvider::new(Arc::new(discriminant_source));

        AggregatorHTTPClient::new(
            server_url.into(),
            None,
            Arc::new(api_version_provider),
            None,
            TestLogger::stdout(),
        )
    }

    fn setup_server_and_client() -> (MockServer, AggregatorHTTPClient) {
        let server = MockServer::start();
        let aggregator_endpoint = server.url("");
        let client = setup_client(&aggregator_endpoint);

        (server, client)
    }

    fn set_returning_500(server: &MockServer) {
        server.mock(|_, then| {
            then.status(500).body("an error occurred");
        });
    }

    fn set_unparsable_json(server: &MockServer) {
        server.mock(|_, then| {
            then.status(200).body("this is not a json");
        });
    }

    fn build_text_response<T: Into<String>>(status_code: StatusCode, body: T) -> Response {
        HttpResponseBuilder::new()
            .status(status_code)
            .body(body.into())
            .unwrap()
            .into()
    }

    fn build_json_response<T: serde::Serialize>(status_code: StatusCode, body: &T) -> Response {
        HttpResponseBuilder::new()
            .status(status_code)
            .header(header::CONTENT_TYPE, JSON_CONTENT_TYPE)
            .body(serde_json::to_string(&body).unwrap())
            .unwrap()
            .into()
    }

    macro_rules! assert_error_text_contains {
        ($error: expr, $expect_contains: expr) => {
            let error = &$error;
            assert!(
                error.contains($expect_contains),
                "Expected error message to contain '{}'\ngot '{error:?}'",
                $expect_contains,
            );
        };
    }

    mod epoch_settings {
        use super::*;

        #[tokio::test]
        async fn test_epoch_settings_ok_200() {
            let (server, client) = setup_server_and_client();
            let epoch_settings_expected = EpochSettingsMessage::dummy();
            let _server_mock = server.mock(|when, then| {
                when.path("/epoch-settings");
                then.status(200).body(json!(epoch_settings_expected).to_string());
            });

            let epoch_settings = client.retrieve_epoch_settings().await;
            epoch_settings.as_ref().expect("unexpected error");
            assert_eq!(epoch_settings_expected, epoch_settings.unwrap().unwrap());
        }

        #[tokio::test]
        async fn test_epoch_settings_ko_500() {
            let (server, client) = setup_server_and_client();
            let _server_mock = server.mock(|when, then| {
                when.path("/epoch-settings");
                then.status(500).body("an error occurred");
            });

            match client.retrieve_epoch_settings().await.unwrap_err() {
                AggregatorClientError::RemoteServerTechnical(_) => (),
                e => panic!("Expected Aggregator::RemoteServerTechnical error, got '{e:?}'."),
            };
        }

        #[tokio::test]
        async fn test_epoch_settings_timeout() {
            let (server, mut client) = setup_server_and_client();
            client.timeout_duration = Some(Duration::from_millis(10));
            let _server_mock = server.mock(|when, then| {
                when.path("/epoch-settings");
                then.delay(Duration::from_millis(100));
            });

            let error = client
                .retrieve_epoch_settings()
                .await
                .expect_err("retrieve_epoch_settings should fail");

            assert!(
                matches!(error, AggregatorClientError::RemoteServerUnreachable(_)),
                "unexpected error type: {error:?}"
            );
        }
    }

    mod aggregator_features {
        use super::*;

        #[tokio::test]
        async fn test_aggregator_features_ok_200() {
            let (server, client) = setup_server_and_client();
            let message_expected = AggregatorFeaturesMessage::dummy();
            let _server_mock = server.mock(|when, then| {
                when.path("/");
                then.status(200).body(json!(message_expected).to_string());
            });

            let message = client.retrieve_aggregator_features().await.unwrap();

            assert_eq!(message_expected, message);
        }

        #[tokio::test]
        async fn test_aggregator_features_ko_500() {
            let (server, client) = setup_server_and_client();
            set_returning_500(&server);

            let error = client.retrieve_aggregator_features().await.unwrap_err();

            assert_is_error!(error, AggregatorClientError::RemoteServerTechnical(_));
        }

        #[tokio::test]
        async fn test_aggregator_features_ko_json_serialization() {
            let (server, client) = setup_server_and_client();
            set_unparsable_json(&server);

            let error = client.retrieve_aggregator_features().await.unwrap_err();

            assert_is_error!(error, AggregatorClientError::JsonParseFailed(_));
        }

        #[tokio::test]
        async fn test_aggregator_features_timeout() {
            let (server, mut client) = setup_server_and_client();
            client.timeout_duration = Some(Duration::from_millis(10));
            let _server_mock = server.mock(|when, then| {
                when.path("/");
                then.delay(Duration::from_millis(100));
            });

            let error = client.retrieve_aggregator_features().await.unwrap_err();

            assert_is_error!(error, AggregatorClientError::RemoteServerUnreachable(_));
        }
    }

    #[tokio::test]
    async fn test_4xx_errors_are_handled_as_remote_server_logical() {
        let response = build_text_response(StatusCode::BAD_REQUEST, "error text");
        let handled_error = AggregatorClientError::from_response(response).await;

        assert!(
            matches!(
                handled_error,
                AggregatorClientError::RemoteServerLogical(..)
            ),
            "Expected error to be RemoteServerLogical\ngot '{handled_error:?}'",
        );
    }

    #[tokio::test]
    async fn test_5xx_errors_are_handled_as_remote_server_technical() {
        let response = build_text_response(StatusCode::INTERNAL_SERVER_ERROR, "error text");
        let handled_error = AggregatorClientError::from_response(response).await;

        assert!(
            matches!(
                handled_error,
                AggregatorClientError::RemoteServerTechnical(..)
            ),
            "Expected error to be RemoteServerLogical\ngot '{handled_error:?}'",
        );
    }

    #[tokio::test]
    async fn test_non_4xx_or_5xx_errors_are_handled_as_unhandled_status_code_and_contains_response_text()
     {
        let response = build_text_response(StatusCode::OK, "ok text");
        let handled_error = AggregatorClientError::from_response(response).await;

        assert!(
            matches!(
                handled_error,
                AggregatorClientError::UnhandledStatusCode(..) if format!("{handled_error:?}").contains("ok text")
            ),
            "Expected error to be UnhandledStatusCode with 'ok text' in error text\ngot '{handled_error:?}'",
        );
    }

    #[tokio::test]
    async fn test_root_cause_of_non_json_response_contains_response_plain_text() {
        let error_text = "An error occurred; please try again later.";
        let response = build_text_response(StatusCode::EXPECTATION_FAILED, error_text);

        assert_error_text_contains!(
            AggregatorClientError::get_root_cause(response).await,
            "expectation failed: An error occurred; please try again later."
        );
    }

    #[tokio::test]
    async fn test_root_cause_of_json_formatted_client_error_response_contains_error_label_and_message()
     {
        let client_error = ClientError::new("label", "message");
        let response = build_json_response(StatusCode::BAD_REQUEST, &client_error);

        assert_error_text_contains!(
            AggregatorClientError::get_root_cause(response).await,
            "bad request: label: message"
        );
    }

    #[tokio::test]
    async fn test_root_cause_of_json_formatted_server_error_response_contains_error_label_and_message()
     {
        let server_error = ServerError::new("message");
        let response = build_json_response(StatusCode::BAD_REQUEST, &server_error);

        assert_error_text_contains!(
            AggregatorClientError::get_root_cause(response).await,
            "bad request: message"
        );
    }

    #[tokio::test]
    async fn test_root_cause_of_unknown_formatted_json_response_contains_json_key_value_pairs() {
        let response = build_json_response(
            StatusCode::INTERNAL_SERVER_ERROR,
            &json!({ "second": "unknown", "first": "foreign" }),
        );

        assert_error_text_contains!(
            AggregatorClientError::get_root_cause(response).await,
            r#"internal server error: {"first":"foreign","second":"unknown"}"#
        );
    }

    #[tokio::test]
    async fn test_root_cause_with_invalid_json_response_still_contains_response_status_name() {
        let response = HttpResponseBuilder::new()
            .status(StatusCode::BAD_REQUEST)
            .header(header::CONTENT_TYPE, JSON_CONTENT_TYPE)
            .body(r#"{"invalid":"unexpected dot", "key": "value".}"#)
            .unwrap()
            .into();

        let root_cause = AggregatorClientError::get_root_cause(response).await;

        assert_error_text_contains!(root_cause, "bad request");
        assert!(
            !root_cause.contains("bad request: "),
            "Expected error message should not contain additional information \ngot '{root_cause:?}'"
        );
    }

    mod warn_if_api_version_mismatch {
        use mithril_common::test::api_version_extensions::ApiVersionProviderTestExtension;

        use super::*;

        fn version_provider_with_open_api_version<V: Into<String>>(
            version: V,
        ) -> APIVersionProvider {
            let mut version_provider = version_provider_without_open_api_version();
            let mut open_api_versions = HashMap::new();
            open_api_versions.insert(
                "openapi.yaml".to_string(),
                Version::parse(&version.into()).unwrap(),
            );
            version_provider.update_open_api_versions(open_api_versions);

            version_provider
        }

        fn version_provider_without_open_api_version() -> APIVersionProvider {
            let mut version_provider =
                APIVersionProvider::new(Arc::new(DummyApiVersionDiscriminantSource::new("dummy")));
            version_provider.update_open_api_versions(HashMap::new());

            version_provider
        }

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
            signer_version: S,
        ) {
            assert!(log_inspector.contains_log(API_VERSION_MISMATCH_WARNING_MESSAGE));
            assert!(
                log_inspector
                    .contains_log(&format!("aggregator_version={}", aggregator_version.into()))
            );
            assert!(
                log_inspector.contains_log(&format!("signer_version={}", signer_version.into()))
            );
        }

        #[test]
        fn test_logs_warning_when_aggregator_api_version_is_newer() {
            let aggregator_version = "2.0.0";
            let signer_version = "1.0.0";
            let (logger, log_inspector) = TestLogger::memory();
            let version_provider = version_provider_with_open_api_version(signer_version);
            let mut client = setup_client("whatever");
            client.api_version_provider = Arc::new(version_provider);
            client.logger = logger;
            let response =
                build_fake_response_with_header(MITHRIL_API_VERSION_HEADER, aggregator_version);

            assert!(
                Version::parse(aggregator_version).unwrap()
                    > Version::parse(signer_version).unwrap()
            );

            client.warn_if_api_version_mismatch(&response);

            assert_api_version_warning_logged(&log_inspector, aggregator_version, signer_version);
        }

        #[test]
        fn test_no_warning_logged_when_versions_match() {
            let version = "1.0.0";
            let (logger, log_inspector) = TestLogger::memory();
            let version_provider = version_provider_with_open_api_version(version);
            let mut client = setup_client("whatever");
            client.api_version_provider = Arc::new(version_provider);
            client.logger = logger;
            let response = build_fake_response_with_header(MITHRIL_API_VERSION_HEADER, version);

            client.warn_if_api_version_mismatch(&response);

            assert!(!log_inspector.contains_log(API_VERSION_MISMATCH_WARNING_MESSAGE));
        }

        #[test]
        fn test_no_warning_logged_when_aggregator_api_version_is_older() {
            let aggregator_version = "1.0.0";
            let signer_version = "2.0.0";
            let (logger, log_inspector) = TestLogger::memory();
            let version_provider = version_provider_with_open_api_version(signer_version);
            let mut client = setup_client("whatever");
            client.api_version_provider = Arc::new(version_provider);
            client.logger = logger;
            let response =
                build_fake_response_with_header(MITHRIL_API_VERSION_HEADER, aggregator_version);

            assert!(
                Version::parse(aggregator_version).unwrap()
                    < Version::parse(signer_version).unwrap()
            );

            client.warn_if_api_version_mismatch(&response);

            assert!(!log_inspector.contains_log(API_VERSION_MISMATCH_WARNING_MESSAGE));
        }

        #[test]
        fn test_does_not_log_or_fail_when_header_is_missing() {
            let (logger, log_inspector) = TestLogger::memory();
            let mut client = setup_client("whatever");
            client.logger = logger;
            let response =
                build_fake_response_with_header("NotMithrilAPIVersionHeader", "whatever");

            client.warn_if_api_version_mismatch(&response);

            assert!(!log_inspector.contains_log(API_VERSION_MISMATCH_WARNING_MESSAGE));
        }

        #[test]
        fn test_does_not_log_or_fail_when_header_is_not_a_version() {
            let (logger, log_inspector) = TestLogger::memory();
            let mut client = setup_client("whatever");
            client.logger = logger;
            let response =
                build_fake_response_with_header(MITHRIL_API_VERSION_HEADER, "not_a_version");

            client.warn_if_api_version_mismatch(&response);

            assert!(!log_inspector.contains_log(API_VERSION_MISMATCH_WARNING_MESSAGE));
        }

        #[test]
        fn test_logs_error_when_signer_version_cannot_be_computed() {
            let (logger, log_inspector) = TestLogger::memory();
            let version_provider = version_provider_without_open_api_version();
            let mut client = setup_client("whatever");
            client.api_version_provider = Arc::new(version_provider);
            client.logger = logger;
            let response = build_fake_response_with_header(MITHRIL_API_VERSION_HEADER, "1.0.0");

            client.warn_if_api_version_mismatch(&response);

            assert!(!log_inspector.contains_log(API_VERSION_MISMATCH_WARNING_MESSAGE));
        }

        #[tokio::test]
        async fn test_epoch_settings_ok_200_log_warning_if_api_version_mismatch() {
            let aggregator_version = "2.0.0";
            let signer_version = "1.0.0";
            let (server, mut client) = setup_server_and_client();
            let (logger, log_inspector) = TestLogger::memory();
            let version_provider = version_provider_with_open_api_version(signer_version);
            client.api_version_provider = Arc::new(version_provider);
            client.logger = logger;

            let epoch_settings_expected = EpochSettingsMessage::dummy();
            let _server_mock = server.mock(|when, then| {
                when.path("/epoch-settings");
                then.status(200)
                    .header(MITHRIL_API_VERSION_HEADER, aggregator_version)
                    .body(json!(epoch_settings_expected).to_string());
            });

            assert!(
                Version::parse(aggregator_version).unwrap()
                    > Version::parse(signer_version).unwrap()
            );

            client.retrieve_epoch_settings().await.unwrap();

            assert_api_version_warning_logged(&log_inspector, aggregator_version, signer_version);
        }
    }
}
