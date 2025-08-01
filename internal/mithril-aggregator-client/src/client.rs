use anyhow::{Context, anyhow};
use reqwest::{IntoUrl, Response, Url, header::HeaderMap};
use semver::Version;
use slog::{Logger, error, warn};
use std::time::Duration;

use mithril_common::MITHRIL_API_VERSION_HEADER;
use mithril_common::api_version::APIVersionProvider;

use crate::AggregatorClientResult;
use crate::builder::AggregatorClientBuilder;
use crate::error::AggregatorClientError;
use crate::query::{AggregatorQuery, QueryContext, QueryMethod};

const API_VERSION_MISMATCH_WARNING_MESSAGE: &str = "OpenAPI version may be incompatible, please update Mithril client library to the latest version.";

/// A client to send HTTP requests to a Mithril Aggregator
pub struct AggregatorClient {
    pub(super) aggregator_endpoint: Url,
    pub(super) api_version_provider: APIVersionProvider,
    pub(super) additional_headers: HeaderMap,
    pub(super) timeout_duration: Option<Duration>,
    pub(super) client: reqwest::Client,
    pub(super) logger: Logger,
}

impl AggregatorClient {
    /// Creates a [AggregatorClientBuilder] to configure a `AggregatorClient`.
    //
    // This is the same as `AggregatorClient::builder()`.
    pub fn builder<U: IntoUrl>(aggregator_url: U) -> AggregatorClientBuilder {
        AggregatorClientBuilder::new(aggregator_url)
    }

    /// Send the given query to the Mithril Aggregator
    pub async fn send<Q: AggregatorQuery>(&self, query: Q) -> AggregatorClientResult<Q::Response> {
        // Todo: error handling ? Reuse the version in `warn_if_api_version_mismatch` ?
        let current_api_version = self.api_version_provider.compute_current_version().unwrap();
        let mut request_builder = match Q::method() {
            QueryMethod::Get => self.client.get(self.join_aggregator_endpoint(&query.route())?),
            QueryMethod::Post => self.client.post(self.join_aggregator_endpoint(&query.route())?),
        }
        .headers(self.additional_headers.clone())
        .header(MITHRIL_API_VERSION_HEADER, current_api_version.to_string());

        if let Some(body) = query.body() {
            request_builder = request_builder.json(&body);
        }

        if let Some(timeout) = self.timeout_duration {
            request_builder = request_builder.timeout(timeout);
        }

        match request_builder.send().await {
            Ok(response) => {
                self.warn_if_api_version_mismatch(&response);

                let context = QueryContext {
                    response,
                    logger: self.logger.clone(),
                };
                query.handle_response(context).await
            }
            Err(err) => Err(AggregatorClientError::RemoteServerUnreachable(anyhow!(err))),
        }
    }

    fn join_aggregator_endpoint(&self, endpoint: &str) -> AggregatorClientResult<Url> {
        self.aggregator_endpoint
            .join(endpoint)
            .with_context(|| {
                format!(
                    "Invalid url when joining given endpoint, '{endpoint}', to aggregator url '{}'",
                    self.aggregator_endpoint
                )
            })
            .map_err(AggregatorClientError::InvalidEndpoint)
    }

    /// Check API version mismatch and log a warning if the aggregator's version is more recent.
    fn warn_if_api_version_mismatch(&self, response: &Response) {
        let remote_aggregator_version = response
            .headers()
            .get(MITHRIL_API_VERSION_HEADER)
            .and_then(|v| v.to_str().ok())
            .and_then(|s| Version::parse(s).ok());

        let client_version = self.api_version_provider.compute_current_version();

        match (remote_aggregator_version, client_version) {
            (Some(aggregator), Ok(client)) if client < aggregator => {
                warn!(self.logger, "{}", API_VERSION_MISMATCH_WARNING_MESSAGE;
                    "remote_aggregator_version" => %aggregator,
                    "caller_version" => %client,
                );
            }
            (Some(_), Err(error)) => {
                error!(
                    self.logger,
                    "Failed to compute the current API version";
                    "error" => error.to_string()
                );
            }
            _ => {}
        }
    }
}

#[cfg(test)]
mod tests {
    use http::StatusCode;

    use mithril_common::test::api_version_extensions::ApiVersionProviderTestExtension;

    use crate::test::{TestLogger, setup_server_and_client};

    use super::*;

    #[derive(Debug, Eq, PartialEq, serde::Deserialize)]
    struct TestResponse {
        foo: String,
        bar: i32,
    }

    struct TestGetQuery;

    #[async_trait::async_trait]
    impl AggregatorQuery for TestGetQuery {
        type Response = TestResponse;
        type Body = ();

        fn method() -> QueryMethod {
            QueryMethod::Get
        }

        fn route(&self) -> String {
            "/dummy-get-route".to_string()
        }

        async fn handle_response(
            &self,
            context: QueryContext,
        ) -> AggregatorClientResult<Self::Response> {
            match context.response.status() {
                StatusCode::OK => context
                    .response
                    .json::<TestResponse>()
                    .await
                    .map_err(|err| AggregatorClientError::JsonParseFailed(anyhow!(err))),
                _ => Err(context.unhandled_status_code().await),
            }
        }
    }

    #[derive(Debug, Clone, Eq, PartialEq, serde::Serialize)]
    struct TestBody {
        pika: String,
        chu: u8,
    }

    impl TestBody {
        fn new<P: Into<String>>(pika: P, chu: u8) -> Self {
            Self {
                pika: pika.into(),
                chu,
            }
        }
    }

    struct TestPostQuery {
        body: TestBody,
    }

    #[async_trait::async_trait]
    impl AggregatorQuery for TestPostQuery {
        type Response = ();
        type Body = TestBody;

        fn method() -> QueryMethod {
            QueryMethod::Post
        }

        fn route(&self) -> String {
            "/dummy-post-route".to_string()
        }

        fn body(&self) -> Option<Self::Body> {
            Some(self.body.clone())
        }

        async fn handle_response(
            &self,
            context: QueryContext,
        ) -> AggregatorClientResult<Self::Response> {
            match context.response.status() {
                StatusCode::CREATED => Ok(()),
                _ => Err(context.unhandled_status_code().await),
            }
        }
    }

    #[tokio::test]
    async fn test_minimal_get_query() {
        let (server, client) = setup_server_and_client();
        server.mock(|when, then| {
            when.method(httpmock::Method::GET).path("/dummy-get-route");
            then.status(200).body(r#"{"foo": "bar", "bar": 123}"#);
        });

        let response = client.send(TestGetQuery).await.unwrap();

        assert_eq!(
            response,
            TestResponse {
                foo: "bar".to_string(),
                bar: 123,
            }
        )
    }

    #[tokio::test]
    async fn test_minimal_post_query() {
        let (server, client) = setup_server_and_client();
        server.mock(|when, then| {
            when.method(httpmock::Method::POST)
                .path("/dummy-post-route")
                .header("content-type", "application/json")
                .body(serde_json::to_string(&TestBody::new("miaouss", 5)).unwrap());
            then.status(201);
        });

        client
            .send(TestPostQuery {
                body: TestBody::new("miaouss", 5),
            })
            .await
            .unwrap();
    }

    #[tokio::test]
    async fn test_query_send_mithril_api_version_header() {
        let (server, mut client) = setup_server_and_client();
        client.api_version_provider =
            APIVersionProvider::new_with_default_version(Version::parse("1.2.9").unwrap());
        server.mock(|when, then| {
            when.method(httpmock::Method::GET)
                .header(MITHRIL_API_VERSION_HEADER, "1.2.9");
            then.status(200).body(r#"{"foo": "a", "bar": 1}"#);
        });

        client.send(TestGetQuery).await.expect("should not fail");
    }

    #[tokio::test]
    async fn test_query_send_additional_header_and_dont_override_mithril_api_version_header() {
        let (server, mut client) = setup_server_and_client();
        client.api_version_provider =
            APIVersionProvider::new_with_default_version(Version::parse("1.2.9").unwrap());
        client.additional_headers = {
            let mut headers = HeaderMap::new();
            headers.insert(MITHRIL_API_VERSION_HEADER, "9.4.5".parse().unwrap());
            headers.insert("foo", "bar".parse().unwrap());
            headers
        };

        server.mock(|when, then| {
            when.method(httpmock::Method::POST)
                .header(MITHRIL_API_VERSION_HEADER, "1.2.9")
                .header("foo", "bar");
            then.status(201).body(r#"{"foo": "a", "bar": 1}"#);
        });

        client
            .send(TestPostQuery {
                body: TestBody::new("miaouss", 3),
            })
            .await
            .expect("should not fail");
    }

    #[tokio::test]
    async fn test_query_timeout() {
        let (server, mut client) = setup_server_and_client();
        client.timeout_duration = Some(Duration::from_millis(10));
        let _server_mock = server.mock(|when, then| {
            when.method(httpmock::Method::GET);
            then.delay(Duration::from_millis(100));
        });

        let error = client.send(TestGetQuery).await.expect_err("should not fail");

        assert!(
            matches!(error, AggregatorClientError::RemoteServerUnreachable(_)),
            "unexpected error type: {error:?}"
        );
    }

    mod warn_if_api_version_mismatch {
        use http::response::Builder as HttpResponseBuilder;
        use reqwest::Response;

        use mithril_common::test::logging::MemoryDrainForTestInspector;

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
            assert!(log_inspector.contains_log(&format!(
                "remote_aggregator_version={}",
                aggregator_version.into()
            )));
            assert!(
                log_inspector.contains_log(&format!("caller_version={}", client_version.into()))
            );
        }

        #[test]
        fn test_logs_warning_when_aggregator_api_version_is_newer() {
            let aggregator_version = "2.0.0";
            let client_version = "1.0.0";
            let (logger, log_inspector) = TestLogger::memory();
            let client = AggregatorClient::builder("http://whatever")
                .with_logger(logger)
                .with_api_version_provider(APIVersionProvider::new_with_default_version(
                    Version::parse(client_version).unwrap(),
                ))
                .build()
                .unwrap();
            let response =
                build_fake_response_with_header(MITHRIL_API_VERSION_HEADER, aggregator_version);

            assert!(
                Version::parse(aggregator_version).unwrap()
                    > Version::parse(client_version).unwrap()
            );

            client.warn_if_api_version_mismatch(&response);

            assert_api_version_warning_logged(&log_inspector, aggregator_version, client_version);
        }

        #[test]
        fn test_no_warning_logged_when_versions_match() {
            let version = "1.0.0";
            let (logger, log_inspector) = TestLogger::memory();
            let client = AggregatorClient::builder("http://whatever")
                .with_logger(logger)
                .with_api_version_provider(APIVersionProvider::new_with_default_version(
                    Version::parse(version).unwrap(),
                ))
                .build()
                .unwrap();
            let response = build_fake_response_with_header(MITHRIL_API_VERSION_HEADER, version);

            client.warn_if_api_version_mismatch(&response);

            assert!(!log_inspector.contains_log(API_VERSION_MISMATCH_WARNING_MESSAGE));
        }

        #[test]
        fn test_no_warning_logged_when_aggregator_api_version_is_older() {
            let aggregator_version = "1.0.0";
            let client_version = "2.0.0";
            let (logger, log_inspector) = TestLogger::memory();
            let client = AggregatorClient::builder("http://whatever")
                .with_logger(logger)
                .with_api_version_provider(APIVersionProvider::new_with_default_version(
                    Version::parse(client_version).unwrap(),
                ))
                .build()
                .unwrap();
            let response =
                build_fake_response_with_header(MITHRIL_API_VERSION_HEADER, aggregator_version);

            assert!(
                Version::parse(aggregator_version).unwrap()
                    < Version::parse(client_version).unwrap()
            );

            client.warn_if_api_version_mismatch(&response);

            assert!(!log_inspector.contains_log(API_VERSION_MISMATCH_WARNING_MESSAGE));
        }

        #[test]
        fn test_does_not_log_or_fail_when_header_is_missing() {
            let (logger, log_inspector) = TestLogger::memory();
            let client = AggregatorClient::builder("http://whatever")
                .with_logger(logger)
                .with_api_version_provider(APIVersionProvider::default())
                .build()
                .unwrap();
            let response =
                build_fake_response_with_header("NotMithrilAPIVersionHeader", "whatever");

            client.warn_if_api_version_mismatch(&response);

            assert!(!log_inspector.contains_log(API_VERSION_MISMATCH_WARNING_MESSAGE));
        }

        #[test]
        fn test_does_not_log_or_fail_when_header_is_not_a_version() {
            let (logger, log_inspector) = TestLogger::memory();
            let client = AggregatorClient::builder("http://whatever")
                .with_logger(logger)
                .with_api_version_provider(APIVersionProvider::default())
                .build()
                .unwrap();
            let response =
                build_fake_response_with_header(MITHRIL_API_VERSION_HEADER, "not_a_version");

            client.warn_if_api_version_mismatch(&response);

            assert!(!log_inspector.contains_log(API_VERSION_MISMATCH_WARNING_MESSAGE));
        }

        #[test]
        fn test_logs_error_when_client_version_cannot_be_computed() {
            let (logger, log_inspector) = TestLogger::memory();
            let client = AggregatorClient::builder("http://whatever")
                .with_logger(logger)
                .with_api_version_provider(APIVersionProvider::new_failing())
                .build()
                .unwrap();
            let response = build_fake_response_with_header(MITHRIL_API_VERSION_HEADER, "1.0.0");

            client.warn_if_api_version_mismatch(&response);

            assert!(!log_inspector.contains_log(API_VERSION_MISMATCH_WARNING_MESSAGE));
        }

        #[tokio::test]
        async fn test_client_log_warning_if_api_version_mismatch() {
            let aggregator_version = "2.0.0";
            let client_version = "1.0.0";
            let (server, mut client) = setup_server_and_client();
            let (logger, log_inspector) = TestLogger::memory();
            client.api_version_provider = APIVersionProvider::new_with_default_version(
                Version::parse(client_version).unwrap(),
            );
            client.logger = logger;
            server.mock(|_, then| {
                then.status(StatusCode::CREATED.as_u16())
                    .header(MITHRIL_API_VERSION_HEADER, aggregator_version);
            });

            assert!(
                Version::parse(aggregator_version).unwrap()
                    > Version::parse(client_version).unwrap()
            );

            client
                .send(TestPostQuery {
                    body: TestBody::new("miaouss", 3),
                })
                .await
                .unwrap();

            assert_api_version_warning_logged(&log_inspector, aggregator_version, client_version);
        }
    }
}
