use anyhow::{Context, anyhow};
use reqwest::{IntoUrl, Url};

use slog::Logger;

use crate::AggregatorClientResult;
use crate::builder::AggregatorClientBuilder;
use crate::error::AggregatorClientError;
use crate::query::{AggregatorQuery, QueryContext, QueryMethod};

pub struct AggregatorClient {
    pub(super) aggregator_endpoint: Url,
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

    pub async fn send<Q: AggregatorQuery>(&self, query: Q) -> AggregatorClientResult<Q::Response> {
        let mut request_builder = match Q::method() {
            QueryMethod::Get => self.client.get(self.join_aggregator_endpoint(&query.route())?),
            QueryMethod::Post => self.client.post(self.join_aggregator_endpoint(&query.route())?),
        };

        if let Some(body) = query.body() {
            request_builder = request_builder.json(&body);
        }

        match request_builder.send().await {
            Ok(response) => {
                // should we always warn?
                if !response.status().is_server_error() {
                    // todo: import code
                    // self.warn_if_api_version_mismatch(&response);
                }

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
}

#[cfg(test)]
mod tests {
    use http::StatusCode;
    use httpmock::MockServer;

    use mithril_common::test::api_version_extensions::ApiVersionProviderTestExtension;

    use crate::test::TestLogger;

    use super::*;

    fn setup_server_and_client() -> (MockServer, AggregatorClient) {
        let server = MockServer::start();
        let client = AggregatorClient::builder(server.base_url())
            .with_logger(TestLogger::stdout())
            .build()
            .unwrap();

        (server, client)
    }

    mod get {
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
    }

    mod post {
        use super::*;

        #[derive(Debug, Clone, Eq, PartialEq, serde::Serialize)]
        struct TestBody {
            pika: String,
            chu: u8,
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
        async fn test_minimal_post_query() {
            let (server, client) = setup_server_and_client();
            server.mock(|when, then| {
                when.method(httpmock::Method::POST)
                    .path("/dummy-post-route")
                    .header("content-type", "application/json")
                    .body(
                        serde_json::to_string(&TestBody {
                            pika: "miaouss".to_string(),
                            chu: 5,
                        })
                        .unwrap(),
                    );
                then.status(201);
            });

            let response = client
                .send(TestPostQuery {
                    body: TestBody {
                        pika: "miaouss".to_string(),
                        chu: 5,
                    },
                })
                .await
                .unwrap();

            assert_eq!(response, ())
        }
    }
}
