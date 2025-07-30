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

    use crate::test::TestLogger;

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
            "/dummy-route".to_string()
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
    async fn test_minimal_query() {
        let server = MockServer::start();
        server.mock(|when, then| {
            when.method(httpmock::Method::GET).path(TestGetQuery.route());
            then.status(200).body(r#"{"foo": "bar", "bar": 123}"#);
        });

        let aggregator_endpoint = Url::parse(&server.url("/")).unwrap();
        let client = AggregatorClient {
            aggregator_endpoint,
            client: reqwest::Client::new(),
            logger: TestLogger::stdout(),
        };

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
