use anyhow::anyhow;
use async_trait::async_trait;
use reqwest::StatusCode;
use slog::debug;

use mithril_common::messages::AggregatorFeaturesMessage;

use crate::query::{AggregatorQuery, QueryContext, QueryMethod};
use crate::{AggregatorHttpClientError, AggregatorHttpClientResult};

/// Query to get the features of the aggregator
pub struct GetAggregatorFeaturesQuery {}

impl GetAggregatorFeaturesQuery {
    /// Instantiate a query to get the features of the aggregator
    pub fn current() -> Self {
        Self {}
    }
}

#[cfg_attr(target_family = "wasm", async_trait(?Send))]
#[cfg_attr(not(target_family = "wasm"), async_trait)]
impl AggregatorQuery for GetAggregatorFeaturesQuery {
    type Response = AggregatorFeaturesMessage;
    type Body = ();

    fn method() -> QueryMethod {
        QueryMethod::Get
    }

    fn route(&self) -> String {
        "/".to_string()
    }

    async fn handle_response(
        &self,
        context: QueryContext,
    ) -> AggregatorHttpClientResult<Self::Response> {
        debug!(context.logger, "Retrieve aggregator features message");

        match context.response.status() {
            StatusCode::OK => Ok(context
                .response
                .json::<AggregatorFeaturesMessage>()
                .await
                .map_err(|e| AggregatorHttpClientError::JsonParseFailed(anyhow!(e)))?),
            _ => Err(context.unhandled_status_code().await),
        }
    }
}

#[cfg(test)]
mod tests {
    use serde_json::json;

    use mithril_common::test::double::Dummy;

    use crate::test::{assert_error_matches, setup_server_and_client};

    use super::*;

    #[tokio::test]
    async fn test_aggregator_features_ok_200() {
        let (server, client) = setup_server_and_client();
        let message_expected = AggregatorFeaturesMessage::dummy();
        let _server_mock = server.mock(|when, then| {
            when.path("/");
            then.status(200).body(json!(message_expected).to_string());
        });

        let message = client.send(GetAggregatorFeaturesQuery::current()).await.unwrap();

        assert_eq!(message_expected, message);
    }

    #[tokio::test]
    async fn test_aggregator_features_ko_500() {
        let (server, client) = setup_server_and_client();
        server.mock(|when, then| {
            when.any_request();
            then.status(500).body("an error occurred");
        });

        let error = client.send(GetAggregatorFeaturesQuery::current()).await.unwrap_err();

        assert_error_matches!(error, AggregatorHttpClientError::RemoteServerTechnical(_));
    }

    #[tokio::test]
    async fn test_aggregator_features_ko_json_serialization() {
        let (server, client) = setup_server_and_client();
        server.mock(|when, then| {
            when.any_request();
            then.status(200).body("this is not a json");
        });

        let error = client.send(GetAggregatorFeaturesQuery::current()).await.unwrap_err();

        assert_error_matches!(error, AggregatorHttpClientError::JsonParseFailed(_));
    }
}
