use async_trait::async_trait;
use reqwest::StatusCode;

use crate::AggregatorHttpClientResult;
use crate::query::{AggregatorQuery, QueryContext, QueryMethod};

/// Query to notify the aggregator that Cardano database ancillary files have been restored.
pub struct PostIncrementCardanoDatabaseAncillaryRestoredStatisticQuery;

#[cfg_attr(target_family = "wasm", async_trait(?Send))]
#[cfg_attr(not(target_family = "wasm"), async_trait)]
impl AggregatorQuery for PostIncrementCardanoDatabaseAncillaryRestoredStatisticQuery {
    type Response = ();
    type Body = ();

    fn method() -> QueryMethod {
        QueryMethod::Post
    }

    fn route(&self) -> String {
        "statistics/cardano-database/ancillary-files-restored".to_string()
    }

    fn body(&self) -> Option<Self::Body> {
        None
    }

    async fn handle_response(
        &self,
        context: QueryContext,
    ) -> AggregatorHttpClientResult<Self::Response> {
        match context.response.status() {
            StatusCode::CREATED => Ok(()),
            _ => Err(context.unhandled_status_code().await),
        }
    }
}

#[cfg(test)]
mod tests {
    use httpmock::Method::POST;

    use mithril_common::entities::ClientError;
    use mithril_common::test::double::Dummy;

    use crate::AggregatorHttpClientError;
    use crate::test::{assert_error_matches, setup_server_and_client};

    use super::*;

    #[tokio::test]
    async fn test_increment_cdb_ancillary_restored_statistics_ok_201() {
        let (server, client) = setup_server_and_client();
        let _server_mock = server.mock(|when, then| {
            when.method(POST)
                .path("/statistics/cardano-database/ancillary-files-restored")
                .body("");
            then.status(201);
        });

        let statistic_query = client
            .send(PostIncrementCardanoDatabaseAncillaryRestoredStatisticQuery)
            .await;
        statistic_query.expect("unexpected error");
    }

    #[tokio::test]
    async fn test_increment_cdb_ancillary_restored_statistics_ko_400() {
        let (server, client) = setup_server_and_client();
        let _server_mock = server.mock(|when, then| {
            when.method(POST).any_request();
            then.status(400)
                .body(serde_json::to_vec(&ClientError::dummy()).unwrap());
        });

        let error = client
            .send(PostIncrementCardanoDatabaseAncillaryRestoredStatisticQuery)
            .await
            .unwrap_err();

        assert_error_matches!(error, AggregatorHttpClientError::RemoteServerLogical(_));
    }

    #[tokio::test]
    async fn test_increment_cdb_ancillary_restored_statistics_ko_500() {
        let (server, client) = setup_server_and_client();
        let _server_mock = server.mock(|when, then| {
            when.method(POST).any_request();
            then.status(500).body("an error occurred");
        });

        let error = client
            .send(PostIncrementCardanoDatabaseAncillaryRestoredStatisticQuery)
            .await
            .unwrap_err();

        assert_error_matches!(error, AggregatorHttpClientError::RemoteServerTechnical(_));
    }
}
