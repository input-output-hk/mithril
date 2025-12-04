use async_trait::async_trait;
use reqwest::StatusCode;

use mithril_common::messages::SnapshotDownloadMessage;

use crate::AggregatorHttpClientResult;
use crate::query::{AggregatorQuery, QueryContext, QueryMethod};

/// Query to notify the aggregator that a snapshot has been downloaded.
pub struct PostIncrementSnapshotDownloadStatisticQuery {
    message: SnapshotDownloadMessage,
}

impl PostIncrementSnapshotDownloadStatisticQuery {
    /// Instantiate a new query that will notify the aggregator that a snapshot has been downloaded.
    pub fn new(message: SnapshotDownloadMessage) -> Self {
        Self { message }
    }
}

#[cfg_attr(target_family = "wasm", async_trait(?Send))]
#[cfg_attr(not(target_family = "wasm"), async_trait)]
impl AggregatorQuery for PostIncrementSnapshotDownloadStatisticQuery {
    type Response = ();
    type Body = SnapshotDownloadMessage;

    fn method() -> QueryMethod {
        QueryMethod::Post
    }

    fn route(&self) -> String {
        "statistics/snapshot".to_string()
    }

    fn body(&self) -> Option<Self::Body> {
        Some(self.message.clone())
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
    async fn test_increment_snapshot_download_statistics_ok_201() {
        let expected_message = SnapshotDownloadMessage::dummy();
        let (server, client) = setup_server_and_client();
        let _server_mock = server.mock(|when, then| {
            when.method(POST)
                .path("/statistics/snapshot")
                .body(serde_json::to_string(&expected_message).unwrap());
            then.status(201);
        });

        let statistic_query = client
            .send(PostIncrementSnapshotDownloadStatisticQuery::new(
                expected_message,
            ))
            .await;
        statistic_query.expect("unexpected error");
    }

    #[tokio::test]
    async fn test_increment_snapshot_download_statistics_ko_400() {
        let (server, client) = setup_server_and_client();
        let _server_mock = server.mock(|when, then| {
            when.method(POST).any_request();
            then.status(400)
                .body(serde_json::to_vec(&ClientError::dummy()).unwrap());
        });

        let error = client
            .send(PostIncrementSnapshotDownloadStatisticQuery::new(
                SnapshotDownloadMessage::dummy(),
            ))
            .await
            .unwrap_err();

        assert_error_matches!(error, AggregatorHttpClientError::RemoteServerLogical(_));
    }

    #[tokio::test]
    async fn test_increment_snapshot_download_statistics_ko_500() {
        let (server, client) = setup_server_and_client();
        let _server_mock = server.mock(|when, then| {
            when.method(POST).any_request();
            then.status(500).body("an error occurred");
        });

        let error = client
            .send(PostIncrementSnapshotDownloadStatisticQuery::new(
                SnapshotDownloadMessage::dummy(),
            ))
            .await
            .unwrap_err();

        assert_error_matches!(error, AggregatorHttpClientError::RemoteServerTechnical(_));
    }
}
