use async_trait::async_trait;
use reqwest::StatusCode;
use slog::debug;

use mithril_common::messages::CardanoDatabaseImmutableFilesRestoredMessage;

use crate::AggregatorHttpClientResult;
use crate::query::{AggregatorQuery, QueryContext, QueryMethod};

/// Query to notify the aggregator that Cardano database immutables files have been restored.
pub struct PostIncrementCardanoDatabaseImmutablesRestoredStatisticQuery {
    message: CardanoDatabaseImmutableFilesRestoredMessage,
}

impl PostIncrementCardanoDatabaseImmutablesRestoredStatisticQuery {
    /// Instantiate a new query to notify the aggregator that Cardano database snapshot immutables
    /// have been restored.
    pub fn new(nb_immutable_files: u32) -> Self {
        Self {
            message: CardanoDatabaseImmutableFilesRestoredMessage { nb_immutable_files },
        }
    }
}

#[cfg_attr(target_family = "wasm", async_trait(?Send))]
#[cfg_attr(not(target_family = "wasm"), async_trait)]
impl AggregatorQuery for PostIncrementCardanoDatabaseImmutablesRestoredStatisticQuery {
    type Response = ();
    type Body = CardanoDatabaseImmutableFilesRestoredMessage;

    fn method() -> QueryMethod {
        QueryMethod::Post
    }

    fn route(&self) -> String {
        "statistics/cardano-database/immutable-files-restored".to_string()
    }

    fn body(&self) -> Option<Self::Body> {
        Some(self.message.clone())
    }

    async fn handle_response(
        &self,
        context: QueryContext,
    ) -> AggregatorHttpClientResult<Self::Response> {
        debug!(
            context.logger, "POST: Increment Cardano database immutables files restored statistic";
            "nb_immutable_files" => self.message.nb_immutable_files
        );

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
    async fn test_increment_cdb_immutables_restored_statistics_ok_201() {
        let expected_message = CardanoDatabaseImmutableFilesRestoredMessage::dummy();
        let (server, client) = setup_server_and_client();
        let _server_mock = server.mock(|when, then| {
            when.method(POST)
                .path("/statistics/cardano-database/immutable-files-restored")
                .body(serde_json::to_string(&expected_message).unwrap());
            then.status(201);
        });

        let statistic_query = client
            .send(
                PostIncrementCardanoDatabaseImmutablesRestoredStatisticQuery::new(
                    expected_message.nb_immutable_files,
                ),
            )
            .await;
        statistic_query.expect("unexpected error");
    }

    #[tokio::test]
    async fn test_increment_cdb_immutables_restored_statistics_ko_400() {
        let (server, client) = setup_server_and_client();
        let _server_mock = server.mock(|when, then| {
            when.method(POST).any_request();
            then.status(400)
                .body(serde_json::to_vec(&ClientError::dummy()).unwrap());
        });

        let error = client
            .send(PostIncrementCardanoDatabaseImmutablesRestoredStatisticQuery::new(32))
            .await
            .unwrap_err();

        assert_error_matches!(error, AggregatorHttpClientError::RemoteServerLogical(_));
    }

    #[tokio::test]
    async fn test_increment_cdb_immutables_restored_statistics_ko_500() {
        let (server, client) = setup_server_and_client();
        let _server_mock = server.mock(|when, then| {
            when.method(POST).any_request();
            then.status(500).body("an error occurred");
        });

        let error = client
            .send(PostIncrementCardanoDatabaseImmutablesRestoredStatisticQuery::new(28))
            .await
            .unwrap_err();

        assert_error_matches!(error, AggregatorHttpClientError::RemoteServerTechnical(_));
    }
}
