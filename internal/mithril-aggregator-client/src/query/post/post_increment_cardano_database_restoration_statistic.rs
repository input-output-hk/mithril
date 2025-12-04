use async_trait::async_trait;
use reqwest::StatusCode;
use std::fmt::{Display, Formatter};

use crate::AggregatorHttpClientResult;
use crate::query::{AggregatorQuery, QueryContext, QueryMethod};

/// Query to notify the aggregator that a Cardano database restoration has been run.
pub struct PostIncrementCardanoDatabaseRestorationStatisticQuery {
    restoration_type: RestorationType,
}

enum RestorationType {
    Complete,
    Partial,
}

impl Display for RestorationType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            RestorationType::Complete => write!(f, "complete"),
            RestorationType::Partial => write!(f, "partial"),
        }
    }
}

impl PostIncrementCardanoDatabaseRestorationStatisticQuery {
    /// Instantiate a new query that will notify the aggregator that a partial Cardano database
    /// restoration has been run.
    pub fn partial() -> Self {
        Self {
            restoration_type: RestorationType::Partial,
        }
    }

    /// Instantiate a new query that will notify the aggregator that a complete Cardano database
    /// restoration has been run.
    pub fn complete() -> Self {
        Self {
            restoration_type: RestorationType::Complete,
        }
    }
}

#[cfg_attr(target_family = "wasm", async_trait(?Send))]
#[cfg_attr(not(target_family = "wasm"), async_trait)]
impl AggregatorQuery for PostIncrementCardanoDatabaseRestorationStatisticQuery {
    type Response = ();
    type Body = ();

    fn method() -> QueryMethod {
        QueryMethod::Post
    }

    fn route(&self) -> String {
        format!(
            "statistics/cardano-database/{}-restoration",
            self.restoration_type
        )
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

    mod complete_restoration_statistic {
        use super::*;

        #[tokio::test]
        async fn test_increment_cdb_complete_restoration_statistics_ok_201() {
            let (server, client) = setup_server_and_client();
            let _server_mock = server.mock(|when, then| {
                when.method(POST)
                    .path("/statistics/cardano-database/complete-restoration")
                    .body("");
                then.status(201);
            });

            let statistic_query = client
                .send(PostIncrementCardanoDatabaseRestorationStatisticQuery::complete())
                .await;
            statistic_query.expect("unexpected error");
        }

        #[tokio::test]
        async fn test_increment_cdb_complete_restoration_statistics_ko_400() {
            let (server, client) = setup_server_and_client();
            let _server_mock = server.mock(|when, then| {
                when.method(POST)
                    .path("/statistics/cardano-database/complete-restoration");
                then.status(400)
                    .body(serde_json::to_vec(&ClientError::dummy()).unwrap());
            });

            let error = client
                .send(PostIncrementCardanoDatabaseRestorationStatisticQuery::complete())
                .await
                .unwrap_err();

            assert_error_matches!(error, AggregatorHttpClientError::RemoteServerLogical(_));
        }

        #[tokio::test]
        async fn test_increment_cdb_complete_restoration_statistics_ko_500() {
            let (server, client) = setup_server_and_client();
            let _server_mock = server.mock(|when, then| {
                when.method(POST)
                    .path("/statistics/cardano-database/complete-restoration");
                then.status(500).body("an error occurred");
            });

            let error = client
                .send(PostIncrementCardanoDatabaseRestorationStatisticQuery::complete())
                .await
                .unwrap_err();

            assert_error_matches!(error, AggregatorHttpClientError::RemoteServerTechnical(_));
        }
    }

    mod partial_complete_restoration_statistic {
        use super::*;

        #[tokio::test]
        async fn test_increment_cdb_partial_restoration_statistics_ok_201() {
            let (server, client) = setup_server_and_client();
            let _server_mock = server.mock(|when, then| {
                when.method(POST)
                    .path("/statistics/cardano-database/partial-restoration")
                    .body("");
                then.status(201);
            });

            let statistic_query = client
                .send(PostIncrementCardanoDatabaseRestorationStatisticQuery::partial())
                .await;
            statistic_query.expect("unexpected error");
        }

        #[tokio::test]
        async fn test_increment_cdb_partial_restoration_statistics_ko_400() {
            let (server, client) = setup_server_and_client();
            let _server_mock = server.mock(|when, then| {
                when.method(POST).any_request();
                then.status(400)
                    .body(serde_json::to_vec(&ClientError::dummy()).unwrap());
            });

            let error = client
                .send(PostIncrementCardanoDatabaseRestorationStatisticQuery::partial())
                .await
                .unwrap_err();

            assert_error_matches!(error, AggregatorHttpClientError::RemoteServerLogical(_));
        }

        #[tokio::test]
        async fn test_increment_cdb_partial_restoration_statistics_ko_500() {
            let (server, client) = setup_server_and_client();
            let _server_mock = server.mock(|when, then| {
                when.method(POST).any_request();
                then.status(500).body("an error occurred");
            });

            let error = client
                .send(PostIncrementCardanoDatabaseRestorationStatisticQuery::partial())
                .await
                .unwrap_err();

            assert_error_matches!(error, AggregatorHttpClientError::RemoteServerTechnical(_));
        }
    }
}
