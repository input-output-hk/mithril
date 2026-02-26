use async_trait::async_trait;
use reqwest::StatusCode;

use mithril_common::messages::CardanoBlocksTransactionsSnapshotListMessage;

use crate::AggregatorHttpClientResult;
use crate::query::{AggregatorQuery, QueryContext, QueryMethod, ResponseExt};

/// Query to get a list of Cardano blocks and transactions snapshots
pub struct GetCardanoBlocksTransactionsListQuery {}

impl GetCardanoBlocksTransactionsListQuery {
    /// Instantiate a query to get the latest Cardano blocks and transactions snapshots list
    pub fn latest() -> Self {
        Self {}
    }
}

#[cfg_attr(target_family = "wasm", async_trait(?Send))]
#[cfg_attr(not(target_family = "wasm"), async_trait)]
impl AggregatorQuery for GetCardanoBlocksTransactionsListQuery {
    type Response = CardanoBlocksTransactionsSnapshotListMessage;
    type Body = ();

    fn method() -> QueryMethod {
        QueryMethod::Get
    }

    fn route(&self) -> String {
        "artifact/cardano-blocks-transactions".to_string()
    }

    async fn handle_response(
        &self,
        context: QueryContext,
    ) -> AggregatorHttpClientResult<Self::Response> {
        match context.response.status() {
            StatusCode::OK => context.response.parse_json().await,
            _ => Err(context.unhandled_status_code().await),
        }
    }
}

#[cfg(test)]
mod tests {
    use serde_json::json;

    use mithril_common::messages::CardanoBlocksTransactionsSnapshotListItemMessage;
    use mithril_common::test::double::Dummy;

    use crate::AggregatorHttpClientError;
    use crate::test::{assert_error_matches, setup_server_and_client};

    use super::*;

    #[tokio::test]
    async fn test_latest_cardano_blocks_transactions_list_ok_200() {
        let (server, client) = setup_server_and_client();
        let expected_list = vec![
            CardanoBlocksTransactionsSnapshotListItemMessage::dummy(),
            CardanoBlocksTransactionsSnapshotListItemMessage::dummy(),
        ];
        let _server_mock = server.mock(|when, then| {
            when.path("/artifact/cardano-blocks-transactions");
            then.status(200).body(json!(expected_list).to_string());
        });

        let fetched_list = client
            .send(GetCardanoBlocksTransactionsListQuery::latest())
            .await
            .unwrap();

        assert_eq!(expected_list, fetched_list);
    }

    #[tokio::test]
    async fn test_latest_cardano_blocks_transactions_list_ko_500() {
        let (server, client) = setup_server_and_client();
        let _server_mock = server.mock(|when, then| {
            when.any_request();
            then.status(500).body("an error occurred");
        });

        let error = client
            .send(GetCardanoBlocksTransactionsListQuery::latest())
            .await
            .unwrap_err();

        assert_error_matches!(error, AggregatorHttpClientError::RemoteServerTechnical(_));
    }
}
