use anyhow::anyhow;
use async_trait::async_trait;
use reqwest::StatusCode;
use slog::debug;

use mithril_common::messages::CardanoTransactionSnapshotListMessage;

use crate::query::{AggregatorQuery, QueryContext, QueryMethod};
use crate::{AggregatorHttpClientError, AggregatorHttpClientResult};

/// Query to get a list of Cardano transactions snapshots
pub struct GetCardanoTransactionsListQuery {}

impl GetCardanoTransactionsListQuery {
    /// Instantiate a query to get the latest Cardano transactions snapshots list
    pub fn latest() -> Self {
        Self {}
    }
}

#[cfg_attr(target_family = "wasm", async_trait(?Send))]
#[cfg_attr(not(target_family = "wasm"), async_trait)]
impl AggregatorQuery for GetCardanoTransactionsListQuery {
    type Response = CardanoTransactionSnapshotListMessage;
    type Body = ();

    fn method() -> QueryMethod {
        QueryMethod::Get
    }

    fn route(&self) -> String {
        "artifact/cardano-transactions".to_string()
    }

    async fn handle_response(
        &self,
        context: QueryContext,
    ) -> AggregatorHttpClientResult<Self::Response> {
        debug!(context.logger, "/GET: List latest cardano transactions snapshots";);

        match context.response.status() {
            StatusCode::OK => {
                match context.response.json::<CardanoTransactionSnapshotListMessage>().await {
                    Ok(message) => Ok(message),
                    Err(err) => Err(AggregatorHttpClientError::JsonParseFailed(anyhow!(err))),
                }
            }
            _ => Err(context.unhandled_status_code().await),
        }
    }
}

#[cfg(test)]
mod tests {
    use serde_json::json;

    use mithril_common::messages::CardanoTransactionSnapshotListItemMessage;
    use mithril_common::test::double::Dummy;

    use crate::test::{assert_error_matches, setup_server_and_client};

    use super::*;

    #[tokio::test]
    async fn test_latest_cardano_transactions_list_ok_200() {
        let (server, client) = setup_server_and_client();
        let expected_list = vec![
            CardanoTransactionSnapshotListItemMessage::dummy(),
            CardanoTransactionSnapshotListItemMessage::dummy(),
        ];
        let _server_mock = server.mock(|when, then| {
            when.path("/artifact/cardano-transactions");
            then.status(200).body(json!(expected_list).to_string());
        });

        let fetched_list = client.send(GetCardanoTransactionsListQuery::latest()).await.unwrap();

        assert_eq!(expected_list, fetched_list);
    }

    #[tokio::test]
    async fn test_latest_cardano_transactions_list_ko_500() {
        let (server, client) = setup_server_and_client();
        let _server_mock = server.mock(|when, then| {
            when.any_request();
            then.status(500).body("an error occurred");
        });

        let error = client
            .send(GetCardanoTransactionsListQuery::latest())
            .await
            .unwrap_err();

        assert_error_matches!(error, AggregatorHttpClientError::RemoteServerTechnical(_));
    }
}
