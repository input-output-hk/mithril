use async_trait::async_trait;
use reqwest::StatusCode;

use mithril_common::messages::CardanoTransactionSnapshotMessage;

use crate::AggregatorHttpClientResult;
use crate::query::{AggregatorQuery, QueryContext, QueryMethod, ResponseExt};

/// Query to get the details of a Cardano transaction snapshot detail
pub struct GetCardanoTransactionQuery {
    hash: String,
}

impl GetCardanoTransactionQuery {
    /// Instantiate a query to get a Cardano transaction snapshot detail by hash
    pub fn by_hash<H: Into<String>>(hash: H) -> Self {
        Self { hash: hash.into() }
    }
}

#[cfg_attr(target_family = "wasm", async_trait(?Send))]
#[cfg_attr(not(target_family = "wasm"), async_trait)]
impl AggregatorQuery for GetCardanoTransactionQuery {
    type Response = Option<CardanoTransactionSnapshotMessage>;
    type Body = ();

    fn method() -> QueryMethod {
        QueryMethod::Get
    }

    fn route(&self) -> String {
        format!("artifact/cardano-transaction/{}", self.hash)
    }

    async fn handle_response(
        &self,
        context: QueryContext,
    ) -> AggregatorHttpClientResult<Self::Response> {
        match context.response.status() {
            StatusCode::OK => context.response.parse_json_option().await,
            StatusCode::NOT_FOUND => Ok(None),
            _ => Err(context.unhandled_status_code().await),
        }
    }
}

#[cfg(test)]
mod tests {
    use serde_json::json;

    use mithril_common::test::double::Dummy;

    use crate::AggregatorHttpClientError;
    use crate::test::{assert_error_matches, setup_server_and_client};

    use super::*;

    #[tokio::test]
    async fn test_cardano_transaction_details_ok_200() {
        let (server, client) = setup_server_and_client();
        let expected_message = CardanoTransactionSnapshotMessage::dummy();
        let _server_mock = server.mock(|when, then| {
            when.path(format!(
                "/artifact/cardano-transaction/{}",
                expected_message.hash
            ));
            then.status(200).body(json!(expected_message).to_string());
        });

        let fetched_message = client
            .send(GetCardanoTransactionQuery::by_hash(&expected_message.hash))
            .await
            .unwrap();

        assert_eq!(Some(expected_message), fetched_message);
    }

    #[tokio::test]
    async fn test_cardano_transaction_details_ok_404() {
        let (server, client) = setup_server_and_client();
        let _server_mock = server.mock(|when, then| {
            when.any_request();
            then.status(404);
        });

        let fetched_message = client
            .send(GetCardanoTransactionQuery::by_hash("whatever"))
            .await
            .unwrap();

        assert_eq!(None, fetched_message);
    }

    #[tokio::test]
    async fn test_cardano_transaction_details_ko_500() {
        let (server, client) = setup_server_and_client();
        let _server_mock = server.mock(|when, then| {
            when.any_request();
            then.status(500).body("an error occurred");
        });

        let error = client
            .send(GetCardanoTransactionQuery::by_hash("whatever"))
            .await
            .unwrap_err();

        assert_error_matches!(error, AggregatorHttpClientError::RemoteServerTechnical(_));
    }
}
