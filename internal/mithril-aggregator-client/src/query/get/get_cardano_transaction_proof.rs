use anyhow::anyhow;
use async_trait::async_trait;
use mithril_common::messages::CardanoTransactionsProofsMessage;
use reqwest::StatusCode;

use crate::AggregatorHttpClientResult;
use crate::error::AggregatorHttpClientError;
use crate::query::{AggregatorQuery, QueryContext, QueryMethod};

/// Query to get a proof of membership for a list of Cardano transaction hashes
pub struct GetCardanoTransactionProofQuery {
    transactions_hashes: Vec<String>,
}

impl GetCardanoTransactionProofQuery {
    /// Instantiate a query to get a proof of membership for a list of Cardano transaction hashes
    pub fn for_hashes<H: ToString>(hashes: &[H]) -> Self {
        Self {
            transactions_hashes: hashes.iter().map(|h| h.to_string()).collect(),
        }
    }
}

#[cfg_attr(target_family = "wasm", async_trait(?Send))]
#[cfg_attr(not(target_family = "wasm"), async_trait)]
impl AggregatorQuery for GetCardanoTransactionProofQuery {
    type Response = Option<CardanoTransactionsProofsMessage>;
    type Body = ();

    fn method() -> QueryMethod {
        QueryMethod::Get
    }

    fn route(&self) -> String {
        format!(
            "proof/cardano-transaction?transaction_hashes={}",
            self.transactions_hashes.join(",")
        )
    }

    async fn handle_response(
        &self,
        context: QueryContext,
    ) -> AggregatorHttpClientResult<Self::Response> {
        match context.response.status() {
            StatusCode::OK => {
                match context.response.json::<CardanoTransactionsProofsMessage>().await {
                    Ok(message) => Ok(Some(message)),
                    Err(err) => Err(AggregatorHttpClientError::JsonParseFailed(anyhow!(err))),
                }
            }
            StatusCode::NOT_FOUND => Ok(None),
            _ => Err(context.unhandled_status_code().await),
        }
    }
}

#[cfg(test)]
mod tests {
    use serde_json::json;

    use super::*;
    use crate::test::{assert_error_matches, setup_server_and_client};

    #[tokio::test]
    async fn test_cardano_transaction_proof_ok_200() {
        let (server, client) = setup_server_and_client();
        let expected_message = CardanoTransactionsProofsMessage {
            certificate_hash: "whatever".to_string(),
            certified_transactions: vec![],
            non_certified_transactions: vec![],
            latest_block_number: Default::default(),
        };
        let _server_mock = server.mock(|when, then| {
            when.path("/proof/cardano-transaction")
                .query_param("transaction_hashes", "hash1,hash3,hash2");
            then.status(200).body(json!(expected_message).to_string());
        });

        let fetched_message = client
            .send(GetCardanoTransactionProofQuery::for_hashes(&[
                "hash1", "hash3", "hash2",
            ]))
            .await
            .unwrap();

        assert_eq!(Some(expected_message), fetched_message);
    }

    #[tokio::test]
    async fn test_cardano_transaction_proof_ok_404() {
        let (server, client) = setup_server_and_client();
        let _server_mock = server.mock(|when, then| {
            when.any_request();
            then.status(404);
        });

        let fetched_message = client
            .send(GetCardanoTransactionProofQuery::for_hashes(&["whatever"]))
            .await
            .unwrap();

        assert_eq!(None, fetched_message);
    }

    #[tokio::test]
    async fn test_cardano_transaction_proof_ko_500() {
        let (server, client) = setup_server_and_client();
        let _server_mock = server.mock(|when, then| {
            when.any_request();
            then.status(500).body("an error occurred");
        });

        let error = client
            .send(GetCardanoTransactionProofQuery::for_hashes(&["whatever"]))
            .await
            .unwrap_err();

        assert_error_matches!(error, AggregatorHttpClientError::RemoteServerTechnical(_));
    }
}
