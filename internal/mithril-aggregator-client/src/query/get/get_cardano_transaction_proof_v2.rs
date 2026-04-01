use async_trait::async_trait;
use reqwest::StatusCode;

use mithril_common::messages::CardanoTransactionsProofsV2Message;

use crate::AggregatorHttpClientResult;
use crate::query::{AggregatorQuery, QueryContext, QueryMethod, ResponseExt};

/// Query to get a v2 proof of membership for a list of Cardano transaction hashes
pub struct GetCardanoTransactionProofV2Query {
    transactions_hashes: Vec<String>,
}

impl GetCardanoTransactionProofV2Query {
    /// Instantiate a query to get a v2 proof of membership for a list of Cardano transaction hashes
    pub fn for_hashes<H: ToString>(hashes: &[H]) -> Self {
        Self {
            transactions_hashes: hashes.iter().map(|h| h.to_string()).collect(),
        }
    }
}

#[cfg_attr(target_family = "wasm", async_trait(?Send))]
#[cfg_attr(not(target_family = "wasm"), async_trait)]
impl AggregatorQuery for GetCardanoTransactionProofV2Query {
    type Response = Option<CardanoTransactionsProofsV2Message>;
    type Body = ();

    fn method() -> QueryMethod {
        QueryMethod::Get
    }

    fn route(&self) -> String {
        format!(
            "proof/v2/cardano-transaction?transaction_hashes={}",
            self.transactions_hashes.join(",")
        )
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
    use mithril_common::messages::MkSetProofMessagePart;
    use serde_json::json;

    use crate::AggregatorHttpClientError;
    use crate::test::{assert_error_matches, setup_server_and_client};

    use super::*;

    #[tokio::test]
    async fn test_cardano_transaction_proof_v2_ok_200() {
        let (server, client) = setup_server_and_client();
        let expected_message = CardanoTransactionsProofsV2Message {
            certificate_hash: "whatever".to_string(),
            certified_transactions: Some(MkSetProofMessagePart {
                items: vec![],
                proof: "proof".to_string(),
            }),
            non_certified_transactions: vec![],
            latest_block_number: Default::default(),
            security_parameter: Default::default(),
        };

        let _server_mock = server.mock(|when, then| {
            when.path("/proof/v2/cardano-transaction")
                .query_param("transaction_hashes", "hash1,hash3,hash2");
            then.status(200).body(json!(expected_message).to_string());
        });

        let fetched_message = client
            .send(GetCardanoTransactionProofV2Query::for_hashes(&[
                "hash1", "hash3", "hash2",
            ]))
            .await
            .unwrap();

        assert_eq!(Some(expected_message), fetched_message);
    }

    #[tokio::test]
    async fn test_cardano_transaction_proof_v2_ok_404() {
        let (server, client) = setup_server_and_client();
        let _server_mock = server.mock(|when, then| {
            when.any_request();
            then.status(404);
        });

        let fetched_message = client
            .send(GetCardanoTransactionProofV2Query::for_hashes(&["whatever"]))
            .await
            .unwrap();

        assert_eq!(None, fetched_message);
    }

    #[tokio::test]
    async fn test_cardano_transaction_proof_v2_ko_500() {
        let (server, client) = setup_server_and_client();
        let _server_mock = server.mock(|when, then| {
            when.any_request();
            then.status(500).body("an error occurred");
        });

        let error = client
            .send(GetCardanoTransactionProofV2Query::for_hashes(&["whatever"]))
            .await
            .unwrap_err();

        assert_error_matches!(error, AggregatorHttpClientError::RemoteServerTechnical(_));
    }
}
