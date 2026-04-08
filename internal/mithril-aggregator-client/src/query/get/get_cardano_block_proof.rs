use async_trait::async_trait;
use reqwest::StatusCode;

use mithril_common::messages::CardanoBlocksProofsMessage;

use crate::AggregatorHttpClientResult;
use crate::query::{AggregatorQuery, QueryContext, QueryMethod, ResponseExt};

/// Query to get a v2 proof of membership for a list of Cardano block hashes
pub struct GetCardanoBlockProofQuery {
    block_hashes: Vec<String>,
}

impl GetCardanoBlockProofQuery {
    /// Instantiate a query to get a v2 proof of membership for a list of Cardano block hashes
    pub fn for_hashes<H: ToString>(hashes: &[H]) -> Self {
        Self {
            block_hashes: hashes.iter().map(|h| h.to_string()).collect(),
        }
    }
}

#[cfg_attr(target_family = "wasm", async_trait(?Send))]
#[cfg_attr(not(target_family = "wasm"), async_trait)]
impl AggregatorQuery for GetCardanoBlockProofQuery {
    type Response = Option<CardanoBlocksProofsMessage>;
    type Body = ();

    fn method() -> QueryMethod {
        QueryMethod::Get
    }

    fn route(&self) -> String {
        format!(
            "proof/v2/cardano-block?block_hashes={}",
            self.block_hashes.join(",")
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
    async fn test_cardano_block_proof_ok_200() {
        let (server, client) = setup_server_and_client();
        let expected_message = CardanoBlocksProofsMessage {
            certificate_hash: "whatever".to_string(),
            certified_blocks: Some(MkSetProofMessagePart {
                items: vec![],
                proof: "proof".to_string(),
            }),
            non_certified_blocks: vec![],
            latest_block_number: Default::default(),
            security_parameter: Default::default(),
        };
        let _server_mock = server.mock(|when, then| {
            when.path("/proof/v2/cardano-block")
                .query_param("block_hashes", "hash1,hash3,hash2");
            then.status(200).body(json!(expected_message).to_string());
        });

        let fetched_message = client
            .send(GetCardanoBlockProofQuery::for_hashes(&[
                "hash1", "hash3", "hash2",
            ]))
            .await
            .unwrap();

        assert_eq!(Some(expected_message), fetched_message);
    }

    #[tokio::test]
    async fn test_cardano_block_proof_ok_404() {
        let (server, client) = setup_server_and_client();
        let _server_mock = server.mock(|when, then| {
            when.any_request();
            then.status(404);
        });

        let fetched_message = client
            .send(GetCardanoBlockProofQuery::for_hashes(&["whatever"]))
            .await
            .unwrap();

        assert_eq!(None, fetched_message);
    }

    #[tokio::test]
    async fn test_cardano_block_proof_ko_500() {
        let (server, client) = setup_server_and_client();
        let _server_mock = server.mock(|when, then| {
            when.any_request();
            then.status(500).body("an error occurred");
        });

        let error = client
            .send(GetCardanoBlockProofQuery::for_hashes(&["whatever"]))
            .await
            .unwrap_err();

        assert_error_matches!(error, AggregatorHttpClientError::RemoteServerTechnical(_));
    }
}
