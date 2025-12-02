use anyhow::anyhow;
use async_trait::async_trait;
use reqwest::StatusCode;
use slog::debug;

use mithril_common::messages::CardanoDatabaseSnapshotMessage;

use crate::AggregatorHttpClientResult;
use crate::error::AggregatorHttpClientError;
use crate::query::{AggregatorQuery, QueryContext, QueryMethod};

/// Query to get the details of a Cardano database v2 snapshot
pub struct GetCardanoDatabaseQuery {
    hash: String,
}

impl GetCardanoDatabaseQuery {
    /// Instantiate a query to get Cardano database v2 snapshot by hash
    pub fn by_hash<H: Into<String>>(hash: H) -> Self {
        Self { hash: hash.into() }
    }
}

#[cfg_attr(target_family = "wasm", async_trait(?Send))]
#[cfg_attr(not(target_family = "wasm"), async_trait)]
impl AggregatorQuery for GetCardanoDatabaseQuery {
    type Response = Option<CardanoDatabaseSnapshotMessage>;
    type Body = ();

    fn method() -> QueryMethod {
        QueryMethod::Get
    }

    fn route(&self) -> String {
        format!("artifact/cardano-database/{}", self.hash)
    }

    async fn handle_response(
        &self,
        context: QueryContext,
    ) -> AggregatorHttpClientResult<Self::Response> {
        debug!(context.logger, "GET: Retrieve cardano database details"; "hash" => %self.hash);

        match context.response.status() {
            StatusCode::OK => match context.response.json::<CardanoDatabaseSnapshotMessage>().await
            {
                Ok(message) => Ok(Some(message)),
                Err(err) => Err(AggregatorHttpClientError::JsonParseFailed(anyhow!(err))),
            },
            StatusCode::NOT_FOUND => Ok(None),
            _ => Err(context.unhandled_status_code().await),
        }
    }
}

#[cfg(test)]
mod tests {
    use serde_json::json;

    use mithril_common::test::double::Dummy;

    use crate::test::{assert_error_matches, setup_server_and_client};

    use super::*;

    #[tokio::test]
    async fn test_cardano_database_snapshot_ok_200() {
        let (server, client) = setup_server_and_client();
        let expected_message = CardanoDatabaseSnapshotMessage::dummy();
        let _server_mock = server.mock(|when, then| {
            when.path(format!(
                "/artifact/cardano-database/{}",
                expected_message.hash
            ));
            then.status(200).body(json!(expected_message).to_string());
        });

        let fetched_message = client
            .send(GetCardanoDatabaseQuery::by_hash(&expected_message.hash))
            .await
            .unwrap();

        assert_eq!(Some(expected_message), fetched_message);
    }

    #[tokio::test]
    async fn test_cardano_database_snapshot_ok_404() {
        let (server, client) = setup_server_and_client();
        let _server_mock = server.mock(|when, then| {
            when.any_request();
            then.status(404);
        });

        let fetched_message = client
            .send(GetCardanoDatabaseQuery::by_hash("whatever"))
            .await
            .unwrap();

        assert_eq!(None, fetched_message);
    }

    #[tokio::test]
    async fn test_cardano_database_snapshot_ko_500() {
        let (server, client) = setup_server_and_client();
        let _server_mock = server.mock(|when, then| {
            when.any_request();
            then.status(500).body("an error occurred");
        });

        let error = client
            .send(GetCardanoDatabaseQuery::by_hash("whatever"))
            .await
            .unwrap_err();

        assert_error_matches!(error, AggregatorHttpClientError::RemoteServerTechnical(_));
    }
}
