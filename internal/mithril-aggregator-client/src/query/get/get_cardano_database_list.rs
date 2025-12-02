use anyhow::anyhow;
use async_trait::async_trait;
use reqwest::StatusCode;
use slog::debug;
use std::fmt::{Display, Formatter};

use mithril_common::entities::EpochSpecifier;
use mithril_common::messages::CardanoDatabaseDigestListMessage;

use crate::query::{AggregatorQuery, QueryContext, QueryMethod};
use crate::{AggregatorHttpClientError, AggregatorHttpClientResult};

/// Query to get a list of Cardano database v2 snapshots
pub struct GetCardanoDatabaseListQuery {
    scope: ListScope,
}

enum ListScope {
    /// Fetch the latest list of Cardano database v2 snapshots, regardless of their epochs.
    Latest,
    /// Fetch the list of Cardano database v2 snapshots for a specified epoch
    Epoch(EpochSpecifier),
}

impl Display for ListScope {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self {
            ListScope::Latest => {
                write!(f, "latest")
            }
            ListScope::Epoch(specifier) => {
                write!(f, "epoch({specifier})")
            }
        }
    }
}

impl GetCardanoDatabaseListQuery {
    /// Instantiate a query to get the latest Cardano database v2 snapshots list
    pub fn latest() -> Self {
        Self {
            scope: ListScope::Latest,
        }
    }

    /// Instantiate a query to get the Cardano database v2 snapshots list for a specified epoch
    pub fn for_epoch(epoch_specifier: EpochSpecifier) -> Self {
        Self {
            scope: ListScope::Epoch(epoch_specifier),
        }
    }
}

#[cfg_attr(target_family = "wasm", async_trait(?Send))]
#[cfg_attr(not(target_family = "wasm"), async_trait)]
impl AggregatorQuery for GetCardanoDatabaseListQuery {
    type Response = CardanoDatabaseDigestListMessage;
    type Body = ();

    fn method() -> QueryMethod {
        QueryMethod::Get
    }

    fn route(&self) -> String {
        match &self.scope {
            ListScope::Latest => "artifact/cardano-database".to_string(),
            ListScope::Epoch(specifier) => {
                format!("artifact/cardano-database/epoch/{specifier}")
            }
        }
    }

    async fn handle_response(
        &self,
        context: QueryContext,
    ) -> AggregatorHttpClientResult<Self::Response> {
        debug!(context.logger, "/GET: List cardano database snapshots"; "scope" => %self.scope);

        match context.response.status() {
            StatusCode::OK => {
                match context.response.json::<CardanoDatabaseDigestListMessage>().await {
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

    use mithril_common::entities::Epoch;
    use mithril_common::messages::CardanoDatabaseDigestListItemMessage;
    use mithril_common::test::double::Dummy;

    use crate::test::{assert_error_matches, setup_server_and_client};

    use super::*;

    #[tokio::test]
    async fn test_latest_cardano_database_list_ok_200() {
        let (server, client) = setup_server_and_client();
        let expected_list = vec![
            CardanoDatabaseDigestListItemMessage::dummy(),
            CardanoDatabaseDigestListItemMessage::dummy(),
        ];
        let _server_mock = server.mock(|when, then| {
            when.path("/artifact/cardano-database");
            then.status(200).body(json!(expected_list).to_string());
        });

        let fetched_list = client.send(GetCardanoDatabaseListQuery::latest()).await.unwrap();

        assert_eq!(expected_list, fetched_list);
    }

    #[tokio::test]
    async fn test_specific_epoch_cardano_database_list_ok_200() {
        let (server, client) = setup_server_and_client();
        let expected_list = vec![
            CardanoDatabaseDigestListItemMessage::dummy(),
            CardanoDatabaseDigestListItemMessage::dummy(),
        ];
        let _server_mock = server.mock(|when, then| {
            when.path("/artifact/cardano-database/epoch/12");
            then.status(200).body(json!(expected_list).to_string());
        });

        let fetched_list = client
            .send(GetCardanoDatabaseListQuery::for_epoch(
                EpochSpecifier::Number(Epoch(12)),
            ))
            .await
            .unwrap();

        assert_eq!(expected_list, fetched_list);
    }

    #[tokio::test]
    async fn test_latest_epoch_cardano_database_list_ok_200() {
        let (server, client) = setup_server_and_client();
        let expected_list = vec![
            CardanoDatabaseDigestListItemMessage::dummy(),
            CardanoDatabaseDigestListItemMessage::dummy(),
        ];
        let _server_mock = server.mock(|when, then| {
            when.path("/artifact/cardano-database/epoch/latest");
            then.status(200).body(json!(expected_list).to_string());
        });

        let fetched_list = client
            .send(GetCardanoDatabaseListQuery::for_epoch(
                EpochSpecifier::Latest,
            ))
            .await
            .unwrap();

        assert_eq!(expected_list, fetched_list);
    }

    #[tokio::test]
    async fn test_latest_epoch_with_offset_cardano_database_list_ok_200() {
        let (server, client) = setup_server_and_client();
        let expected_list = vec![
            CardanoDatabaseDigestListItemMessage::dummy(),
            CardanoDatabaseDigestListItemMessage::dummy(),
        ];
        let _server_mock = server.mock(|when, then| {
            when.path("/artifact/cardano-database/epoch/latest-3");
            then.status(200).body(json!(expected_list).to_string());
        });

        let fetched_list = client
            .send(GetCardanoDatabaseListQuery::for_epoch(
                EpochSpecifier::LatestMinusOffset(3),
            ))
            .await
            .unwrap();

        assert_eq!(expected_list, fetched_list);
    }

    #[tokio::test]
    async fn test_cardano_database_list_ko_500() {
        let (server, client) = setup_server_and_client();
        let _server_mock = server.mock(|when, then| {
            when.any_request();
            then.status(500).body("an error occurred");
        });

        let error = client.send(GetCardanoDatabaseListQuery::latest()).await.unwrap_err();

        assert_error_matches!(error, AggregatorHttpClientError::RemoteServerTechnical(_));
    }
}
