use std::fmt::{Display, Formatter};

use anyhow::anyhow;
use async_trait::async_trait;
use mithril_common::entities::EpochSpecifier;
use mithril_common::messages::CardanoStakeDistributionMessage;
use reqwest::StatusCode;

use crate::query::{AggregatorQuery, QueryContext, QueryMethod};
use crate::{AggregatorHttpClientError, AggregatorHttpClientResult};

/// Query to get a Cardano stake distribution
pub struct GetCardanoStakeDistributionQuery {
    scope: QueryScope,
}

enum QueryScope {
    /// Fetch the Cardano stake distribution with the given hash
    Hash(String),
    /// Fetch the Cardano stake distribution for a specified epoch
    Epoch(EpochSpecifier),
}

impl Display for QueryScope {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self {
            QueryScope::Hash(hash) => write!(f, "hash({})", hash),
            QueryScope::Epoch(specifier) => {
                write!(f, "epoch({specifier})")
            }
        }
    }
}

impl GetCardanoStakeDistributionQuery {
    /// Instantiate a query to get a Cardano stake distribution by hash
    pub fn by_hash<H: Into<String>>(hash: H) -> Self {
        Self {
            scope: QueryScope::Hash(hash.into()),
        }
    }

    /// Instantiate a query to get a Cardano stake distribution for a specified epoch
    pub fn for_epoch(epoch_specifier: EpochSpecifier) -> Self {
        Self {
            scope: QueryScope::Epoch(epoch_specifier),
        }
    }
}

#[cfg_attr(target_family = "wasm", async_trait(?Send))]
#[cfg_attr(not(target_family = "wasm"), async_trait)]
impl AggregatorQuery for GetCardanoStakeDistributionQuery {
    type Response = Option<CardanoStakeDistributionMessage>;
    type Body = ();

    fn method() -> QueryMethod {
        QueryMethod::Get
    }

    fn route(&self) -> String {
        match &self.scope {
            QueryScope::Hash(hash) => format!("artifact/cardano-stake-distribution/{hash}"),
            QueryScope::Epoch(specifier) => {
                format!("artifact/cardano-stake-distribution/epoch/{specifier}")
            }
        }
    }

    async fn handle_response(
        &self,
        context: QueryContext,
    ) -> AggregatorHttpClientResult<Self::Response> {
        match context.response.status() {
            StatusCode::OK => {
                match context.response.json::<CardanoStakeDistributionMessage>().await {
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
    use mithril_common::entities::Epoch;
    use mithril_common::test::double::Dummy;
    use serde_json::json;

    use super::*;
    use crate::test::{assert_error_matches, setup_server_and_client};

    #[tokio::test]
    async fn test_cardano_stake_distribution_by_hash_ok_200() {
        let (server, client) = setup_server_and_client();
        let expected_message = CardanoStakeDistributionMessage::dummy();
        let _server_mock = server.mock(|when, then| {
            when.path(format!(
                "/artifact/cardano-stake-distribution/{}",
                expected_message.hash
            ));
            then.status(200).body(json!(expected_message).to_string());
        });

        let fetched_message = client
            .send(GetCardanoStakeDistributionQuery::by_hash(
                &expected_message.hash,
            ))
            .await
            .unwrap();

        assert_eq!(Some(expected_message), fetched_message);
    }

    #[tokio::test]
    async fn test_cardano_stake_distribution_by_epoch_ok_200() {
        let (server, client) = setup_server_and_client();
        let expected_message = CardanoStakeDistributionMessage::dummy();
        let _server_mock = server.mock(|when, then| {
            when.path("/artifact/cardano-stake-distribution/epoch/6");
            then.status(200).body(json!(expected_message).to_string());
        });

        let fetched_message = client
            .send(GetCardanoStakeDistributionQuery::for_epoch(
                EpochSpecifier::Number(Epoch(6)),
            ))
            .await
            .unwrap();

        assert_eq!(Some(expected_message), fetched_message);
    }

    #[tokio::test]
    async fn test_cardano_stake_distribution_for_latest_epoch_ok_200() {
        let (server, client) = setup_server_and_client();
        let expected_message = CardanoStakeDistributionMessage::dummy();
        let _server_mock = server.mock(|when, then| {
            when.path("/artifact/cardano-stake-distribution/epoch/latest");
            then.status(200).body(json!(expected_message).to_string());
        });

        let fetched_message = client
            .send(GetCardanoStakeDistributionQuery::for_epoch(
                EpochSpecifier::Latest,
            ))
            .await
            .unwrap();

        assert_eq!(Some(expected_message), fetched_message);
    }

    #[tokio::test]
    async fn test_cardano_stake_distribution_for_latest_epoch_with_offset_ok_200() {
        let (server, client) = setup_server_and_client();
        let expected_message = CardanoStakeDistributionMessage::dummy();
        let _server_mock = server.mock(|when, then| {
            when.path("/artifact/cardano-stake-distribution/epoch/latest-5");
            then.status(200).body(json!(expected_message).to_string());
        });

        let fetched_message = client
            .send(GetCardanoStakeDistributionQuery::for_epoch(
                EpochSpecifier::LatestMinusOffset(5),
            ))
            .await
            .unwrap();

        assert_eq!(Some(expected_message), fetched_message);
    }

    #[tokio::test]
    async fn test_cardano_stake_distribution_details_ok_404() {
        let (server, client) = setup_server_and_client();
        let _server_mock = server.mock(|when, then| {
            when.any_request();
            then.status(404);
        });

        let fetched_message = client
            .send(GetCardanoStakeDistributionQuery::by_hash("whatever"))
            .await
            .unwrap();

        assert_eq!(None, fetched_message);
    }

    #[tokio::test]
    async fn test_cardano_stake_distribution_details_ko_500() {
        let (server, client) = setup_server_and_client();
        let _server_mock = server.mock(|when, then| {
            when.any_request();
            then.status(500).body("an error occurred");
        });

        let error = client
            .send(GetCardanoStakeDistributionQuery::by_hash("whatever"))
            .await
            .unwrap_err();

        assert_error_matches!(error, AggregatorHttpClientError::RemoteServerTechnical(_));
    }
}
