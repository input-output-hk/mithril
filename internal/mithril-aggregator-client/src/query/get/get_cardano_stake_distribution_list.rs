use async_trait::async_trait;
use reqwest::StatusCode;

use mithril_common::messages::CardanoStakeDistributionListMessage;

use crate::AggregatorHttpClientResult;
use crate::query::{AggregatorQuery, QueryContext, QueryMethod, ResponseExt};

/// Query to get a list of Cardano stake distributions
pub struct GetCardanoStakeDistributionsListQuery {}

impl GetCardanoStakeDistributionsListQuery {
    /// Instantiate a query to get the latest Cardano stake distributions list
    pub fn latest() -> Self {
        Self {}
    }
}

#[cfg_attr(target_family = "wasm", async_trait(?Send))]
#[cfg_attr(not(target_family = "wasm"), async_trait)]
impl AggregatorQuery for GetCardanoStakeDistributionsListQuery {
    type Response = CardanoStakeDistributionListMessage;
    type Body = ();

    fn method() -> QueryMethod {
        QueryMethod::Get
    }

    fn route(&self) -> String {
        "artifact/cardano-stake-distributions".to_string()
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

    use mithril_common::messages::CardanoStakeDistributionListItemMessage;
    use mithril_common::test::double::Dummy;

    use crate::AggregatorHttpClientError;
    use crate::test::{assert_error_matches, setup_server_and_client};

    use super::*;

    #[tokio::test]
    async fn test_latest_mithril_stake_distributions_list_ok_200() {
        let (server, client) = setup_server_and_client();
        let expected_list = vec![
            CardanoStakeDistributionListItemMessage::dummy(),
            CardanoStakeDistributionListItemMessage::dummy(),
        ];
        let _server_mock = server.mock(|when, then| {
            when.path("/artifact/cardano-stake-distributions");
            then.status(200).body(json!(expected_list).to_string());
        });

        let fetched_list = client
            .send(GetCardanoStakeDistributionsListQuery::latest())
            .await
            .unwrap();

        assert_eq!(expected_list, fetched_list);
    }

    #[tokio::test]
    async fn test_latest_mithril_stake_distributions_list_ko_500() {
        let (server, client) = setup_server_and_client();
        let _server_mock = server.mock(|when, then| {
            when.any_request();
            then.status(500).body("an error occurred");
        });

        let error = client
            .send(GetCardanoStakeDistributionsListQuery::latest())
            .await
            .unwrap_err();

        assert_error_matches!(error, AggregatorHttpClientError::RemoteServerTechnical(_));
    }
}
