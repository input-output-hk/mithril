use anyhow::anyhow;
use async_trait::async_trait;
use reqwest::StatusCode;
use slog::debug;

use mithril_common::entities::Epoch;
use mithril_common::messages::ProtocolConfigurationMessage;

use crate::query::{AggregatorQuery, QueryContext, QueryMethod};
use crate::{AggregatorHttpClientError, AggregatorHttpClientResult};

/// Query to get the protocol configuration of a given epoch
pub struct GetProtocolConfigurationQuery {
    epoch: Epoch,
}

impl GetProtocolConfigurationQuery {
    /// Instantiate a query to get the current epoch settings
    pub fn epoch(epoch: Epoch) -> Self {
        Self { epoch }
    }
}

#[cfg_attr(target_family = "wasm", async_trait(?Send))]
#[cfg_attr(not(target_family = "wasm"), async_trait)]
impl AggregatorQuery for GetProtocolConfigurationQuery {
    type Response = Option<ProtocolConfigurationMessage>;
    type Body = ();

    fn method() -> QueryMethod {
        QueryMethod::Get
    }

    fn route(&self) -> String {
        format!("protocol-configuration/{}", self.epoch)
    }

    async fn handle_response(
        &self,
        context: QueryContext,
    ) -> AggregatorHttpClientResult<Self::Response> {
        debug!(context.logger, "GET: Retrieve protocol configuration"; "epoch" => %self.epoch);

        match context.response.status() {
            StatusCode::OK => match context.response.json::<ProtocolConfigurationMessage>().await {
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
    async fn test_ok_200() {
        let (server, client) = setup_server_and_client();
        let message_expected = ProtocolConfigurationMessage::dummy();
        let _server_mock = server.mock(|when, then| {
            when.path("/protocol-configuration/42");
            then.status(200).body(json!(message_expected).to_string());
        });

        let message = client
            .send(GetProtocolConfigurationQuery::epoch(Epoch(42)))
            .await
            .unwrap();

        assert_eq!(Some(message_expected), message);
    }

    #[tokio::test]
    async fn test_ok_404() {
        let (server, client) = setup_server_and_client();
        let _server_mock = server.mock(|when, then| {
            when.any_request();
            then.status(404);
        });

        let message = client
            .send(GetProtocolConfigurationQuery::epoch(Epoch(42)))
            .await
            .unwrap();

        assert_eq!(None, message);
    }

    #[tokio::test]
    async fn test_ko_500() {
        let (server, client) = setup_server_and_client();
        let _server_mock = server.mock(|when, then| {
            when.any_request();
            then.status(500).body("an error occurred");
        });

        let error = client
            .send(GetProtocolConfigurationQuery::epoch(Epoch(42)))
            .await
            .expect_err("should throw a error");

        assert_error_matches!(error, AggregatorHttpClientError::RemoteServerTechnical(_));
    }
}
