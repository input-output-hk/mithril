use anyhow::anyhow;
use async_trait::async_trait;
use reqwest::StatusCode;
use slog::debug;

use mithril_common::messages::EpochSettingsMessage;

use crate::query::{AggregatorQuery, QueryContext, QueryMethod};
use crate::{AggregatorHttpClientError, AggregatorHttpClientResult};

/// Query to get the current epoch settings
pub struct GetEpochSettingsQuery {}

impl GetEpochSettingsQuery {
    /// Instantiate a query to get the current epoch settings
    pub fn current() -> Self {
        Self {}
    }
}

#[cfg_attr(target_family = "wasm", async_trait(?Send))]
#[cfg_attr(not(target_family = "wasm"), async_trait)]
impl AggregatorQuery for GetEpochSettingsQuery {
    type Response = EpochSettingsMessage;
    type Body = ();

    fn method() -> QueryMethod {
        QueryMethod::Get
    }

    fn route(&self) -> String {
        "epoch-settings".to_string()
    }

    async fn handle_response(
        &self,
        context: QueryContext,
    ) -> AggregatorHttpClientResult<Self::Response> {
        debug!(context.logger, "/GET: Retrieve current epoch settings");

        match context.response.status() {
            StatusCode::OK => match context.response.json::<EpochSettingsMessage>().await {
                Ok(message) => Ok(message),
                Err(err) => Err(AggregatorHttpClientError::JsonParseFailed(anyhow!(err))),
            },
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
    async fn test_epoch_settings_ok_200() {
        let (server, client) = setup_server_and_client();
        let epoch_settings_expected = EpochSettingsMessage::dummy();
        let _server_mock = server.mock(|when, then| {
            when.path("/epoch-settings");
            then.status(200).body(json!(epoch_settings_expected).to_string());
        });

        let epoch_settings = client.send(GetEpochSettingsQuery::current()).await.unwrap();
        assert_eq!(epoch_settings_expected, epoch_settings);
    }

    #[tokio::test]
    async fn test_epoch_settings_ko_500() {
        let (server, client) = setup_server_and_client();
        let _server_mock = server.mock(|when, then| {
            when.any_request();
            then.status(500).body("an error occurred");
        });

        let error = client.send(GetEpochSettingsQuery::current()).await.unwrap_err();

        assert_error_matches!(error, AggregatorHttpClientError::RemoteServerTechnical(_));
    }
}
