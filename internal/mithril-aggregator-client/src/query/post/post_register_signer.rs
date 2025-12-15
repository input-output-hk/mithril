use async_trait::async_trait;
use mithril_common::messages::RegisterSignerMessage;
use reqwest::StatusCode;

use crate::AggregatorHttpClientResult;
use crate::query::{AggregatorQuery, QueryContext, QueryLogFields, QueryMethod};

/// Query to register a signer to a Mithril Aggregator.
pub struct PostRegisterSignerQuery {
    message: RegisterSignerMessage,
}

impl PostRegisterSignerQuery {
    /// Instantiate a new query to register a signer
    pub fn new(message: RegisterSignerMessage) -> Self {
        Self { message }
    }
}

#[cfg_attr(target_family = "wasm", async_trait(?Send))]
#[cfg_attr(not(target_family = "wasm"), async_trait)]
impl AggregatorQuery for PostRegisterSignerQuery {
    type Response = ();
    type Body = RegisterSignerMessage;

    fn method() -> QueryMethod {
        QueryMethod::Post
    }

    fn route(&self) -> String {
        "register-signer".to_string()
    }

    fn body(&self) -> Option<Self::Body> {
        Some(self.message.clone())
    }

    fn entry_log_additional_fields(&self) -> QueryLogFields {
        QueryLogFields::from([
            ("epoch", format!("{}", *self.message.epoch)),
            ("party_id", self.message.party_id.clone()),
        ])
    }

    async fn handle_response(
        &self,
        context: QueryContext,
    ) -> AggregatorHttpClientResult<Self::Response> {
        match context.response.status() {
            StatusCode::CREATED => Ok(()),
            _ => Err(context.unhandled_status_code().await),
        }
    }
}

#[cfg(test)]
mod tests {
    use httpmock::Method::POST;
    use mithril_common::entities::ClientError;
    use mithril_common::test::double::Dummy;

    use super::*;
    use crate::AggregatorHttpClientError;
    use crate::test::{assert_error_matches, setup_server_and_client};

    #[tokio::test]
    async fn test_register_signer_ok_201() {
        let expected_message = RegisterSignerMessage::dummy();
        let (server, client) = setup_server_and_client();
        let _server_mock = server.mock(|when, then| {
            when.method(POST)
                .path("/register-signer")
                .body(serde_json::to_string(&expected_message).unwrap());
            then.status(201);
        });

        let register_signer = client.send(PostRegisterSignerQuery::new(expected_message)).await;
        register_signer.expect("unexpected error");
    }

    #[tokio::test]
    async fn test_register_signer_ko_400() {
        let (server, client) = setup_server_and_client();
        let _server_mock = server.mock(|when, then| {
            when.any_request();
            then.status(400)
                .body(serde_json::to_vec(&ClientError::dummy()).unwrap());
        });

        let error = client
            .send(PostRegisterSignerQuery::new(RegisterSignerMessage::dummy()))
            .await
            .unwrap_err();

        assert_error_matches!(error, AggregatorHttpClientError::RemoteServerLogical(_));
    }

    #[tokio::test]
    async fn test_register_signer_ko_500() {
        let (server, client) = setup_server_and_client();
        let _server_mock = server.mock(|when, then| {
            when.any_request();
            then.status(500).body("an error occurred");
        });

        let error = client
            .send(PostRegisterSignerQuery::new(RegisterSignerMessage::dummy()))
            .await
            .unwrap_err();

        assert_error_matches!(error, AggregatorHttpClientError::RemoteServerTechnical(_));
    }
}
