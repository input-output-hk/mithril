use async_trait::async_trait;
use reqwest::StatusCode;
use slog::debug;

use mithril_common::messages::RegisterSignatureMessageHttp;

use crate::query::{AggregatorQuery, QueryContext, QueryLogFields, QueryMethod};
use crate::{AggregatorHttpClientError, AggregatorHttpClientResult};

/// Query to register a signature to a Mithril Aggregator.
pub struct PostRegisterSignatureQuery {
    message: RegisterSignatureMessageHttp,
}

impl PostRegisterSignatureQuery {
    /// Instantiate a new query to register a signature
    pub fn new(message: RegisterSignatureMessageHttp) -> Self {
        Self { message }
    }
}

#[cfg_attr(target_family = "wasm", async_trait(?Send))]
#[cfg_attr(not(target_family = "wasm"), async_trait)]
impl AggregatorQuery for PostRegisterSignatureQuery {
    type Response = ();
    type Body = RegisterSignatureMessageHttp;

    fn method() -> QueryMethod {
        QueryMethod::Post
    }

    fn route(&self) -> String {
        "register-signatures".to_string()
    }

    fn body(&self) -> Option<Self::Body> {
        Some(self.message.clone())
    }

    fn entry_log_additional_fields(&self) -> QueryLogFields {
        QueryLogFields::from([
            ("party_id", self.message.party_id.clone()),
            (
                "signed_entity",
                format!("{:?}", self.message.signed_entity_type),
            ),
        ])
    }

    async fn handle_response(
        &self,
        context: QueryContext,
    ) -> AggregatorHttpClientResult<Self::Response> {
        match context.response.status() {
            StatusCode::CREATED | StatusCode::ACCEPTED => Ok(()),
            StatusCode::GONE => {
                let root_cause = AggregatorHttpClientError::get_root_cause(context.response).await;
                debug!(context.logger, "Message already certified or expired"; "details" => &root_cause);

                Ok(())
            }
            _ => Err(context.unhandled_status_code().await),
        }
    }
}

#[cfg(test)]
mod tests {
    use httpmock::Method::POST;

    use mithril_common::entities::ClientError;
    use mithril_common::test::double::Dummy;

    use crate::test::{TestLogger, assert_error_matches, setup_server_and_client};

    use super::*;

    #[tokio::test]
    async fn test_register_signature_ok_201() {
        let expected_message = RegisterSignatureMessageHttp::dummy();
        let (server, client) = setup_server_and_client();
        let _server_mock = server.mock(|when, then| {
            when.method(POST)
                .path("/register-signatures")
                .body(serde_json::to_string(&expected_message).unwrap());
            then.status(201);
        });

        let register_signature =
            client.send(PostRegisterSignatureQuery::new(expected_message)).await;
        register_signature.expect("unexpected error");
    }

    #[tokio::test]
    async fn test_register_signature_ok_202() {
        let (server, client) = setup_server_and_client();
        let _server_mock = server.mock(|when, then| {
            when.method(POST).path("/register-signatures");
            then.status(202);
        });

        let register_signature = client
            .send(PostRegisterSignatureQuery::new(
                RegisterSignatureMessageHttp::dummy(),
            ))
            .await;
        register_signature.expect("unexpected error");
    }

    #[tokio::test]
    async fn test_register_signature_ko_400() {
        let (server, client) = setup_server_and_client();
        let _server_mock = server.mock(|when, then| {
            when.any_request();
            then.status(400)
                .body(serde_json::to_vec(&ClientError::dummy()).unwrap());
        });

        let error = client
            .send(PostRegisterSignatureQuery::new(
                RegisterSignatureMessageHttp::dummy(),
            ))
            .await
            .unwrap_err();

        assert_error_matches!(error, AggregatorHttpClientError::RemoteServerLogical(_));
    }

    #[tokio::test]
    async fn test_register_signature_ok_410_log_response_body() {
        let (logger, log_inspector) = TestLogger::memory();

        let (server, mut client) = setup_server_and_client();
        client.logger = logger;
        let _server_mock = server.mock(|when, then| {
            when.any_request();
            then.status(410).body(
                serde_json::to_vec(&ClientError::new(
                    "already_aggregated".to_string(),
                    "too late".to_string(),
                ))
                .unwrap(),
            );
        });

        client
            .send(PostRegisterSignatureQuery::new(
                RegisterSignatureMessageHttp::dummy(),
            ))
            .await
            .expect("Should not fail when status is 410 (GONE)");

        assert!(log_inspector.contains_log("already_aggregated"));
        assert!(log_inspector.contains_log("too late"));
    }

    #[tokio::test]
    async fn test_register_signature_ko_409() {
        let (server, client) = setup_server_and_client();
        let _server_mock = server.mock(|when, then| {
            when.any_request();
            then.status(409);
        });

        let error = client
            .send(PostRegisterSignatureQuery::new(
                RegisterSignatureMessageHttp::dummy(),
            ))
            .await
            .unwrap_err();

        assert_error_matches!(error, AggregatorHttpClientError::RemoteServerLogical(_));
    }

    #[tokio::test]
    async fn test_register_signature_ko_500() {
        let (server, client) = setup_server_and_client();
        let _server_mock = server.mock(|when, then| {
            when.any_request();
            then.status(500).body("an error occurred");
        });

        let error = client
            .send(PostRegisterSignatureQuery::new(
                RegisterSignatureMessageHttp::dummy(),
            ))
            .await
            .unwrap_err();

        assert_error_matches!(error, AggregatorHttpClientError::RemoteServerTechnical(_));
    }
}
