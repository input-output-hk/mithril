use anyhow::anyhow;
use async_trait::async_trait;
use reqwest::StatusCode;

use mithril_common::messages::CertificateMessage;

use crate::AggregatorHttpClientResult;
use crate::error::AggregatorHttpClientError;
use crate::query::{AggregatorQuery, QueryContext, QueryMethod};

/// Query to get the details of a certificate
pub struct GetCertificateQuery {
    hash: String,
}

impl GetCertificateQuery {
    /// Instantiate a query to get a certificate by hash
    pub fn by_hash<H: Into<String>>(hash: H) -> Self {
        Self { hash: hash.into() }
    }

    /// Instantiate a query to get the latest genesis certificate
    pub fn latest_genesis() -> Self {
        Self {
            hash: "genesis".to_string(),
        }
    }
}

#[cfg_attr(target_family = "wasm", async_trait(?Send))]
#[cfg_attr(not(target_family = "wasm"), async_trait)]
impl AggregatorQuery for GetCertificateQuery {
    type Response = Option<CertificateMessage>;
    type Body = ();

    fn method() -> QueryMethod {
        QueryMethod::Get
    }

    fn route(&self) -> String {
        format!("certificate/{}", self.hash)
    }

    async fn handle_response(
        &self,
        context: QueryContext,
    ) -> AggregatorHttpClientResult<Self::Response> {
        match context.response.status() {
            StatusCode::OK => match context.response.json::<CertificateMessage>().await {
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
    async fn test_certificates_details_ok_200() {
        let (server, client) = setup_server_and_client();
        let expected_message = CertificateMessage::dummy();
        let _server_mock = server.mock(|when, then| {
            when.path(format!("/certificate/{}", expected_message.hash));
            then.status(200).body(json!(expected_message).to_string());
        });

        let fetched_message = client
            .send(GetCertificateQuery::by_hash(&expected_message.hash))
            .await
            .unwrap();

        assert_eq!(Some(expected_message), fetched_message);
    }

    #[tokio::test]
    async fn test_certificates_details_ok_404() {
        let (server, client) = setup_server_and_client();
        let _server_mock = server.mock(|when, then| {
            when.any_request();
            then.status(404);
        });

        let fetched_message = client.send(GetCertificateQuery::by_hash("whatever")).await.unwrap();

        assert_eq!(None, fetched_message);
    }

    #[tokio::test]
    async fn test_certificates_details_ko_500() {
        let (server, client) = setup_server_and_client();
        let _server_mock = server.mock(|when, then| {
            when.any_request();
            then.status(500).body("an error occurred");
        });

        let error = client
            .send(GetCertificateQuery::by_hash("whatever"))
            .await
            .unwrap_err();

        assert_error_matches!(error, AggregatorHttpClientError::RemoteServerTechnical(_));
    }

    #[tokio::test]
    async fn test_latest_genesis_ok_200() {
        let (server, client) = setup_server_and_client();
        let genesis_message = CertificateMessage::dummy();
        let _server_mock = server.mock(|when, then| {
            when.path("/certificate/genesis");
            then.status(200).body(json!(genesis_message).to_string());
        });

        let fetched = client.send(GetCertificateQuery::latest_genesis()).await.unwrap();

        assert_eq!(Some(genesis_message), fetched);
    }

    #[tokio::test]
    async fn test_latest_genesis_ok_404() {
        let (server, client) = setup_server_and_client();
        let _server_mock = server.mock(|when, then| {
            when.any_request();
            then.status(404);
        });

        let fetched = client.send(GetCertificateQuery::latest_genesis()).await.unwrap();

        assert_eq!(None, fetched);
    }

    #[tokio::test]
    async fn test_latest_genesis_ko_500() {
        let (server, client) = setup_server_and_client();
        let _server_mock = server.mock(|when, then| {
            when.any_request();
            then.status(500).body("an error occurred");
        });

        let error = client.send(GetCertificateQuery::latest_genesis()).await.unwrap_err();

        assert_error_matches!(error, AggregatorHttpClientError::RemoteServerTechnical(_));
    }
}
