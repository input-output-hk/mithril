use anyhow::{Context, anyhow};
use async_trait::async_trait;
use mithril_common::certificate_chain::{CertificateRetriever, CertificateRetrieverError};
use mithril_common::entities::Certificate;

use crate::AggregatorHttpClient;
use crate::query::GetCertificateQuery;

#[cfg_attr(target_family = "wasm", async_trait(?Send))]
#[cfg_attr(not(target_family = "wasm"), async_trait)]
impl CertificateRetriever for AggregatorHttpClient {
    async fn get_certificate_details(
        &self,
        certificate_hash: &str,
    ) -> Result<Certificate, CertificateRetrieverError> {
        let message = self
            .send(GetCertificateQuery::by_hash(certificate_hash))
            .await
            .with_context(|| {
                format!("Failed to retrieve certificate with hash: '{certificate_hash}'")
            })
            .map_err(CertificateRetrieverError)?
            .ok_or(CertificateRetrieverError(anyhow!(
                "Certificate does not exist: '{certificate_hash}'"
            )))?;

        message.try_into().map_err(CertificateRetrieverError)
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::{
        entities::ServerError, messages::CertificateMessage, test::double::Dummy,
    };

    use super::*;

    #[tokio::test]
    async fn test_retrieve_certificate_that_exist() {
        let certificate_message = CertificateMessage::dummy();
        let expected_certificate = certificate_message.clone().try_into().unwrap();

        let (server, client) = crate::test::setup_server_and_client();
        server.mock(|when, then| {
            when.method(httpmock::Method::GET)
                .path(format!("/certificate/{}", certificate_message.hash));
            then.status(200)
                .body(serde_json::to_string(&certificate_message).unwrap());
        });

        let certificate = client
            .get_certificate_details(&certificate_message.hash)
            .await
            .unwrap();

        assert_eq!(certificate, expected_certificate);
    }

    #[tokio::test]
    async fn test_retrieve_certificate_that_does_not_exist() {
        let (server, client) = crate::test::setup_server_and_client();
        server.mock(|when, then| {
            when.method(httpmock::Method::GET);
            then.status(404);
        });

        let error = client.get_certificate_details("whatever").await.unwrap_err();

        assert!(
            format!("{error:?}").contains("Certificate does not exist"),
            "Error message should contain 'Certificate does not exist', error:\n{error:?}",
        );
    }

    #[tokio::test]
    async fn test_retrieve_certificate_when_request_fails() {
        let (server, client) = crate::test::setup_server_and_client();
        server.mock(|when, then| {
            when.method(httpmock::Method::GET);
            then.status(500)
                .body(serde_json::to_string(&ServerError::new("an error")).unwrap());
        });

        let error = client.get_certificate_details("whatever").await.unwrap_err();

        assert!(
            format!("{error:?}").contains("Failed to retrieve certificate with hash"),
            "Error message should contain 'Failed to retrieve certificate with hash', error:\n{error:?}",
        );
    }
}
