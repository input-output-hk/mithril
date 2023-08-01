use std::sync::Arc;

use async_trait::async_trait;

use mithril_common::{
    certificate_chain::{CertificateRetriever, CertificateRetrieverError},
    entities::Certificate,
    messages::{CertificateMessage, TryFromMessageAdapter},
    StdResult,
};
use slog_scope::{crit, debug};

use crate::FromCertificateMessageAdapter;

use super::{AggregatorClient, AggregatorHTTPClientError};

/// Aggregator client for the Certificate
pub struct CertificateClient {
    http_client: Arc<dyn AggregatorClient>,
}

impl CertificateClient {
    /// Constructor
    pub fn new(http_client: Arc<dyn AggregatorClient>) -> Self {
        Self { http_client }
    }

    /// Get a single certificate full information from the aggregator.
    pub async fn get(&self, certificate_hash: &str) -> StdResult<Option<Certificate>> {
        let url = format!("certificate/{certificate_hash}");
        let response = self.http_client.get_content(&url).await;

        match response {
            Err(e) if matches!(e, AggregatorHTTPClientError::RemoteServerLogical(_)) => Ok(None),
            Err(e) => Err(e.into()),
            Ok(response) => {
                let message =
                    serde_json::from_str::<CertificateMessage>(&response).map_err(|e| {
                        crit!("Could not create certificate from API message: {e}.");
                        debug!("Certificate message = {response}");
                        e
                    })?;

                Ok(Some(FromCertificateMessageAdapter::try_adapt(message)?))
            }
        }
    }
}

#[async_trait]
impl CertificateRetriever for CertificateClient {
    async fn get_certificate_details(
        &self,
        certificate_hash: &str,
    ) -> Result<Certificate, CertificateRetrieverError> {
        self.get(certificate_hash)
            .await
            .map_err(|e| CertificateRetrieverError::General(format!("{e}")))?
            .ok_or_else(|| {
                CertificateRetrieverError::General(format!(
                    "Certificate '{certificate_hash}' not found"
                ))
            })
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::entities::CertificateSignature;
    use mithril_common::messages::{CertificateMetadataMessage, SignerWithStakeMessagePart};
    use mithril_common::test_utils::fake_data;

    use crate::aggregator_client::MockAggregatorHTTPClient;

    use super::*;

    #[tokio::test]
    async fn test_show_ok_some() {
        let mut http_client = MockAggregatorHTTPClient::new();
        let certificate_hash = "cert-hash-123".to_string();
        let certificate = fake_data::certificate(certificate_hash.clone());
        let expected_certificate = certificate.clone();
        let previous_hash = certificate.previous_hash.clone();
        http_client
            .expect_get_content()
            .return_once(move |_| {
                let (multi_signature, genesis_signature) = match certificate.signature {
                    CertificateSignature::GenesisSignature(signature) => {
                        (String::new(), signature.try_into().unwrap())
                    }
                    CertificateSignature::MultiSignature(signature) => {
                        (signature.to_json_hex().unwrap(), String::new())
                    }
                };

                let message = CertificateMessage {
                    hash: certificate_hash.clone(),
                    previous_hash: previous_hash.clone(),
                    beacon: certificate.beacon.clone(),
                    metadata: CertificateMetadataMessage {
                        protocol_version: certificate.metadata.protocol_version.clone(),
                        protocol_parameters: certificate.metadata.protocol_parameters.clone(),
                        initiated_at: certificate.metadata.initiated_at,
                        sealed_at: certificate.metadata.sealed_at,
                        signers: SignerWithStakeMessagePart::from_signers(
                            certificate.metadata.signers.clone(),
                        ),
                    },
                    protocol_message: certificate.protocol_message.clone(),
                    signed_message: certificate.signed_message.clone(),
                    aggregate_verification_key: certificate.aggregate_verification_key.clone(),
                    multi_signature,
                    genesis_signature,
                };
                Ok(serde_json::to_string(&message).unwrap())
            })
            .times(1);

        let certificate_client = CertificateClient::new(Arc::new(http_client));
        let cert = certificate_client
            .get("cert-hash-123")
            .await
            .unwrap()
            .expect("The certificate should be found");

        assert_eq!(expected_certificate, cert);
    }

    #[tokio::test]
    async fn test_show_ok_none() {
        let mut http_client = MockAggregatorHTTPClient::new();
        http_client
            .expect_get_content()
            .return_once(move |_| {
                Err(AggregatorHTTPClientError::RemoteServerLogical(String::new()))
            })
            .times(1);

        let certificate_client = CertificateClient::new(Arc::new(http_client));
        assert!(certificate_client
            .get("cert-hash-123")
            .await
            .unwrap()
            .is_none());
    }

    #[tokio::test]
    async fn test_show_ko() {
        let mut http_client = MockAggregatorHTTPClient::new();
        http_client
            .expect_get_content()
            .return_once(move |_| {
                Err(AggregatorHTTPClientError::RemoteServerTechnical(
                    String::new(),
                ))
            })
            .times(1);

        let certificate_client = CertificateClient::new(Arc::new(http_client));
        certificate_client
            .get("cert-hash-123")
            .await
            .expect_err("The certificate client should fail here.");
    }
}
