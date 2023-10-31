use std::sync::Arc;

use anyhow::anyhow;
use async_trait::async_trait;
use slog::Logger;

use crate::aggregator_client::{AggregatorClient, AggregatorClientError, AggregatorRequest};
use crate::MithrilResult;
use mithril_common::crypto_helper::ProtocolGenesisVerificationKey;
use mithril_common::{
    certificate_chain::{
        CertificateRetriever, CertificateRetrieverError, CertificateVerifier,
        MithrilCertificateVerifier,
    },
    entities::Certificate,
    messages::CertificateMessage,
};
use slog_scope::{crit, debug};

/// Aggregator client for the Certificate
pub struct CertificateClient {
    aggregator_client: Arc<dyn AggregatorClient>,
    retriever: Arc<InternalCertificateRetriever>,
    verifier: Arc<dyn CertificateVerifier>,
    genesis_verification_key: ProtocolGenesisVerificationKey,
}

/// Internal type to implement the [InternalCertificateRetriever] trait and avoid a circular
/// dependency between the [CertificateClient] and the [MithrilCertificateVerifier] that need
/// a [CertificateRetriever] as a dependency.
struct InternalCertificateRetriever {
    aggregator_client: Arc<dyn AggregatorClient>,
}

impl CertificateClient {
    /// Constructor
    pub(crate) fn new(
        aggregator_client: Arc<dyn AggregatorClient>,
        genesis_verification_key: ProtocolGenesisVerificationKey,
        logger: Logger,
    ) -> Self {
        let retriever = Arc::new(InternalCertificateRetriever {
            aggregator_client: aggregator_client.clone(),
        });
        let verifier = Arc::new(MithrilCertificateVerifier::new(logger, retriever.clone()));

        Self {
            aggregator_client,
            retriever,
            verifier,
            genesis_verification_key,
        }
    }

    /// Constructor
    pub(crate) fn new_with_verifier(
        aggregator_client: Arc<dyn AggregatorClient>,
        genesis_verification_key: ProtocolGenesisVerificationKey,
        verifier: Arc<dyn CertificateVerifier>,
    ) -> Self {
        let retriever = Arc::new(InternalCertificateRetriever {
            aggregator_client: aggregator_client.clone(),
        });

        Self {
            aggregator_client,
            retriever,
            verifier,
            genesis_verification_key,
        }
    }

    /// Get a single certificate full information from the aggregator.
    pub async fn get(&self, certificate_hash: &str) -> MithrilResult<Option<Certificate>> {
        self.retriever.get(certificate_hash).await
    }

    /// Get given certificate if exist, validate the chain it belongs to, return the certificate if
    /// chain valid.
    pub async fn verify_chain(&self, certificate_hash: &str) -> MithrilResult<Certificate> {
        let certificate = self.get(certificate_hash).await?.ok_or(anyhow!(
            "No certificate exist for hash '{certificate_hash}'"
        ))?;

        self.verifier
            .verify_certificate_chain(certificate.clone(), &self.genesis_verification_key)
            .await?;

        Ok(certificate)
    }
}

impl InternalCertificateRetriever {
    async fn get(&self, certificate_hash: &str) -> MithrilResult<Option<Certificate>> {
        let response = self
            .aggregator_client
            .get_content(AggregatorRequest::GetCertificate {
                hash: certificate_hash.to_string(),
            })
            .await;

        match response {
            Err(AggregatorClientError::RemoteServerLogical(_)) => Ok(None),
            Err(e) => Err(e.into()),
            Ok(response) => {
                let message =
                    serde_json::from_str::<CertificateMessage>(&response).map_err(|e| {
                        crit!("Could not create certificate from API message: {e}.");
                        debug!("Certificate message = {response}");
                        e
                    })?;

                Ok(Some(message.try_into()?))
            }
        }
    }
}

#[async_trait]
impl CertificateRetriever for InternalCertificateRetriever {
    async fn get_certificate_details(
        &self,
        certificate_hash: &str,
    ) -> Result<Certificate, CertificateRetrieverError> {
        self.get(certificate_hash)
            .await
            .map_err(|e| CertificateRetrieverError(anyhow!(e)))?
            .ok_or(CertificateRetrieverError(anyhow!(format!(
                "Certificate does not exist: '{}'",
                certificate_hash
            ))))
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::entities::CertificateSignature;
    use mithril_common::messages::{CertificateMetadataMessagePart, SignerWithStakeMessagePart};
    use mithril_common::test_utils::{fake_data, fake_keys};

    use crate::aggregator_client::MockAggregatorHTTPClient;
    use crate::test_utils;

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
                    metadata: CertificateMetadataMessagePart {
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
                    aggregate_verification_key: certificate
                        .aggregate_verification_key
                        .try_into()
                        .unwrap(),
                    multi_signature,
                    genesis_signature,
                };
                Ok(serde_json::to_string(&message).unwrap())
            })
            .times(1);

        let certificate_client = CertificateClient::new(
            Arc::new(http_client),
            fake_keys::genesis_verification_key()[0].try_into().unwrap(),
            test_utils::test_logger(),
        );
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
                Err(AggregatorClientError::RemoteServerLogical(anyhow!(
                    "an error"
                )))
            })
            .times(1);

        let certificate_client = CertificateClient::new(
            Arc::new(http_client),
            fake_keys::genesis_verification_key()[0].try_into().unwrap(),
            test_utils::test_logger(),
        );
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
                Err(AggregatorClientError::RemoteServerTechnical(anyhow!(
                    "an error"
                )))
            })
            .times(1);

        let certificate_client = CertificateClient::new(
            Arc::new(http_client),
            fake_keys::genesis_verification_key()[0].try_into().unwrap(),
            test_utils::test_logger(),
        );
        certificate_client
            .get("cert-hash-123")
            .await
            .expect_err("The certificate client should fail here.");
    }
}
