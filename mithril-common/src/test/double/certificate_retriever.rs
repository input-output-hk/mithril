//! A module used for a fake implementation of a certificate chain retriever
//!

use anyhow::anyhow;
use async_trait::async_trait;
use std::collections::HashMap;
use tokio::sync::RwLock;

use crate::certificate_chain::{CertificateRetriever, CertificateRetrieverError};
use crate::entities::Certificate;

/// A fake [CertificateRetriever] that returns a [Certificate] given its hash
pub struct FakeCertificaterRetriever {
    certificates_map: RwLock<HashMap<String, Certificate>>,
}

impl FakeCertificaterRetriever {
    /// Create a new [FakeCertificaterRetriever]
    pub fn from_certificates(certificates: &[Certificate]) -> Self {
        let certificates_map = certificates
            .iter()
            .map(|certificate| (certificate.hash.clone(), certificate.clone()))
            .collect::<HashMap<_, _>>();
        let certificates_map = RwLock::new(certificates_map);

        Self { certificates_map }
    }
}

#[cfg_attr(target_family = "wasm", async_trait(?Send))]
#[cfg_attr(not(target_family = "wasm"), async_trait)]
impl CertificateRetriever for FakeCertificaterRetriever {
    async fn get_certificate_details(
        &self,
        certificate_hash: &str,
    ) -> Result<Certificate, CertificateRetrieverError> {
        let certificates_map = self.certificates_map.read().await;
        certificates_map
            .get(certificate_hash)
            .cloned()
            .ok_or_else(|| CertificateRetrieverError(anyhow!("Certificate not found")))
    }
}

#[cfg(test)]
mod tests {
    use crate::test::double::fake_data;

    use super::*;

    #[tokio::test]
    async fn fake_certificate_retriever_retrieves_existing_certificate() {
        let certificate = fake_data::certificate("certificate-hash-123".to_string());
        let certificate_hash = certificate.hash.clone();
        let certificate_retriever =
            FakeCertificaterRetriever::from_certificates(std::slice::from_ref(&certificate));

        let retrieved_certificate = certificate_retriever
            .get_certificate_details(&certificate_hash)
            .await
            .expect("Should retrieve certificate");

        assert_eq!(retrieved_certificate, certificate);
    }

    #[tokio::test]
    async fn test_fake_certificate_fails_retrieving_unknown_certificate() {
        let certificate = fake_data::certificate("certificate-hash-123".to_string());
        let certificate_retriever = FakeCertificaterRetriever::from_certificates(&[certificate]);

        let retrieved_certificate = certificate_retriever
            .get_certificate_details("certificate-hash-not-found")
            .await;

        retrieved_certificate.expect_err("get_certificate_details should fail");
    }
}
