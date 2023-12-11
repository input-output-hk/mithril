use std::sync::Arc;

use super::mock;
use mithril_client::certificate_client::CertificateVerifier;

pub struct FakeCertificateVerifier;

impl FakeCertificateVerifier {
    pub fn build_that_validate_any_certificate() -> Arc<dyn CertificateVerifier> {
        let mut mock_verifier = mock::MockCertificateVerifierImpl::new();
        mock_verifier.expect_verify_chain().returning(|_| Ok(()));

        Arc::new(mock_verifier)
    }
}
