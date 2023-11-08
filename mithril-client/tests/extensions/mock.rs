use async_trait::async_trait;
use mithril_client::{certificate_client::CertificateVerifier, MithrilCertificate, MithrilResult};
use mockall::mock;

mock! {
    pub CertificateVerifierImpl { }

    #[async_trait]
    impl CertificateVerifier for CertificateVerifierImpl {
        async fn verify_chain(&self, certificate: &MithrilCertificate) -> MithrilResult<()>;
    }
}
