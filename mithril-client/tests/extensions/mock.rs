use async_trait::async_trait;
use mithril_client::{common::CertificateVerifier, MithrilCertificate};
use mithril_common::{
    crypto_helper::ProtocolGenesisVerificationKey, entities::ProtocolMessage, StdResult,
};
use mockall::mock;

mock! {
    pub CertificateVerifierImpl { }

    #[async_trait]
    impl CertificateVerifier for CertificateVerifierImpl {
        async fn verify_genesis_certificate(
            &self,
            genesis_certificate: &MithrilCertificate,
            genesis_verification_key: &ProtocolGenesisVerificationKey,
        ) -> StdResult<()>;

        async fn verify_certificate(
            &self,
            certificate: &MithrilCertificate,
            genesis_verification_key: &ProtocolGenesisVerificationKey,
        ) -> StdResult<Option<MithrilCertificate>>;

        async fn verify_certificate_chain(
            &self,
            certificate: MithrilCertificate,
            genesis_verification_key: &ProtocolGenesisVerificationKey,
        ) -> StdResult<()>;

        fn verify_protocol_message(
            &self,
            protocol_message: &ProtocolMessage,
            certificate: &MithrilCertificate,
        ) -> bool;
    }
}
