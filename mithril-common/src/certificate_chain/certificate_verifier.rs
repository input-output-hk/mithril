//! A module used to validate the Certificate Chain created by an aggregator
//!
use anyhow::{anyhow, Context};
use async_trait::async_trait;
use hex::ToHex;
use slog::{debug, Logger};
use std::sync::Arc;
use thiserror::Error;

use super::CertificateRetriever;
use crate::crypto_helper::{
    ProtocolAggregateVerificationKey, ProtocolGenesisError, ProtocolGenesisVerificationKey,
    ProtocolMultiSignature,
};
use crate::entities::{
    Certificate, CertificateSignature, ProtocolMessagePartKey, ProtocolParameters,
};
use crate::logging::LoggerExtensions;
use crate::StdResult;

#[cfg(test)]
use mockall::automock;

/// [CertificateVerifier] related errors.
#[derive(Error, Debug)]
pub enum CertificateVerifierError {
    /// Error raised when the multi signatures verification fails.
    #[error("multi signature verification failed: '{0}'")]
    VerifyMultiSignature(String),

    /// Error raised when the Genesis Signature stored in a [Certificate] is invalid.
    #[error("certificate genesis error")]
    CertificateGenesis(#[from] ProtocolGenesisError),

    /// Error raised when the hash stored in a [Certificate] doesn't match a recomputed hash.
    #[error("certificate hash unmatch error")]
    CertificateHashUnmatch,

    /// Error raised when validating the certificate chain if a previous [Certificate] hash isn't
    /// equal to the current certificate `previous_hash`.
    #[error("certificate chain previous hash unmatch error")]
    CertificateChainPreviousHashUnmatch,

    /// Error raised when validating the certificate chain if the current [Certificate]
    /// `signed_message` doesn't match the hash of the `protocol_message` of the current certificate
    #[error("certificate protocol message unmatch error")]
    CertificateProtocolMessageUnmatch,

    /// Error raised when validating the certificate chain if the current [Certificate]
    /// `aggregate_verification_key` doesn't match the signed `next_aggregate_verification_key` of the previous certificate
    /// (if the certificates are on different epoch) or the `aggregate_verification_key` of the previous certificate
    /// (if the certificates are on the same epoch).
    #[error("certificate chain AVK unmatch error")]
    CertificateChainAVKUnmatch,

    /// Error raised when validating the certificate chain if the current [Certificate]
    /// `protocol_parameters` don't match the signed `next_protocol_parameters` of the previous certificate
    /// (if the certificates are on different epoch) or the `protocol_parameters` of the previous certificate
    /// (if the certificates are on the same epoch).
    #[error("certificate chain protocol parameters unmatch error")]
    CertificateChainProtocolParametersUnmatch,

    /// Error raised when validating the certificate chain if the current [Certificate]
    /// `epoch` doesn't match the signed `current_epoch` of the current certificate
    #[error("certificate epoch unmatch error")]
    CertificateEpochUnmatch,

    /// Error raised when validating the certificate chain if the current [Certificate]
    /// `epoch` is neither equal to the previous certificate `epoch` nor to the previous certificate `epoch - 1`
    #[error("certificate chain missing epoch error")]
    CertificateChainMissingEpoch,

    /// Error raised when validating the certificate chain if the chain loops.
    #[error("certificate chain infinite loop error")]
    CertificateChainInfiniteLoop,

    /// Error raised when [CertificateVerifier::verify_genesis_certificate] was called with a
    /// certificate that's not a genesis certificate.
    #[error("can't validate genesis certificate: given certificate isn't a genesis certificate")]
    InvalidGenesisCertificateProvided,

    /// Error raised when [CertificateVerifier::verify_standard_certificate] was called with a
    /// certificate that's not a standard certificate.
    #[error("can't validate standard certificate: given certificate isn't a standard certificate")]
    InvalidStandardCertificateProvided,
}

/// CertificateVerifier is the cryptographic engine in charge of verifying multi signatures and
/// [certificates](Certificate)
#[cfg_attr(test, automock)]
#[cfg_attr(target_family = "wasm", async_trait(?Send))]
#[cfg_attr(not(target_family = "wasm"), async_trait)]
pub trait CertificateVerifier: Send + Sync {
    /// Verify Genesis certificate
    async fn verify_genesis_certificate(
        &self,
        genesis_certificate: &Certificate,
        genesis_verification_key: &ProtocolGenesisVerificationKey,
    ) -> StdResult<()>;

    /// Verify Standard certificate
    async fn verify_standard_certificate(
        &self,
        certificate: &Certificate,
        previous_certificate: &Certificate,
    ) -> StdResult<()>;

    /// Verify if a Certificate is valid and returns the previous Certificate in the chain if exists
    async fn verify_certificate(
        &self,
        certificate: &Certificate,
        genesis_verification_key: &ProtocolGenesisVerificationKey,
    ) -> StdResult<Option<Certificate>>;

    /// Verify that the Certificate Chain associated to a Certificate is valid
    async fn verify_certificate_chain(
        &self,
        certificate: Certificate,
        genesis_verification_key: &ProtocolGenesisVerificationKey,
    ) -> StdResult<()> {
        let mut certificate = certificate;
        while let Some(previous_certificate) = self
            .verify_certificate(&certificate, genesis_verification_key)
            .await?
        {
            certificate = previous_certificate;
        }

        Ok(())
    }
}

/// MithrilCertificateVerifier is an implementation of the CertificateVerifier
pub struct MithrilCertificateVerifier {
    logger: Logger,
    certificate_retriever: Arc<dyn CertificateRetriever>,
}

impl MithrilCertificateVerifier {
    /// MithrilCertificateVerifier factory
    pub fn new(logger: Logger, certificate_retriever: Arc<dyn CertificateRetriever>) -> Self {
        debug!(logger, "New MithrilCertificateVerifier created");
        Self {
            logger: logger.new_with_component_name::<Self>(),
            certificate_retriever,
        }
    }

    async fn fetch_previous_certificate(
        &self,
        certificate: &Certificate,
    ) -> StdResult<Certificate> {
        self.certificate_retriever
            .get_certificate_details(&certificate.previous_hash)
            .await
            .map_err(|e| anyhow!(e))
            .with_context(|| "Can not retrieve previous certificate during verification")
    }

    fn verify_multi_signature(
        &self,
        message: &[u8],
        multi_signature: &ProtocolMultiSignature,
        aggregate_verification_key: &ProtocolAggregateVerificationKey,
        protocol_parameters: &ProtocolParameters,
    ) -> Result<(), CertificateVerifierError> {
        debug!(
            self.logger,
            "Verify multi signature for {:?}",
            message.encode_hex::<String>()
        );

        multi_signature
            .verify(
                message,
                aggregate_verification_key,
                &protocol_parameters.to_owned().into(),
            )
            .map_err(|e| CertificateVerifierError::VerifyMultiSignature(e.to_string()))
    }

    fn verify_is_not_in_infinite_loop(&self, certificate: &Certificate) -> StdResult<()> {
        if certificate.is_chaining_to_itself() {
            return Err(anyhow!(
                CertificateVerifierError::CertificateChainInfiniteLoop
            ));
        }

        Ok(())
    }

    fn verify_hash_matches_content(&self, certificate: &Certificate) -> StdResult<()> {
        if certificate.compute_hash() != certificate.hash {
            return Err(anyhow!(CertificateVerifierError::CertificateHashUnmatch));
        }

        Ok(())
    }

    fn verify_previous_hash_matches_previous_certificate_hash(
        &self,
        certificate: &Certificate,
        previous_certificate: &Certificate,
    ) -> StdResult<()> {
        if previous_certificate.hash != certificate.previous_hash {
            return Err(anyhow!(
                CertificateVerifierError::CertificateChainPreviousHashUnmatch
            ));
        }

        Ok(())
    }

    fn verify_signed_message_matches_hashed_protocol_message(
        &self,
        certificate: &Certificate,
    ) -> StdResult<()> {
        if certificate.protocol_message.compute_hash() != certificate.signed_message {
            return Err(anyhow!(
                CertificateVerifierError::CertificateProtocolMessageUnmatch
            ));
        }

        Ok(())
    }

    fn verify_epoch_matches_protocol_message(&self, certificate: &Certificate) -> StdResult<()> {
        if let Some(signed_epoch) = &certificate
            .protocol_message
            .get_message_part(&ProtocolMessagePartKey::CurrentEpoch)
        {
            if **signed_epoch == certificate.epoch.to_string() {
                return Ok(());
            }
        }

        Err(anyhow!(CertificateVerifierError::CertificateEpochUnmatch))
    }

    fn verify_epoch_chaining(
        &self,
        certificate: &Certificate,
        previous_certificate: &Certificate,
    ) -> StdResult<()> {
        if certificate.epoch.has_gap_with(&previous_certificate.epoch) {
            return Err(anyhow!(
                CertificateVerifierError::CertificateChainMissingEpoch
            ));
        }

        Ok(())
    }

    fn verify_aggregate_verification_key_chaining(
        &self,
        certificate: &Certificate,
        previous_certificate: &Certificate,
    ) -> StdResult<()> {
        let previous_certificate_has_same_epoch = previous_certificate.epoch == certificate.epoch;
        let certificate_has_valid_aggregate_verification_key =
            if previous_certificate_has_same_epoch {
                previous_certificate.aggregate_verification_key
                    == certificate.aggregate_verification_key
            } else {
                match &previous_certificate
                    .protocol_message
                    .get_message_part(&ProtocolMessagePartKey::NextAggregateVerificationKey)
                {
                    Some(previous_certificate_next_aggregate_verification_key) => {
                        **previous_certificate_next_aggregate_verification_key
                            == certificate
                                .aggregate_verification_key
                                .to_json_hex()
                                .with_context(|| {
                                    format!(
                                    "aggregate verification key to string conversion error for certificate: `{}`",
                                    certificate.hash
                                )
                                })?
                    }
                    None => false,
                }
            };
        if !certificate_has_valid_aggregate_verification_key {
            debug!(
                self.logger,
                "Previous certificate {:#?}", previous_certificate
            );
            return Err(anyhow!(
                CertificateVerifierError::CertificateChainAVKUnmatch
            ));
        }

        Ok(())
    }

    fn verify_protocol_parameters_chaining(
        &self,
        certificate: &Certificate,
        previous_certificate: &Certificate,
    ) -> StdResult<()> {
        let previous_certificate_has_same_epoch = previous_certificate.epoch == certificate.epoch;
        let certificate_has_valid_protocol_parameters = if previous_certificate_has_same_epoch {
            previous_certificate.metadata.protocol_parameters
                == certificate.metadata.protocol_parameters
        } else {
            match &previous_certificate
                .protocol_message
                .get_message_part(&ProtocolMessagePartKey::NextProtocolParameters)
            {
                Some(previous_certificate_next_protocol_parameters) => {
                    **previous_certificate_next_protocol_parameters
                        == certificate.metadata.protocol_parameters.compute_hash()
                }
                None => false,
            }
        };
        if !certificate_has_valid_protocol_parameters {
            debug!(
                self.logger,
                "Previous certificate {:#?}", previous_certificate
            );
            return Err(anyhow!(
                CertificateVerifierError::CertificateChainProtocolParametersUnmatch
            ));
        }

        Ok(())
    }
}

#[cfg_attr(target_family = "wasm", async_trait(?Send))]
#[cfg_attr(not(target_family = "wasm"), async_trait)]
impl CertificateVerifier for MithrilCertificateVerifier {
    async fn verify_genesis_certificate(
        &self,
        genesis_certificate: &Certificate,
        genesis_verification_key: &ProtocolGenesisVerificationKey,
    ) -> StdResult<()> {
        let genesis_signature = match &genesis_certificate.signature {
            CertificateSignature::GenesisSignature(signature) => Ok(signature),
            _ => Err(CertificateVerifierError::InvalidGenesisCertificateProvided),
        }?;
        self.verify_hash_matches_content(genesis_certificate)?;
        self.verify_signed_message_matches_hashed_protocol_message(genesis_certificate)?;
        genesis_verification_key
            .verify(
                genesis_certificate.signed_message.as_bytes(),
                genesis_signature,
            )
            .with_context(|| "Certificate verifier failed verifying a genesis certificate")?;
        self.verify_epoch_matches_protocol_message(genesis_certificate)?;

        Ok(())
    }

    async fn verify_standard_certificate(
        &self,
        certificate: &Certificate,
        previous_certificate: &Certificate,
    ) -> StdResult<()> {
        let multi_signature = match &certificate.signature {
            CertificateSignature::MultiSignature(_, signature) => Ok(signature),
            _ => Err(CertificateVerifierError::InvalidStandardCertificateProvided),
        }?;
        self.verify_is_not_in_infinite_loop(certificate)?;
        self.verify_hash_matches_content(certificate)?;
        self.verify_signed_message_matches_hashed_protocol_message(certificate)?;
        self.verify_multi_signature(
            certificate.signed_message.as_bytes(),
            multi_signature,
            &certificate.aggregate_verification_key,
            &certificate.metadata.protocol_parameters,
        )?;
        self.verify_epoch_matches_protocol_message(certificate)?;
        self.verify_epoch_chaining(certificate, previous_certificate)?;
        self.verify_previous_hash_matches_previous_certificate_hash(
            certificate,
            previous_certificate,
        )?;
        self.verify_aggregate_verification_key_chaining(certificate, previous_certificate)?;
        self.verify_protocol_parameters_chaining(certificate, previous_certificate)?;

        Ok(())
    }

    /// Verify a certificate
    async fn verify_certificate(
        &self,
        certificate: &Certificate,
        genesis_verification_key: &ProtocolGenesisVerificationKey,
    ) -> StdResult<Option<Certificate>> {
        debug!(
            self.logger, "Verifying certificate";
            "certificate_hash" => &certificate.hash,
            "certificate_previous_hash" => &certificate.previous_hash,
            "certificate_epoch" => ?certificate.epoch,
            "certificate_signed_entity_type" => ?certificate.signed_entity_type(),
        );

        match &certificate.signature {
            CertificateSignature::GenesisSignature(_) => {
                self.verify_genesis_certificate(certificate, genesis_verification_key)
                    .await?;

                Ok(None)
            }
            CertificateSignature::MultiSignature(_, _) => {
                let previous_certificate = self.fetch_previous_certificate(certificate).await?;
                self.verify_standard_certificate(certificate, &previous_certificate)
                    .await?;

                Ok(Some(previous_certificate))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use async_trait::async_trait;
    use mockall::mock;
    use tokio::sync::Mutex;

    use super::CertificateRetriever;
    use super::*;

    use crate::certificate_chain::{CertificateRetrieverError, FakeCertificaterRetriever};
    use crate::crypto_helper::{tests_setup::*, ProtocolClerk};
    use crate::test_utils::{
        CertificateChainBuilder, CertificateChainBuilderContext, MithrilFixtureBuilder, TestLogger,
    };

    macro_rules! assert_error_matches {
        ( $expected_error:path, $error:expr ) => {{
            let error = $error
                .downcast_ref::<CertificateVerifierError>()
                .expect("Can not downcast to `CertificateVerifierError`.");
            let expected_error = $expected_error;

            assert!(
                matches!(error, $expected_error),
                "unexpected error type: got {error:?}, want {expected_error:?}"
            );
        }};
    }

    mock! {
        pub CertificateRetrieverImpl { }

        #[async_trait]
        impl CertificateRetriever for CertificateRetrieverImpl {

            async fn get_certificate_details(
                &self,
                certificate_hash: &str,
            ) -> Result<Certificate, CertificateRetrieverError>;
        }
    }

    struct MockDependencyInjector {
        mock_certificate_retriever: MockCertificateRetrieverImpl,
    }

    impl MockDependencyInjector {
        fn new() -> MockDependencyInjector {
            MockDependencyInjector {
                mock_certificate_retriever: MockCertificateRetrieverImpl::new(),
            }
        }

        fn build_certificate_verifier(self) -> MithrilCertificateVerifier {
            MithrilCertificateVerifier::new(
                TestLogger::stdout(),
                Arc::new(self.mock_certificate_retriever),
            )
        }
    }

    #[test]
    fn verify_multi_signature_success() {
        let protocol_parameters = setup_protocol_parameters();
        let fixture = MithrilFixtureBuilder::default()
            .with_signers(5)
            .with_protocol_parameters(protocol_parameters.into())
            .build();
        let signers = fixture.signers_fixture();
        let message_hash = setup_message().compute_hash().as_bytes().to_vec();

        let single_signatures = signers
            .iter()
            .filter_map(|s| s.protocol_signer.sign(&message_hash))
            .collect::<Vec<_>>();

        let first_signer = &signers[0].protocol_signer;
        let clerk = ProtocolClerk::from_signer(first_signer);
        let aggregate_verification_key = clerk.compute_avk().into();
        let multi_signature = clerk
            .aggregate(&single_signatures, &message_hash)
            .unwrap()
            .into();

        let verifier = MithrilCertificateVerifier::new(
            TestLogger::stdout(),
            Arc::new(MockCertificateRetrieverImpl::new()),
        );
        let message_tampered = message_hash[1..].to_vec();
        assert!(
            verifier
                .verify_multi_signature(
                    &message_tampered,
                    &multi_signature,
                    &aggregate_verification_key,
                    &fixture.protocol_parameters(),
                )
                .is_err(),
            "multi signature verification should have failed"
        );
        verifier
            .verify_multi_signature(
                &message_hash,
                &multi_signature,
                &aggregate_verification_key,
                &fixture.protocol_parameters(),
            )
            .expect("multi signature verification should have succeeded");
    }

    #[tokio::test]
    async fn verify_genesis_certificate_success() {
        let (total_certificates, certificates_per_epoch) = (5, 1);
        let (fake_certificates, genesis_verifier) =
            setup_certificate_chain(total_certificates, certificates_per_epoch);
        let verifier = MockDependencyInjector::new().build_certificate_verifier();
        let genesis_certificate = fake_certificates.last().unwrap().clone();

        let verify = verifier
            .verify_genesis_certificate(
                &genesis_certificate,
                &genesis_verifier.to_verification_key(),
            )
            .await;

        verify.expect("verify_genesis_certificate should not fail");
    }

    #[tokio::test]
    async fn verify_genesis_certificate_fails_if_is_not_genesis() {
        let (total_certificates, certificates_per_epoch) = (5, 1);
        let (fake_certificates, genesis_verifier) =
            setup_certificate_chain(total_certificates, certificates_per_epoch);
        let verifier = MockDependencyInjector::new().build_certificate_verifier();
        let standard_certificate = fake_certificates[0].clone();
        let mut genesis_certificate = fake_certificates.last().unwrap().clone();
        genesis_certificate.signature = standard_certificate.signature.clone();
        genesis_certificate.hash = genesis_certificate.compute_hash();

        let error = verifier
            .verify_genesis_certificate(
                &genesis_certificate,
                &genesis_verifier.to_verification_key(),
            )
            .await
            .expect_err("verify_genesis_certificate should fail");

        assert_error_matches!(
            CertificateVerifierError::InvalidGenesisCertificateProvided,
            error
        )
    }

    #[tokio::test]
    async fn verify_genesis_certificate_fails_if_hash_unmatch() {
        let (total_certificates, certificates_per_epoch) = (5, 1);
        let (fake_certificates, genesis_verifier) =
            setup_certificate_chain(total_certificates, certificates_per_epoch);
        let verifier = MockDependencyInjector::new().build_certificate_verifier();
        let mut genesis_certificate = fake_certificates.last().unwrap().clone();
        genesis_certificate.hash = "another-hash".to_string();

        let error = verifier
            .verify_genesis_certificate(
                &genesis_certificate,
                &genesis_verifier.to_verification_key(),
            )
            .await
            .expect_err("verify_genesis_certificate should fail");

        assert_error_matches!(CertificateVerifierError::CertificateHashUnmatch, error)
    }

    #[tokio::test]
    async fn verify_genesis_certificate_fails_if_protocol_message_unmatch() {
        let (total_certificates, certificates_per_epoch) = (5, 1);
        let (fake_certificates, genesis_verifier) =
            setup_certificate_chain(total_certificates, certificates_per_epoch);
        let verifier = MockDependencyInjector::new().build_certificate_verifier();
        let mut genesis_certificate = fake_certificates.last().unwrap().clone();
        genesis_certificate.protocol_message.set_message_part(
            ProtocolMessagePartKey::CurrentEpoch,
            "another-value".to_string(),
        );
        genesis_certificate.hash = genesis_certificate.compute_hash();

        let error = verifier
            .verify_genesis_certificate(
                &genesis_certificate,
                &genesis_verifier.to_verification_key(),
            )
            .await
            .expect_err("verify_genesis_certificate should fail");

        assert_error_matches!(
            CertificateVerifierError::CertificateProtocolMessageUnmatch,
            error
        )
    }

    #[tokio::test]
    async fn verify_genesis_certificate_fails_if_epoch_unmatch() {
        let (total_certificates, certificates_per_epoch) = (5, 1);
        let (fake_certificates, genesis_verifier) =
            setup_certificate_chain(total_certificates, certificates_per_epoch);
        let verifier = MockDependencyInjector::new().build_certificate_verifier();
        let mut genesis_certificate = fake_certificates.last().unwrap().clone();
        genesis_certificate.epoch -= 1;
        genesis_certificate.hash = genesis_certificate.compute_hash();

        let error = verifier
            .verify_genesis_certificate(
                &genesis_certificate,
                &genesis_verifier.to_verification_key(),
            )
            .await
            .expect_err("verify_genesis_certificate should fail");

        assert_error_matches!(CertificateVerifierError::CertificateEpochUnmatch, error)
    }

    #[tokio::test]
    async fn verify_standard_certificate_success_with_different_epochs_as_previous() {
        let (total_certificates, certificates_per_epoch) = (5, 1);
        let (fake_certificates, _) =
            setup_certificate_chain(total_certificates, certificates_per_epoch);
        let verifier = MockDependencyInjector::new().build_certificate_verifier();
        let certificate = fake_certificates[0].clone();
        let previous_certificate = fake_certificates[1].clone();

        let verify = verifier
            .verify_standard_certificate(&certificate, &previous_certificate)
            .await;

        verify.expect("verify_standard_certificate should not fail");
    }

    #[tokio::test]
    async fn verify_standard_certificate_success_with_same_epoch_as_previous() {
        let (total_certificates, certificates_per_epoch) = (5, 2);
        let (fake_certificates, _) =
            setup_certificate_chain(total_certificates, certificates_per_epoch);
        let verifier = MockDependencyInjector::new().build_certificate_verifier();
        let certificate = fake_certificates[0].clone();
        let previous_certificate = fake_certificates[1].clone();

        let verify = verifier
            .verify_standard_certificate(&certificate, &previous_certificate)
            .await;

        verify.expect("verify_standard_certificate should not fail");
    }

    #[tokio::test]
    async fn verify_standard_certificate_fails_if_is_not_genesis() {
        let (total_certificates, certificates_per_epoch) = (5, 1);
        let (fake_certificates, _) =
            setup_certificate_chain(total_certificates, certificates_per_epoch);
        let verifier = MockDependencyInjector::new().build_certificate_verifier();
        let genesis_certificate = fake_certificates.last().unwrap().clone();
        let mut standard_certificate = fake_certificates[0].clone();
        standard_certificate.signature = genesis_certificate.signature.clone();
        standard_certificate.hash = standard_certificate.compute_hash();
        let standard_previous_certificate = fake_certificates[1].clone();

        let error = verifier
            .verify_standard_certificate(&standard_certificate, &standard_previous_certificate)
            .await
            .expect_err("verify_standard_certificate should fail");

        assert_error_matches!(
            CertificateVerifierError::InvalidStandardCertificateProvided,
            error
        )
    }

    #[tokio::test]
    async fn verify_standard_certificate_fails_if_infinite_loop() {
        let (total_certificates, certificates_per_epoch) = (5, 1);
        let (fake_certificates, _) =
            setup_certificate_chain(total_certificates, certificates_per_epoch);
        let verifier = MockDependencyInjector::new().build_certificate_verifier();
        let mut certificate = fake_certificates[0].clone();
        certificate.previous_hash = certificate.hash.clone();
        let previous_certificate = fake_certificates[1].clone();

        let error = verifier
            .verify_standard_certificate(&certificate, &previous_certificate)
            .await
            .expect_err("verify_standard_certificate should fail");

        assert_error_matches!(
            CertificateVerifierError::CertificateChainInfiniteLoop,
            error
        )
    }

    #[tokio::test]
    async fn verify_standard_certificate_fails_if_hash_unmatch() {
        let (total_certificates, certificates_per_epoch) = (5, 1);
        let (fake_certificates, _) =
            setup_certificate_chain(total_certificates, certificates_per_epoch);
        let verifier = MockDependencyInjector::new().build_certificate_verifier();
        let mut certificate = fake_certificates[0].clone();
        certificate.hash = "another-hash".to_string();
        let previous_certificate = fake_certificates[1].clone();

        let error = verifier
            .verify_standard_certificate(&certificate, &previous_certificate)
            .await
            .expect_err("verify_standard_certificate should fail");

        assert_error_matches!(CertificateVerifierError::CertificateHashUnmatch, error)
    }

    #[tokio::test]
    async fn verify_standard_certificate_fails_if_protocol_message_unmatch() {
        let (total_certificates, certificates_per_epoch) = (5, 1);
        let (fake_certificates, _) =
            setup_certificate_chain(total_certificates, certificates_per_epoch);
        let verifier = MockDependencyInjector::new().build_certificate_verifier();
        let mut certificate = fake_certificates[0].clone();
        certificate.protocol_message.set_message_part(
            ProtocolMessagePartKey::CurrentEpoch,
            "another-value".to_string(),
        );
        certificate.hash = certificate.compute_hash();
        let previous_certificate = fake_certificates[1].clone();

        let error = verifier
            .verify_standard_certificate(&certificate, &previous_certificate)
            .await
            .expect_err("verify_standard_certificate should fail");

        assert_error_matches!(
            CertificateVerifierError::CertificateProtocolMessageUnmatch,
            error
        )
    }

    #[tokio::test]
    async fn verify_standard_certificate_fails_if_epoch_unmatch() {
        let (total_certificates, certificates_per_epoch) = (5, 1);
        let (fake_certificates, _) =
            setup_certificate_chain(total_certificates, certificates_per_epoch);
        let verifier = MockDependencyInjector::new().build_certificate_verifier();
        let mut certificate = fake_certificates[0].clone();
        certificate.epoch -= 1;
        certificate.hash = certificate.compute_hash();
        let previous_certificate = fake_certificates[1].clone();

        let error = verifier
            .verify_standard_certificate(&certificate, &previous_certificate)
            .await
            .expect_err("verify_standard_certificate should fail");

        assert_error_matches!(CertificateVerifierError::CertificateEpochUnmatch, error)
    }

    #[tokio::test]
    async fn verify_standard_certificate_fails_if_has_missing_epoch() {
        fn create_epoch_gap_certificate(
            certificate: Certificate,
            context: &CertificateChainBuilderContext,
        ) -> Certificate {
            let fixture = context.fixture;
            let modified_epoch = certificate.epoch + 1;
            let mut protocol_message = certificate.protocol_message.to_owned();
            protocol_message.set_message_part(
                ProtocolMessagePartKey::CurrentEpoch,
                modified_epoch.to_string(),
            );
            let signed_message = protocol_message.compute_hash();
            let mut modified_certificate = certificate;
            modified_certificate.epoch = modified_epoch;
            modified_certificate.protocol_message = protocol_message;
            modified_certificate.signed_message = signed_message.clone();
            let single_signatures = fixture
                .signers_fixture()
                .iter()
                .filter_map(|s| s.protocol_signer.sign(signed_message.as_bytes()))
                .collect::<Vec<_>>();
            let clerk = ProtocolClerk::from_signer(&fixture.signers_fixture()[0].protocol_signer);
            let modified_multi_signature = clerk
                .aggregate(&single_signatures, signed_message.as_bytes())
                .unwrap();
            modified_certificate.signature = CertificateSignature::MultiSignature(
                modified_certificate.signed_entity_type(),
                modified_multi_signature.into(),
            );

            modified_certificate
        }

        let (total_certificates, certificates_per_epoch) = (5, 1);
        let (fake_certificates, _) = CertificateChainBuilder::new()
            .with_total_certificates(total_certificates)
            .with_certificates_per_epoch(certificates_per_epoch)
            .with_standard_certificate_processor(&|certificate, context| {
                if context.is_last_certificate() {
                    create_epoch_gap_certificate(certificate, context)
                } else {
                    certificate
                }
            })
            .build();
        let verifier = MockDependencyInjector::new().build_certificate_verifier();
        let certificate = fake_certificates[0].clone();
        let previous_certificate = fake_certificates[1].clone();

        let error = verifier
            .verify_standard_certificate(&certificate, &previous_certificate)
            .await
            .expect_err("verify_standard_certificate should fail");

        assert_error_matches!(
            CertificateVerifierError::CertificateChainMissingEpoch,
            error
        )
    }

    #[tokio::test]
    async fn verify_standard_certificate_fails_if_certificate_previous_hash_unmatch() {
        let (total_certificates, certificates_per_epoch) = (5, 1);
        let (fake_certificates, _) =
            setup_certificate_chain(total_certificates, certificates_per_epoch);
        let verifier = MockDependencyInjector::new().build_certificate_verifier();
        let certificate = fake_certificates[0].clone();
        let mut previous_certificate = fake_certificates[1].clone();
        previous_certificate.previous_hash = "another-hash".to_string();
        previous_certificate.hash = previous_certificate.compute_hash();

        let error = verifier
            .verify_standard_certificate(&certificate, &previous_certificate)
            .await
            .expect_err("verify_standard_certificate should fail");

        assert_error_matches!(
            CertificateVerifierError::CertificateChainPreviousHashUnmatch,
            error
        )
    }

    #[tokio::test]
    async fn verify_standard_certificate_fails_if_certificate_chain_avk_unmatch() {
        let (total_certificates, certificates_per_epoch) = (5, 1);
        let (fake_certificates, _) =
            setup_certificate_chain(total_certificates, certificates_per_epoch);
        let verifier = MockDependencyInjector::new().build_certificate_verifier();
        let mut certificate = fake_certificates[0].clone();
        let mut previous_certificate = fake_certificates[1].clone();
        previous_certificate.protocol_message.set_message_part(
            ProtocolMessagePartKey::NextAggregateVerificationKey,
            "another-avk".to_string(),
        );
        previous_certificate.hash = previous_certificate.compute_hash();
        certificate
            .previous_hash
            .clone_from(&previous_certificate.hash);
        certificate.hash = certificate.compute_hash();

        let error = verifier
            .verify_standard_certificate(&certificate, &previous_certificate)
            .await
            .expect_err("verify_standard_certificate should fail");

        assert_error_matches!(CertificateVerifierError::CertificateChainAVKUnmatch, error)
    }

    #[tokio::test]
    async fn verify_standard_certificate_fails_if_certificate_chain_protocol_parameters_unmatch() {
        let (total_certificates, certificates_per_epoch) = (5, 1);
        let (fake_certificates, _) =
            setup_certificate_chain(total_certificates, certificates_per_epoch);
        let verifier = MockDependencyInjector::new().build_certificate_verifier();
        let mut certificate = fake_certificates[0].clone();
        let mut previous_certificate = fake_certificates[1].clone();
        previous_certificate.protocol_message.set_message_part(
            ProtocolMessagePartKey::NextProtocolParameters,
            "protocol-params-hash-123".to_string(),
        );
        previous_certificate.hash = previous_certificate.compute_hash();
        certificate
            .previous_hash
            .clone_from(&previous_certificate.hash);
        certificate.hash = certificate.compute_hash();

        let error = verifier
            .verify_standard_certificate(&certificate, &previous_certificate)
            .await
            .expect_err("verify_standard_certificate should fail");

        assert_error_matches!(
            CertificateVerifierError::CertificateChainProtocolParametersUnmatch,
            error
        )
    }

    #[tokio::test]
    async fn verify_certificate_success_when_certificate_is_genesis_and_valid() {
        let (total_certificates, certificates_per_epoch) = (5, 1);
        let (fake_certificates, genesis_verifier) =
            setup_certificate_chain(total_certificates, certificates_per_epoch);
        let genesis_certificate = fake_certificates.last().unwrap().clone();
        let mock_container = MockDependencyInjector::new();
        let verifier = mock_container.build_certificate_verifier();

        let verify = verifier
            .verify_certificate(
                &genesis_certificate,
                &genesis_verifier.to_verification_key(),
            )
            .await;

        verify.expect("verify_certificate should not fail");
    }

    #[tokio::test]
    async fn verify_certificate_success_when_certificate_is_standard_and_valid() {
        let (total_certificates, certificates_per_epoch) = (5, 1);
        let (fake_certificates, genesis_verifier) =
            setup_certificate_chain(total_certificates, certificates_per_epoch);
        let certificate = fake_certificates[0].clone();
        let previous_certificate = fake_certificates[1].clone();
        let mut mock_container = MockDependencyInjector::new();
        mock_container
            .mock_certificate_retriever
            .expect_get_certificate_details()
            .returning(move |_| Ok(previous_certificate.clone()))
            .times(1);
        let verifier = mock_container.build_certificate_verifier();

        let verify = verifier
            .verify_certificate(&certificate, &genesis_verifier.to_verification_key())
            .await;

        verify.expect("verify_certificate should not fail");
    }

    #[tokio::test]
    async fn verify_certificate_chain_verifies_all_chained_certificates() {
        struct CertificateVerifierTest {
            certificates_unverified: Mutex<HashMap<String, Certificate>>,
        }

        impl CertificateVerifierTest {
            fn from_certificates(certificates: &[Certificate]) -> Self {
                Self {
                    certificates_unverified: Mutex::new(HashMap::from_iter(
                        certificates
                            .iter()
                            .map(|c| (c.hash.to_owned(), c.to_owned())),
                    )),
                }
            }

            async fn has_unverified_certificates(&self) -> bool {
                !self.certificates_unverified.lock().await.is_empty()
            }
        }

        #[async_trait]
        impl CertificateVerifier for CertificateVerifierTest {
            async fn verify_genesis_certificate(
                &self,
                _genesis_certificate: &Certificate,
                _genesis_verification_key: &ProtocolGenesisVerificationKey,
            ) -> StdResult<()> {
                unimplemented!()
            }

            async fn verify_standard_certificate(
                &self,
                _certificate: &Certificate,
                _previous_certificate: &Certificate,
            ) -> StdResult<()> {
                unimplemented!()
            }

            async fn verify_certificate(
                &self,
                certificate: &Certificate,
                _genesis_verification_key: &ProtocolGenesisVerificationKey,
            ) -> StdResult<Option<Certificate>> {
                let mut certificates_unverified = self.certificates_unverified.lock().await;
                let _verified_certificate = (*certificates_unverified).remove(&certificate.hash);
                let previous_certificate = (*certificates_unverified)
                    .get(&certificate.previous_hash)
                    .cloned();

                Ok(previous_certificate)
            }
        }

        let (total_certificates, certificates_per_epoch) = (10, 1);
        let (fake_certificates, genesis_verifier) =
            setup_certificate_chain(total_certificates, certificates_per_epoch);
        let fake_certificate_to_verify = fake_certificates[0].clone();
        let verifier = CertificateVerifierTest::from_certificates(&fake_certificates);
        assert!(verifier.has_unverified_certificates().await);

        let verify = verifier
            .verify_certificate_chain(
                fake_certificate_to_verify,
                &genesis_verifier.to_verification_key(),
            )
            .await;

        verify.expect("verify_certificate_chain should not fail");
        assert!(!verifier.has_unverified_certificates().await);
    }

    #[tokio::test]
    async fn verify_certificate_chain_success_when_chain_is_valid() {
        let (total_certificates, certificates_per_epoch) = (7, 2);
        let (fake_certificates, genesis_verifier) =
            setup_certificate_chain(total_certificates, certificates_per_epoch);
        let certificate_retriever =
            FakeCertificaterRetriever::from_certificates(&fake_certificates);
        let verifier =
            MithrilCertificateVerifier::new(TestLogger::stdout(), Arc::new(certificate_retriever));
        let certificate_to_verify = fake_certificates[0].clone();

        let verify = verifier
            .verify_certificate_chain(
                certificate_to_verify,
                &genesis_verifier.to_verification_key(),
            )
            .await;
        verify.expect("verify_certificate_chain should not fail");
    }

    #[tokio::test]
    async fn verify_certificate_chain_fails_when_chain_is_tampered() {
        let (total_certificates, certificates_per_epoch) = (7, 2);
        let (mut fake_certificates, genesis_verifier) =
            setup_certificate_chain(total_certificates, certificates_per_epoch);
        let index_certificate_fail = (total_certificates / 2) as usize;
        fake_certificates[index_certificate_fail].signed_message = "tampered-message".to_string();
        let certificate_retriever =
            FakeCertificaterRetriever::from_certificates(&fake_certificates);
        let verifier =
            MithrilCertificateVerifier::new(TestLogger::stdout(), Arc::new(certificate_retriever));
        let certificate_to_verify = fake_certificates[0].clone();

        let error = verifier
            .verify_certificate_chain(
                certificate_to_verify,
                &genesis_verifier.to_verification_key(),
            )
            .await
            .expect_err("verify_certificate_chain should fail");

        assert_error_matches!(CertificateVerifierError::CertificateHashUnmatch, error)
    }

    #[tokio::test]
    async fn verify_certificate_chain_fails_when_adversarial_with_registered_signer_forgery_through_protocol_parameters(
    ) {
        // Create an adversarial certificate with a forged multi signature:
        // - with the valid signed message
        // - with the valid aggregate verification key (valid stake distribution)
        // - by a legit adversarial registered signer in the signing stake distribution
        // - with adversarial protocol parameters (phi_f = 1.0, i.e. all lotteries are won by the adversarial signer and quorum is reached)
        fn forge_certificate(
            certificate: Certificate,
            context: &CertificateChainBuilderContext,
        ) -> Certificate {
            assert_ne!(
                1.0, certificate.metadata.protocol_parameters.phi_f,
                "Adversarial protocol parameters phi_f should be different from 1.0"
            );
            let fixture = context.fixture;
            let signed_message = certificate.signed_message.to_owned();
            let mut forged_certificate = certificate;
            let mut forged_protocol_parameters = fixture.protocol_parameters();
            forged_protocol_parameters.phi_f = 1.0;
            let forged_single_signatures = fixture
                .signers_fixture()
                .iter()
                .take(1)
                .filter_map(|s| {
                    let s_adversary = s
                        .to_owned()
                        .try_new_with_protocol_parameters(forged_protocol_parameters.clone())
                        .unwrap();
                    let signature = s_adversary.protocol_signer.sign(signed_message.as_bytes());

                    signature
                })
                .collect::<Vec<_>>();
            let forged_clerk = ProtocolClerk::from_registration(
                &forged_protocol_parameters.clone().into(),
                &fixture.signers_fixture()[0].protocol_closed_key_registration,
            );
            let forged_multi_signature = forged_clerk
                .aggregate(&forged_single_signatures, signed_message.as_bytes())
                .unwrap();
            forged_certificate.signature = CertificateSignature::MultiSignature(
                forged_certificate.signed_entity_type(),
                forged_multi_signature.into(),
            );
            forged_certificate.metadata.protocol_parameters = forged_protocol_parameters;

            forged_certificate
        }

        let (total_certificates, certificates_per_epoch) = (7, 2);
        let (fake_certificates, genesis_verifier) = CertificateChainBuilder::new()
            .with_total_certificates(total_certificates)
            .with_certificates_per_epoch(certificates_per_epoch)
            .with_standard_certificate_processor(&|certificate, context| {
                if context.is_last_certificate() {
                    forge_certificate(certificate, context)
                } else {
                    certificate
                }
            })
            .build();
        let certificate_to_verify = fake_certificates[0].clone();
        let mock_container = MockDependencyInjector::new();
        let mut verifier = mock_container.build_certificate_verifier();
        verifier.certificate_retriever = Arc::new(FakeCertificaterRetriever::from_certificates(
            &fake_certificates,
        ));

        let error = verifier
            .verify_certificate(
                &certificate_to_verify,
                &genesis_verifier.to_verification_key(),
            )
            .await
            .expect_err("verify_certificate_chain should fail");

        assert_error_matches!(
            CertificateVerifierError::CertificateChainProtocolParametersUnmatch,
            error
        )
    }
}
