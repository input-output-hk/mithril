//! A module used to validate the Certificate Chain created by an aggregator
//!
use async_trait::async_trait;
use hex::ToHex;
use slog::{debug, Logger};
use std::sync::Arc;
use thiserror::Error;

use super::{CertificateRetriever, CertificateRetrieverError};
use crate::crypto_helper::{
    ProtocolAggregateVerificationKey, ProtocolGenesisError, ProtocolGenesisVerifier,
    ProtocolMultiSignature,
};
use crate::entities::{
    Certificate, CertificateSignature, ProtocolMessage, ProtocolMessagePartKey, ProtocolParameters,
};
use crate::StdError;

#[cfg(test)]
use mockall::automock;

/// [CertificateVerifier] related errors.
#[derive(Error, Debug)]
pub enum CertificateVerifierError {
    /// Error raised when the multi signatures verification fails.
    #[error("multi signature verification failed: '{0}'")]
    VerifyMultiSignature(String),

    /// Error raised when encoding or decoding of data to hex fails.
    #[error("codec hex error: '{0:?}'")]
    Codec(StdError),

    /// Error raised when encoding or decoding of data to genesis type.
    #[error("codec genesis error: '{0}'")]
    CodecGenesis(String),

    /// Error raised when a CertificateRetriever tries to retrieve a [Certificate].
    #[error("certificate retriever error: '{0}'")]
    CertificateRetriever(#[from] CertificateRetrieverError),

    /// Error raised when the Genesis Signature stored in a [Certificate] is invalid.
    #[error("certificate genesis error: '{0}'")]
    CertificateGenesis(#[from] ProtocolGenesisError),

    /// Error raised when the hash stored in a [Certificate] doesn't match a recomputed hash.
    #[error("certificate hash unmatch error")]
    CertificateHashUnmatch,

    /// Error raised when validating the certificate chain if a previous [Certificate] hash isn't
    /// equal to the current certificate `previous_hash`.
    #[error("certificate chain previous hash unmatch error")]
    CertificateChainPreviousHashUnmatch,

    /// Error raised when validating the certificate chain if the current [Certificate]
    /// `aggregate_verification_key` doesn't match the previous `aggregate_verification_key` (if
    /// the certificates are on the same epoch) or the previous `next_aggregate_verification_key`
    /// (if the certificates are on different epoch).
    #[error("certificate chain AVK unmatch error")]
    CertificateChainAVKUnmatch,

    /// Error raised when validating the certificate chain if the chain loops.
    #[error("certificate chain infinite loop error")]
    CertificateChainInfiniteLoop,

    /// Error raised when [CertificateVerifier::verify_genesis_certificate] was called with a
    /// certificate that's not a genesis certificate.
    #[error("can't validate genesis certificate: given certificate isn't a genesis certificate")]
    InvalidGenesisCertificateProvided,
}

/// CertificateVerifier is the cryptographic engine in charge of verifying multi signatures and
/// [certificates](Certificate)
#[cfg_attr(test, automock)]
#[async_trait]
pub trait CertificateVerifier: Send + Sync {
    /// Verify Genesis certificate
    async fn verify_genesis_certificate(
        &self,
        genesis_certificate: &Certificate,
        genesis_verifier: &ProtocolGenesisVerifier,
    ) -> Result<(), CertificateVerifierError>;

    /// Verify if a Certificate is valid and returns the previous Certificate in the chain if exists
    /// Step 1: Check if the hash is valid (i.e. the Certificate has not been tampered by modifying its content)
    /// Step 2: Check that the multi signature is valid if it is a Standard Certificate (i.e verification of the Mithril multi signature)
    /// Step 3: Check that the aggregate verification key of the Certificate is registered in the previous Certificate in the chain
    async fn verify_certificate(
        &self,
        certificate: &Certificate,
        certificate_retriever: Arc<dyn CertificateRetriever>,
        genesis_verifier: &ProtocolGenesisVerifier,
    ) -> Result<Option<Certificate>, CertificateVerifierError>;

    /// Verify that the Certificate Chain associated to a Certificate is valid
    /// TODO: see if we can borrow the certificate instead.
    async fn verify_certificate_chain(
        &self,
        certificate: Certificate,
        certificate_retriever: Arc<dyn CertificateRetriever>,
        genesis_verifier: &ProtocolGenesisVerifier,
    ) -> Result<(), CertificateVerifierError> {
        let mut certificate = certificate;
        while let Some(previous_certificate) = self
            .verify_certificate(
                &certificate,
                certificate_retriever.clone(),
                genesis_verifier,
            )
            .await?
        {
            certificate = previous_certificate;
        }

        Ok(())
    }

    /// still a dirty hack to mock the protocol message
    /// verify that the protocol message is equal to the signed message of the certificate.
    /// TODO: Remove this method.
    fn verify_protocol_message(
        &self,
        protocol_message: &ProtocolMessage,
        certificate: &Certificate,
    ) -> bool {
        protocol_message.compute_hash() == certificate.signed_message
    }
}

/// MithrilCertificateVerifier is an implementation of the CertificateVerifier
pub struct MithrilCertificateVerifier {
    /// The logger where the logs should be written
    logger: Logger,
}

impl MithrilCertificateVerifier {
    /// MithrilCertificateVerifier factory
    pub fn new(logger: Logger) -> Self {
        debug!(logger, "New MithrilCertificateVerifier created");
        Self { logger }
    }

    /// Verify a multi signature
    fn verify_multi_signature(
        &self,
        message: &[u8],
        multi_signature: &ProtocolMultiSignature,
        aggregate_verification_key: &str,
        protocol_parameters: &ProtocolParameters,
    ) -> Result<(), CertificateVerifierError> {
        debug!(
            self.logger,
            "Verify multi signature for {:?}",
            message.encode_hex::<String>()
        );
        let aggregate_verification_key =
            ProtocolAggregateVerificationKey::from_json_hex(aggregate_verification_key)
                .map_err(CertificateVerifierError::Codec)?;

        multi_signature
            .verify(
                message,
                &aggregate_verification_key,
                &protocol_parameters.to_owned().into(),
            )
            .map_err(|e| CertificateVerifierError::VerifyMultiSignature(e.to_string()))
    }

    /// Verify Standard certificate
    async fn verify_standard_certificate(
        &self,
        certificate: &Certificate,
        signature: &ProtocolMultiSignature,
        certificate_retriever: Arc<dyn CertificateRetriever>,
    ) -> Result<Option<Certificate>, CertificateVerifierError> {
        self.verify_multi_signature(
            certificate.signed_message.as_bytes(),
            signature,
            &certificate.aggregate_verification_key,
            &certificate.metadata.protocol_parameters,
        )?;
        let previous_certificate = certificate_retriever
            .get_certificate_details(&certificate.previous_hash)
            .await?;
        let valid_certificate_has_different_epoch_as_previous =
            |next_aggregate_verification_key: &String| -> bool {
                next_aggregate_verification_key == &certificate.aggregate_verification_key
                    && previous_certificate.beacon.epoch != certificate.beacon.epoch
            };
        let valid_certificate_has_same_epoch_as_previous = || -> bool {
            previous_certificate.aggregate_verification_key
                == certificate.aggregate_verification_key
                && previous_certificate.beacon.epoch == certificate.beacon.epoch
        };
        if previous_certificate.hash != certificate.previous_hash {
            return Err(CertificateVerifierError::CertificateChainPreviousHashUnmatch);
        }

        match &previous_certificate
            .protocol_message
            .get_message_part(&ProtocolMessagePartKey::NextAggregateVerificationKey)
        {
            Some(next_aggregate_verification_key)
                if valid_certificate_has_different_epoch_as_previous(
                    next_aggregate_verification_key,
                ) =>
            {
                Ok(Some(previous_certificate.to_owned()))
            }
            Some(_) if valid_certificate_has_same_epoch_as_previous() => {
                Ok(Some(previous_certificate.to_owned()))
            }
            None => Ok(None),
            _ => {
                debug!(
                    self.logger,
                    "Previous certificate {:#?}", previous_certificate
                );
                Err(CertificateVerifierError::CertificateChainAVKUnmatch)
            }
        }
    }
}

#[async_trait]
impl CertificateVerifier for MithrilCertificateVerifier {
    /// Verify Genesis certificate
    async fn verify_genesis_certificate(
        &self,
        genesis_certificate: &Certificate,
        genesis_verifier: &ProtocolGenesisVerifier,
    ) -> Result<(), CertificateVerifierError> {
        let genesis_signature = match &genesis_certificate.signature {
            CertificateSignature::GenesisSignature(signature) => Ok(signature),
            _ => Err(CertificateVerifierError::InvalidGenesisCertificateProvided),
        }?;

        genesis_verifier.verify(
            genesis_certificate.signed_message.as_bytes(),
            genesis_signature,
        )?;

        Ok(())
    }

    /// Verify a certificate
    async fn verify_certificate(
        &self,
        certificate: &Certificate,
        certificate_retriever: Arc<dyn CertificateRetriever>,
        genesis_verifier: &ProtocolGenesisVerifier,
    ) -> Result<Option<Certificate>, CertificateVerifierError> {
        debug!(
            self.logger,
            "Verifying certificate";
            "certificate_hash" => &certificate.hash,
            "certificate_previous_hash" => &certificate.previous_hash,
            "certificate_beacon" => ?certificate.beacon
        );

        certificate
            .hash
            .eq(&certificate.compute_hash())
            .then(|| certificate.hash.clone())
            .ok_or(CertificateVerifierError::CertificateHashUnmatch)?;

        if certificate.is_chaining_to_itself() {
            Err(CertificateVerifierError::CertificateChainInfiniteLoop)
        } else {
            match &certificate.signature {
                CertificateSignature::GenesisSignature(_signature) => {
                    self.verify_genesis_certificate(certificate, genesis_verifier)
                        .await?;
                    Ok(None)
                }
                CertificateSignature::MultiSignature(signature) => {
                    self.verify_standard_certificate(certificate, signature, certificate_retriever)
                        .await
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use async_trait::async_trait;
    use mockall::mock;
    use slog_scope;

    use super::CertificateRetriever;
    use super::*;

    use crate::crypto_helper::{tests_setup::*, ProtocolClerk};
    use crate::test_utils::MithrilFixtureBuilder;

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

    #[test]
    fn test_verify_multi_signature_ok() {
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

        let verifier = MithrilCertificateVerifier::new(slog_scope::logger());
        let message_tampered = message_hash[1..].to_vec();
        assert!(
            verifier
                .verify_multi_signature(
                    &message_tampered,
                    &multi_signature,
                    &aggregate_verification_key.to_json_hex().unwrap(),
                    &fixture.protocol_parameters(),
                )
                .is_err(),
            "multi signature verification should have failed"
        );
        verifier
            .verify_multi_signature(
                &message_hash,
                &multi_signature,
                &aggregate_verification_key.to_json_hex().unwrap(),
                &fixture.protocol_parameters(),
            )
            .expect("multi signature verification should have succeeded");
    }

    #[tokio::test]
    async fn test_verify_certificate_ok_different_epochs() {
        let total_certificates = 5;
        let certificates_per_epoch = 1;
        let (fake_certificates, genesis_verifier) =
            setup_certificate_chain(total_certificates, certificates_per_epoch);
        let fake_certificate1 = fake_certificates[0].clone();
        let fake_certificate2 = fake_certificates[1].clone();
        let mut mock_certificate_retriever = MockCertificateRetrieverImpl::new();
        mock_certificate_retriever
            .expect_get_certificate_details()
            .returning(move |_| Ok(fake_certificate2.clone()))
            .times(1);
        let verifier = MithrilCertificateVerifier::new(slog_scope::logger());
        let verify = verifier
            .verify_certificate(
                &fake_certificate1,
                Arc::new(mock_certificate_retriever),
                &genesis_verifier,
            )
            .await;
        verify.expect("unexpected error");
    }

    #[tokio::test]
    async fn test_verify_certificate_ok_same_epoch() {
        let total_certificates = 5;
        let certificates_per_epoch = 2;
        let (fake_certificates, genesis_verifier) =
            setup_certificate_chain(total_certificates, certificates_per_epoch);
        let fake_certificate1 = fake_certificates[0].clone();
        let fake_certificate2 = fake_certificates[1].clone();
        let mut mock_certificate_retriever = MockCertificateRetrieverImpl::new();
        mock_certificate_retriever
            .expect_get_certificate_details()
            .returning(move |_| Ok(fake_certificate2.clone()))
            .times(1);
        let verifier = MithrilCertificateVerifier::new(slog_scope::logger());
        let verify = verifier
            .verify_certificate(
                &fake_certificate1,
                Arc::new(mock_certificate_retriever),
                &genesis_verifier,
            )
            .await;
        verify.expect("unexpected error");
    }

    #[tokio::test]
    async fn test_verify_certificate_ko_certificate_chain_previous_hash_unmatch() {
        let total_certificates = 5;
        let certificates_per_epoch = 1;
        let (fake_certificates, genesis_verifier) =
            setup_certificate_chain(total_certificates, certificates_per_epoch);
        let fake_certificate1 = fake_certificates[0].clone();
        let mut fake_certificate2 = fake_certificates[1].clone();
        fake_certificate2.previous_hash = "another-hash".to_string();
        fake_certificate2.hash = fake_certificate2.compute_hash();
        let mut mock_certificate_retriever = MockCertificateRetrieverImpl::new();
        mock_certificate_retriever
            .expect_get_certificate_details()
            .returning(move |_| Ok(fake_certificate2.clone()))
            .times(1);
        let verifier = MithrilCertificateVerifier::new(slog_scope::logger());
        let verify = verifier
            .verify_certificate(
                &fake_certificate1,
                Arc::new(mock_certificate_retriever),
                &genesis_verifier,
            )
            .await;
        assert!(
            matches!(
                verify,
                Err(CertificateVerifierError::CertificateChainPreviousHashUnmatch)
            ),
            "unexpected error type: {verify:?}"
        );
    }

    #[tokio::test]
    async fn test_verify_certificate_ko_certificate_chain_avk_unmatch() {
        let total_certificates = 5;
        let certificates_per_epoch = 1;
        let (fake_certificates, genesis_verifier) =
            setup_certificate_chain(total_certificates, certificates_per_epoch);
        let mut fake_certificate1 = fake_certificates[0].clone();
        let mut fake_certificate2 = fake_certificates[1].clone();
        fake_certificate2.protocol_message.set_message_part(
            ProtocolMessagePartKey::NextAggregateVerificationKey,
            "another-avk".to_string(),
        );
        fake_certificate2.hash = fake_certificate2.compute_hash();
        fake_certificate1.previous_hash = fake_certificate2.hash.clone();
        fake_certificate1.hash = fake_certificate1.compute_hash();
        let mut mock_certificate_retriever = MockCertificateRetrieverImpl::new();
        mock_certificate_retriever
            .expect_get_certificate_details()
            .returning(move |_| Ok(fake_certificate2.clone()))
            .times(1);
        let verifier = MithrilCertificateVerifier::new(slog_scope::logger());
        let verify = verifier
            .verify_certificate(
                &fake_certificate1,
                Arc::new(mock_certificate_retriever),
                &genesis_verifier,
            )
            .await;
        assert!(
            matches!(
                verify,
                Err(CertificateVerifierError::CertificateChainAVKUnmatch)
            ),
            "unexpected error type: {verify:?}"
        );
    }

    #[tokio::test]
    async fn test_verify_certificate_ko_certificate_hash_not_matching() {
        let total_certificates = 5;
        let certificates_per_epoch = 1;
        let (fake_certificates, genesis_verifier) =
            setup_certificate_chain(total_certificates, certificates_per_epoch);
        let mut fake_certificate1 = fake_certificates[0].clone();
        fake_certificate1.hash = "another-hash".to_string();
        let mock_certificate_retriever = MockCertificateRetrieverImpl::new();
        let verifier = MithrilCertificateVerifier::new(slog_scope::logger());
        let verify = verifier
            .verify_certificate(
                &fake_certificate1,
                Arc::new(mock_certificate_retriever),
                &genesis_verifier,
            )
            .await;
        assert!(
            matches!(
                verify,
                Err(CertificateVerifierError::CertificateHashUnmatch)
            ),
            "unexpected error type: {verify:?}"
        );
    }

    #[tokio::test]
    async fn test_verify_certificate_chain_ok() {
        let total_certificates = 15;
        let certificates_per_epoch = 2;
        let (fake_certificates, genesis_verifier) =
            setup_certificate_chain(total_certificates, certificates_per_epoch);
        let mut mock_certificate_retriever = MockCertificateRetrieverImpl::new();
        let certificate_to_verify = fake_certificates[0].clone();
        for fake_certificate in fake_certificates.into_iter().skip(1) {
            mock_certificate_retriever
                .expect_get_certificate_details()
                .returning(move |_| Ok(fake_certificate.clone()))
                .times(1);
        }
        let verifier = MithrilCertificateVerifier::new(slog_scope::logger());
        let verify = verifier
            .verify_certificate_chain(
                certificate_to_verify,
                Arc::new(mock_certificate_retriever),
                &genesis_verifier,
            )
            .await;
        verify.expect("unexpected error");
    }

    #[tokio::test]
    async fn test_verify_certificate_chain_ko() {
        let total_certificates = 15;
        let certificates_per_epoch = 2;
        let (mut fake_certificates, genesis_verifier) =
            setup_certificate_chain(total_certificates, certificates_per_epoch);
        let index_certificate_fail = (total_certificates / 2) as usize;
        fake_certificates[index_certificate_fail].hash = "tampered-hash".to_string();
        let mut mock_certificate_retriever = MockCertificateRetrieverImpl::new();
        let certificate_to_verify = fake_certificates[0].clone();
        for fake_certificate in fake_certificates
            .into_iter()
            .skip(1)
            .take(index_certificate_fail)
        {
            mock_certificate_retriever
                .expect_get_certificate_details()
                .returning(move |_| Ok(fake_certificate.clone()))
                .times(1);
        }
        let verifier = MithrilCertificateVerifier::new(slog_scope::logger());
        let verify = verifier
            .verify_certificate_chain(
                certificate_to_verify,
                Arc::new(mock_certificate_retriever),
                &genesis_verifier,
            )
            .await;
        assert!(
            matches!(
                verify,
                Err(CertificateVerifierError::CertificateChainPreviousHashUnmatch)
            ),
            "unexpected error type: {verify:?}"
        );
    }
}
