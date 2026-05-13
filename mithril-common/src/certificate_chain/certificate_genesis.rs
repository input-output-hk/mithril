//! A module used to create a Genesis Certificate
//!
use std::sync::Arc;

use chrono::prelude::*;
#[cfg(feature = "future_snark")]
use slog::warn;
use slog::{Logger, o};
use thiserror::Error;

#[cfg(feature = "future_snark")]
use crate::crypto_helper::ProtocolAggregateVerificationKeyForSnark;
#[cfg(feature = "future_snark")]
use crate::crypto_helper::{GenesisSchnorrSignature, GenesisSchnorrSigner, sha256_digest};
use crate::{
    StdResult,
    crypto_helper::{
        GenesisEd25519Signature, GenesisEd25519Signer, PROTOCOL_VERSION,
        ProtocolAggregateVerificationKey, ProtocolKey,
    },
    entities::{
        Certificate, CertificateMetadata, CertificateSignature, Epoch, ProtocolMessage,
        ProtocolMessagePartKey, ProtocolParameters, SupportedEra,
    },
    protocol::ToMessage,
};

/// The exact byte size of the rigid protocol-message preimage signed under Lagrange.
///
/// Mirrors the in-circuit `PREIMAGE_SIZE` constant pinned by the IVC witness layout
/// in `mithril-stm/src/circuits/halo2_ivc/mod.rs`. The producer enforces this invariant
/// before signing so layout drift surfaces at signing time, not deep inside the SNARK gadget.
#[cfg(feature = "future_snark")]
pub const PREIMAGE_SIZE: usize = 190;

/// [CertificateGenesisProducer] related errors.
#[derive(Error, Debug)]
pub enum CertificateGenesisProducerError {
    /// Error raised when there is no genesis signer available
    #[error("missing genesis signer error")]
    MissingGenesisSigner(),

    /// Error raised when a Lagrange genesis certificate is requested without a SNARK signer.
    #[cfg(feature = "future_snark")]
    #[error(
        "missing SNARK genesis signer error: Lagrange-era genesis certificates require both signers"
    )]
    MissingSnarkGenesisSigner,

    /// Error raised when the rigid protocol-message preimage does not match the pinned
    /// [PREIMAGE_SIZE] expected by the IVC gadget.
    #[cfg(feature = "future_snark")]
    #[error(
        "rigid protocol-message preimage must be exactly {expected} bytes (got {actual}); \
        check the rigid-segment values built into ProtocolMessage"
    )]
    InvalidPreimageSize {
        /// Expected pinned size ([PREIMAGE_SIZE]).
        expected: usize,
        /// Actual size produced by [ProtocolMessage::rigid_preimage].
        actual: usize,
    },

    /// Error surfaced when the SNARK signer fails to produce a signature.
    #[cfg(feature = "future_snark")]
    #[error("SNARK genesis signing failed: {0}")]
    SnarkSigningFailure(String),
}

/// CertificateGenesisProducer is in charge of producing a Genesis Certificate
#[derive(Debug)]
pub struct CertificateGenesisProducer {
    genesis_signer: Option<Arc<GenesisEd25519Signer>>,
    #[cfg(feature = "future_snark")]
    snark_genesis_signer: Option<Arc<GenesisSchnorrSigner>>,
    logger: Logger,
}

impl CertificateGenesisProducer {
    /// CertificateGenesisProducer factory
    pub fn new(genesis_signer: Option<Arc<GenesisEd25519Signer>>) -> Self {
        Self {
            genesis_signer,
            #[cfg(feature = "future_snark")]
            snark_genesis_signer: None,
            logger: Logger::root(slog::Discard, o!()),
        }
    }

    /// Attach a SNARK genesis signer. Required when producing Lagrange-era genesis certificates.
    #[cfg(feature = "future_snark")]
    pub fn with_snark_genesis_signer(
        mut self,
        snark_genesis_signer: Arc<GenesisSchnorrSigner>,
    ) -> Self {
        self.snark_genesis_signer = Some(snark_genesis_signer);
        self
    }

    /// Set the [Logger] to use.
    pub fn with_logger(mut self, logger: Logger) -> Self {
        self.logger = logger;
        self
    }

    /// Create the Genesis protocol message
    pub fn create_genesis_protocol_message(
        &self,
        genesis_protocol_parameters: &ProtocolParameters,
        genesis_avk: &ProtocolAggregateVerificationKey,
        genesis_epoch: &Epoch,
        mithril_era: SupportedEra,
    ) -> StdResult<ProtocolMessage> {
        let genesis_aggregate_verification_key_for_concatenation =
            ProtocolKey::new(genesis_avk.to_concatenation_aggregate_verification_key().to_owned());
        let genesis_concatenation_avk =
            genesis_aggregate_verification_key_for_concatenation.to_json_hex()?;
        let mut protocol_message = match mithril_era {
            SupportedEra::Pythagoras => ProtocolMessage::new(),
            #[cfg(feature = "future_snark")]
            SupportedEra::Lagrange => ProtocolMessage::new_rigid(),
            #[cfg(not(feature = "future_snark"))]
            SupportedEra::Lagrange => ProtocolMessage::new(),
        };
        protocol_message.set_message_part(
            ProtocolMessagePartKey::NextAggregateVerificationKey,
            genesis_concatenation_avk,
        );

        if mithril_era != SupportedEra::Pythagoras {
            #[cfg(feature = "future_snark")]
            match genesis_avk.to_snark_aggregate_verification_key() {
                Some(snark_avk) => {
                    let genesis_snark_avk: ProtocolAggregateVerificationKeyForSnark =
                        ProtocolKey::new(snark_avk.to_owned());
                    protocol_message.set_message_part(
                        ProtocolMessagePartKey::NextSnarkAggregateVerificationKey,
                        genesis_snark_avk.to_bytes_hex()?,
                    );
                }
                None => {
                    warn!(
                        self.logger,
                        "SNARK aggregate verification key is unavailable, genesis certificate will not include SNARK AVK"
                    );
                }
            }
        }

        protocol_message.set_message_part(
            ProtocolMessagePartKey::NextProtocolParameters,
            genesis_protocol_parameters.compute_hash(),
        );
        protocol_message.set_message_part(
            ProtocolMessagePartKey::CurrentEpoch,
            genesis_epoch.to_string(),
        );
        Ok(protocol_message)
    }

    /// Sign the Genesis protocol message (test only)
    pub fn sign_genesis_protocol_message<T: ToMessage>(
        &self,
        genesis_message: T,
    ) -> Result<GenesisEd25519Signature, CertificateGenesisProducerError> {
        Ok(self
            .genesis_signer
            .as_ref()
            .ok_or_else(CertificateGenesisProducerError::MissingGenesisSigner)?
            .sign(genesis_message.to_message().as_bytes()))
    }

    /// Sign the rigid protocol-message preimage with the SNARK genesis signer.
    ///
    /// The function asserts the preimage is exactly [PREIMAGE_SIZE] bytes, hashes it once with
    /// SHA-256 to obtain the 32-byte digest the IVC gadget expects, and feeds the digest to the
    /// underlying `sign_standard` Schnorr routine via the wrapper.
    #[cfg(feature = "future_snark")]
    pub fn sign_genesis_protocol_message_snark(
        &self,
        protocol_message: &ProtocolMessage,
    ) -> Result<GenesisSchnorrSignature, CertificateGenesisProducerError> {
        let signer = self
            .snark_genesis_signer
            .as_ref()
            .ok_or(CertificateGenesisProducerError::MissingSnarkGenesisSigner)?;
        let preimage = protocol_message.rigid_preimage();
        if preimage.len() != PREIMAGE_SIZE {
            return Err(CertificateGenesisProducerError::InvalidPreimageSize {
                expected: PREIMAGE_SIZE,
                actual: preimage.len(),
            });
        }
        let digest = sha256_digest(&preimage);
        signer
            .sign_non_deterministic(&digest)
            .map_err(|e| CertificateGenesisProducerError::SnarkSigningFailure(e.to_string()))
    }

    /// Create a Genesis Certificate
    pub fn create_genesis_certificate<T: Into<String>>(
        &self,
        protocol_parameters: ProtocolParameters,
        network: T,
        epoch: Epoch,
        genesis_avk: ProtocolAggregateVerificationKey,
        genesis_ed25519_signature: GenesisEd25519Signature,
        mithril_era: SupportedEra,
    ) -> StdResult<Certificate> {
        let protocol_version = PROTOCOL_VERSION.to_string();
        let initiated_at = Utc::now();
        let sealed_at = Utc::now();
        let signers = vec![];
        let metadata = CertificateMetadata::new(
            network,
            protocol_version,
            protocol_parameters.clone(),
            initiated_at,
            sealed_at,
            signers,
        );
        let previous_hash = "".to_string();
        let genesis_protocol_message = self.create_genesis_protocol_message(
            &protocol_parameters,
            &genesis_avk,
            &epoch,
            mithril_era,
        )?;
        let signature = match mithril_era {
            SupportedEra::Pythagoras => {
                CertificateSignature::GenesisSignature(genesis_ed25519_signature)
            }
            #[cfg(feature = "future_snark")]
            SupportedEra::Lagrange => {
                let genesis_schnorr_signature =
                    self.sign_genesis_protocol_message_snark(&genesis_protocol_message)?;
                CertificateSignature::GenesisDualSignature(
                    genesis_ed25519_signature,
                    genesis_schnorr_signature,
                )
            }
            #[cfg(not(feature = "future_snark"))]
            SupportedEra::Lagrange => {
                CertificateSignature::GenesisSignature(genesis_ed25519_signature)
            }
        };
        Ok(Certificate::new(
            previous_hash,
            epoch,
            metadata,
            genesis_protocol_message,
            genesis_avk,
            signature,
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::entities::ProtocolMessagePartKey;
    use crate::test::TestLogger;
    use crate::test::builder::MithrilFixtureBuilder;

    #[test]
    fn genesis_protocol_message_has_expected_keys_and_values() {
        let fixture = MithrilFixtureBuilder::default().with_signers(5).build();
        let genesis_protocol_parameters = fixture.protocol_parameters();
        let genesis_avk = fixture.compute_aggregate_verification_key();
        let genesis_epoch = Epoch(123);
        let genesis_producer =
            CertificateGenesisProducer::new(None).with_logger(TestLogger::stdout());
        let protocol_message = genesis_producer
            .create_genesis_protocol_message(
                &genesis_protocol_parameters,
                &genesis_avk,
                &genesis_epoch,
                SupportedEra::Pythagoras,
            )
            .unwrap();

        let expected_genesis_avk_value =
            fixture.compute_and_encode_concatenation_aggregate_verification_key();
        assert_eq!(
            protocol_message
                .get_message_part(&ProtocolMessagePartKey::NextAggregateVerificationKey),
            Some(&expected_genesis_avk_value)
        );

        let expected_genesis_protocol_parameters_value = genesis_protocol_parameters.compute_hash();
        assert_eq!(
            protocol_message.get_message_part(&ProtocolMessagePartKey::NextProtocolParameters),
            Some(&expected_genesis_protocol_parameters_value)
        );

        let expected_genesis_epoch = genesis_epoch.to_string();
        assert_eq!(
            protocol_message.get_message_part(&ProtocolMessagePartKey::CurrentEpoch),
            Some(&expected_genesis_epoch)
        );
    }

    #[cfg(feature = "future_snark")]
    #[test]
    fn genesis_protocol_message_includes_snark_aggregate_verification_key() {
        let fixture = MithrilFixtureBuilder::default().with_signers(5).build();
        let genesis_protocol_parameters = fixture.protocol_parameters();
        let genesis_avk = fixture.compute_aggregate_verification_key();
        let genesis_epoch = Epoch(123);
        let genesis_producer =
            CertificateGenesisProducer::new(None).with_logger(TestLogger::stdout());
        let protocol_message = genesis_producer
            .create_genesis_protocol_message(
                &genesis_protocol_parameters,
                &genesis_avk,
                &genesis_epoch,
                SupportedEra::Lagrange,
            )
            .unwrap();

        let expected_snark_avk_value = fixture
            .compute_and_encode_snark_aggregate_verification_key()
            .expect("SNARK AVK should be available");
        assert_eq!(
            protocol_message
                .get_message_part(&ProtocolMessagePartKey::NextSnarkAggregateVerificationKey),
            Some(&expected_snark_avk_value)
        );
    }

    #[cfg(feature = "future_snark")]
    mod era_dispatched_genesis_certificate {
        use rand_chacha::ChaCha20Rng;
        use rand_core::SeedableRng;

        use crate::crypto_helper::{
            GenesisEd25519Signer, GenesisSchnorrSigner, GenesisSchnorrVerifier,
        };

        use super::*;

        fn build_ed25519_signer() -> Arc<GenesisEd25519Signer> {
            Arc::new(GenesisEd25519Signer::create_deterministic_signer())
        }

        fn build_schnorr_signer() -> Arc<GenesisSchnorrSigner> {
            let mut rng = ChaCha20Rng::from_seed([13u8; 32]);
            Arc::new(GenesisSchnorrSigner::generate(&mut rng))
        }

        fn build_genesis_signature(
            signer: &GenesisEd25519Signer,
            protocol_message: &ProtocolMessage,
        ) -> GenesisEd25519Signature {
            signer.sign(protocol_message.compute_hash().as_bytes())
        }

        #[test]
        fn pythagoras_creates_a_single_signature_variant() {
            let fixture = MithrilFixtureBuilder::default().with_signers(3).build();
            let ed25519_signer = build_ed25519_signer();
            let producer = CertificateGenesisProducer::new(Some(ed25519_signer.clone()))
                .with_logger(TestLogger::stdout());
            let protocol_message = producer
                .create_genesis_protocol_message(
                    &fixture.protocol_parameters(),
                    &fixture.compute_aggregate_verification_key(),
                    &Epoch(1),
                    SupportedEra::Pythagoras,
                )
                .unwrap();
            let genesis_signature = build_genesis_signature(&ed25519_signer, &protocol_message);

            let certificate = producer
                .create_genesis_certificate(
                    fixture.protocol_parameters(),
                    "testnet",
                    Epoch(1),
                    fixture.compute_aggregate_verification_key(),
                    genesis_signature,
                    SupportedEra::Pythagoras,
                )
                .unwrap();

            assert!(matches!(
                certificate.signature,
                CertificateSignature::GenesisSignature(_)
            ));
        }

        #[test]
        fn lagrange_creates_a_dual_signature_variant() {
            let fixture = MithrilFixtureBuilder::default().with_signers(3).build();
            let ed25519_signer = build_ed25519_signer();
            let schnorr_signer = build_schnorr_signer();
            let producer = CertificateGenesisProducer::new(Some(ed25519_signer.clone()))
                .with_snark_genesis_signer(schnorr_signer.clone())
                .with_logger(TestLogger::stdout());
            let protocol_message = producer
                .create_genesis_protocol_message(
                    &fixture.protocol_parameters(),
                    &fixture.compute_aggregate_verification_key(),
                    &Epoch(1),
                    SupportedEra::Lagrange,
                )
                .unwrap();
            let genesis_signature = build_genesis_signature(&ed25519_signer, &protocol_message);

            let certificate = producer
                .create_genesis_certificate(
                    fixture.protocol_parameters(),
                    "testnet",
                    Epoch(1),
                    fixture.compute_aggregate_verification_key(),
                    genesis_signature,
                    SupportedEra::Lagrange,
                )
                .unwrap();

            let (genesis_ed25519_signature, genesis_schnorr_signature) = match certificate.signature
            {
                CertificateSignature::GenesisDualSignature(ed25519, schnorr) => (ed25519, schnorr),
                other => panic!("expected GenesisDualSignature, got {other:?}"),
            };

            // Cross-check the Schnorr signature verifies under the SNARK signer's verifier with
            // the rigid preimage SHA-256 digest, matching the in-circuit encoding.
            let verifier =
                GenesisSchnorrVerifier::from_verification_key(schnorr_signer.verification_key());
            let preimage = certificate.protocol_message.rigid_preimage();
            assert_eq!(preimage.len(), PREIMAGE_SIZE);
            let digest = sha256_digest(&preimage);
            verifier.verify(&digest, &genesis_schnorr_signature).expect(
                "Schnorr signature produced by the dual-genesis path must verify against the same digest",
            );
            // Sanity: the Ed25519 signature still verifies against the legacy signed_message.
            ed25519_signer
                .create_verifier()
                .verify(
                    certificate.signed_message.as_bytes(),
                    &genesis_ed25519_signature,
                )
                .expect("Ed25519 signature must verify against the legacy signed_message bytes");
        }

        #[test]
        fn preimage_size_constant_matches_rigid_protocol_message_preimage_length() {
            let mut rigid = ProtocolMessage::new_rigid();
            rigid.set_message_part(ProtocolMessagePartKey::CurrentEpoch, "1".to_string());

            assert_eq!(
                rigid.rigid_preimage().len(),
                PREIMAGE_SIZE,
                "Rigid protocol-message preimage length must match the PREIMAGE_SIZE constant \
                 pinned by the IVC gadget; any drift here is the regression the runtime \
                 InvalidPreimageSize guard exists to catch"
            );
        }

        #[test]
        fn lagrange_fails_without_a_schnorr_signer() {
            let fixture = MithrilFixtureBuilder::default().with_signers(3).build();
            let ed25519_signer = build_ed25519_signer();
            let producer = CertificateGenesisProducer::new(Some(ed25519_signer.clone()))
                .with_logger(TestLogger::stdout());
            let protocol_message = producer
                .create_genesis_protocol_message(
                    &fixture.protocol_parameters(),
                    &fixture.compute_aggregate_verification_key(),
                    &Epoch(1),
                    SupportedEra::Lagrange,
                )
                .unwrap();
            let genesis_signature = build_genesis_signature(&ed25519_signer, &protocol_message);

            let error = producer
                .create_genesis_certificate(
                    fixture.protocol_parameters(),
                    "testnet",
                    Epoch(1),
                    fixture.compute_aggregate_verification_key(),
                    genesis_signature,
                    SupportedEra::Lagrange,
                )
                .unwrap_err();

            let downcast = error
                .downcast_ref::<CertificateGenesisProducerError>()
                .expect("Producer must surface CertificateGenesisProducerError");
            assert!(matches!(
                downcast,
                CertificateGenesisProducerError::MissingSnarkGenesisSigner
            ));
        }
    }
}
