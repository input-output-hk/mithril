//! A module used to create a Genesis Certificate
//!
use anyhow::anyhow;
use chrono::prelude::*;
#[cfg(feature = "future_snark")]
use slog::warn;
use slog::{Logger, o};

#[cfg(feature = "future_snark")]
use crate::crypto_helper::ProtocolAggregateVerificationKeyForSnark;
use crate::{
    StdResult,
    crypto_helper::{PROTOCOL_VERSION, ProtocolAggregateVerificationKey, ProtocolKey},
    entities::{
        Certificate, CertificateMetadata, CertificateSignature, Epoch, ProtocolMessage,
        ProtocolMessagePartKey, ProtocolParameters, SupportedEra,
    },
};

/// CertificateGenesisProducer is in charge of producing a Genesis Certificate
#[derive(Debug)]
pub struct CertificateGenesisProducer {
    logger: Logger,
}

impl Default for CertificateGenesisProducer {
    fn default() -> Self {
        Self::new()
    }
}

impl CertificateGenesisProducer {
    /// CertificateGenesisProducer factory
    pub fn new() -> Self {
        Self {
            logger: Logger::root(slog::Discard, o!()),
        }
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

    /// Assemble a Genesis Certificate from an already-produced [CertificateSignature].
    ///
    /// Signing is performed upstream by [`GenesisSigner`][crate::crypto_helper::GenesisSigner];
    /// this only builds the certificate body. Accepts both the single Ed25519 and the dual
    /// (Ed25519 + Schnorr) genesis signatures, and rejects a multi-signature to preserve the
    /// genesis-certificate invariant relied upon by chain verification.
    pub fn create_genesis_certificate<T: Into<String>>(
        &self,
        protocol_parameters: ProtocolParameters,
        network: T,
        epoch: Epoch,
        genesis_avk: ProtocolAggregateVerificationKey,
        signature: CertificateSignature,
        mithril_era: SupportedEra,
    ) -> StdResult<Certificate> {
        if matches!(signature, CertificateSignature::MultiSignature(..)) {
            return Err(anyhow!(
                "Can not create a genesis certificate from a multi-signature"
            ));
        }
        let metadata = CertificateMetadata::new(
            network,
            PROTOCOL_VERSION.to_string(),
            protocol_parameters.clone(),
            Utc::now(),
            Utc::now(),
            vec![],
        );
        let genesis_protocol_message = self.create_genesis_protocol_message(
            &protocol_parameters,
            &genesis_avk,
            &epoch,
            mithril_era,
        )?;
        Ok(Certificate::new(
            "".to_string(),
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

    use crate::entities::{ProtocolMessagePartKey, SignedEntityType};
    use crate::test::TestLogger;
    use crate::test::builder::MithrilFixtureBuilder;
    use crate::test::double::fake_keys;

    #[test]
    fn create_genesis_certificate_rejects_a_multi_signature() {
        let fixture = MithrilFixtureBuilder::default().with_signers(3).build();
        let producer = CertificateGenesisProducer::new().with_logger(TestLogger::stdout());
        let multi_signature = CertificateSignature::MultiSignature(
            SignedEntityType::MithrilStakeDistribution(Epoch(1)),
            fake_keys::multi_signature()[0].try_into().unwrap(),
        );

        producer
            .create_genesis_certificate(
                fixture.protocol_parameters(),
                "testnet",
                Epoch(1),
                fixture.compute_aggregate_verification_key(),
                multi_signature,
                SupportedEra::Pythagoras,
            )
            .expect_err("a multi-signature must be rejected as a genesis signature");
    }

    #[test]
    fn genesis_protocol_message_has_expected_keys_and_values() {
        let fixture = MithrilFixtureBuilder::default().with_signers(5).build();
        let genesis_protocol_parameters = fixture.protocol_parameters();
        let genesis_avk = fixture.compute_aggregate_verification_key();
        let genesis_epoch = Epoch(123);
        let genesis_producer = CertificateGenesisProducer::new().with_logger(TestLogger::stdout());
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
        let genesis_producer = CertificateGenesisProducer::new().with_logger(TestLogger::stdout());
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
        use crate::crypto_helper::{
            GenesisBundleError, GenesisEd25519Signer, GenesisSchnorrVerifier, GenesisSigner,
            PREIMAGE_SIZE, sha256_digest,
        };

        use super::*;

        #[test]
        fn pythagoras_creates_a_single_signature_variant() {
            let fixture = MithrilFixtureBuilder::default().with_signers(3).build();
            let genesis_signer = GenesisSigner::create_deterministic_signer();
            let producer = CertificateGenesisProducer::new().with_logger(TestLogger::stdout());
            let protocol_message = producer
                .create_genesis_protocol_message(
                    &fixture.protocol_parameters(),
                    &fixture.compute_aggregate_verification_key(),
                    &Epoch(1),
                    SupportedEra::Pythagoras,
                )
                .unwrap();
            let signature = genesis_signer
                .sign_deterministic(&protocol_message, SupportedEra::Pythagoras)
                .unwrap();

            let certificate = producer
                .create_genesis_certificate(
                    fixture.protocol_parameters(),
                    "testnet",
                    Epoch(1),
                    fixture.compute_aggregate_verification_key(),
                    signature,
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
            let genesis_signer = GenesisSigner::create_deterministic_signer();
            let producer = CertificateGenesisProducer::new().with_logger(TestLogger::stdout());
            let protocol_message = producer
                .create_genesis_protocol_message(
                    &fixture.protocol_parameters(),
                    &fixture.compute_aggregate_verification_key(),
                    &Epoch(1),
                    SupportedEra::Lagrange,
                )
                .unwrap();
            let signature = genesis_signer
                .sign_deterministic(&protocol_message, SupportedEra::Lagrange)
                .unwrap();

            let certificate = producer
                .create_genesis_certificate(
                    fixture.protocol_parameters(),
                    "testnet",
                    Epoch(1),
                    fixture.compute_aggregate_verification_key(),
                    signature,
                    SupportedEra::Lagrange,
                )
                .unwrap();

            let (genesis_ed25519_signature, genesis_schnorr_signature) = match certificate.signature
            {
                CertificateSignature::GenesisDualSignature(ed25519, schnorr) => (ed25519, schnorr),
                other => panic!("expected GenesisDualSignature, got {other:?}"),
            };

            let verifier = GenesisSchnorrVerifier::from_verification_key(
                genesis_signer.schnorr.as_ref().unwrap().verification_key(),
            );
            let preimage = certificate.protocol_message.rigid_preimage();
            assert_eq!(preimage.len(), PREIMAGE_SIZE);
            let digest = sha256_digest(&preimage);
            verifier.verify(&digest, &genesis_schnorr_signature).expect(
                "Schnorr signature produced by the dual-genesis path must verify against the same digest",
            );
            genesis_signer
                .ed25519
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
                 pinned by the IVC gadget; any drift here is the regression the signer's \
                 preimage-size guard exists to catch"
            );
        }

        #[test]
        fn lagrange_fails_without_a_schnorr_signer() {
            let fixture = MithrilFixtureBuilder::default().with_signers(3).build();
            let genesis_signer =
                GenesisSigner::from_ed25519(GenesisEd25519Signer::create_deterministic_signer());
            let producer = CertificateGenesisProducer::new().with_logger(TestLogger::stdout());
            let protocol_message = producer
                .create_genesis_protocol_message(
                    &fixture.protocol_parameters(),
                    &fixture.compute_aggregate_verification_key(),
                    &Epoch(1),
                    SupportedEra::Lagrange,
                )
                .unwrap();

            let error = genesis_signer
                .sign_deterministic(&protocol_message, SupportedEra::Lagrange)
                .unwrap_err();

            assert!(matches!(
                error.downcast_ref::<GenesisBundleError>(),
                Some(GenesisBundleError::LegacySigningKey)
            ));
        }
    }
}
