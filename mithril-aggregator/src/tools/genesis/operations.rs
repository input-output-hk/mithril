use std::{
    fs::File,
    io::{Write, prelude::*},
    path::{Path, PathBuf},
    sync::Arc,
};

use anyhow::Context;
use slog::Logger;

use mithril_common::{
    CardanoNetwork, StdResult,
    certificate_chain::{CertificateGenesisProducer, CertificateVerifier},
    crypto_helper::{
        GenesisEd25519Signature, GenesisEd25519Signer, GenesisEd25519VerificationKey,
        GenesisSigner, ProtocolAggregateVerificationKey,
    },
    entities::{Epoch, ProtocolParameters, SupportedEra},
    protocol::SignerBuilder,
};

#[cfg(feature = "future_snark")]
use crate::tools::GenesisSignedPayload;
use crate::{
    database::repository::CertificateRepository,
    dependency_injection::GenesisCommandDependenciesContainer,
};
#[cfg(feature = "future_snark")]
use mithril_common::crypto_helper::{
    GenesisSchnorrSigner, GenesisSigningKeyBundle, GenesisVerificationKeyBundle, ProtocolKey,
    sha256_digest, signed_message_from_digest,
};

/// Configuration for the genesis tools.
pub struct GenesisToolsConfiguration {
    /// Cardano network.
    pub network: CardanoNetwork,

    /// Current epoch.
    pub epoch: Epoch,

    /// Aggregate verification key for the genesis stake distribution.
    pub genesis_avk: ProtocolAggregateVerificationKey,

    /// Protocol parameters for the genesis stake distribution.
    pub genesis_protocol_parameters: ProtocolParameters,

    /// Mithril era to use for the genesis certificate.
    pub mithril_era: SupportedEra,
}

/// Genesis tools for creating and managing genesis certificates.
pub struct GenesisTools {
    configuration: GenesisToolsConfiguration,
    certificate_verifier: Arc<dyn CertificateVerifier>,
    certificate_repository: Arc<CertificateRepository>,
    logger: Logger,
}

impl GenesisTools {
    /// GenesisTools factory
    pub fn new(
        configuration: GenesisToolsConfiguration,
        certificate_verifier: Arc<dyn CertificateVerifier>,
        certificate_repository: Arc<CertificateRepository>,
        logger: Logger,
    ) -> Self {
        Self {
            configuration,
            certificate_verifier,
            certificate_repository,
            logger,
        }
    }

    pub async fn from_dependencies(
        dependencies: GenesisCommandDependenciesContainer,
    ) -> StdResult<Self> {
        let epoch = dependencies
            .chain_observer
            .get_current_epoch()
            .await?
            .with_context(|| "Chain observer can not retrieve current epoch")?;
        let certificate_verifier = dependencies.certificate_verifier.clone();
        let certificate_repository = dependencies.certificate_repository.clone();
        let protocol_parameters_retriever = dependencies.protocol_parameters_retriever.clone();
        let genesis_avk_epoch = epoch.offset_to_next_signer_retrieval_epoch();
        let genesis_protocol_parameters = protocol_parameters_retriever
            .get_protocol_parameters(epoch.offset_to_signer_retrieval_epoch()?)
            .await?
            .with_context(|| {
                format!("Missing protocol parameters for epoch {genesis_avk_epoch}")
            })?;
        let genesis_signers = dependencies
            .verification_key_store
            .get_signers(genesis_avk_epoch)
            .await?
            .with_context(|| format!("Missing signers for epoch {genesis_avk_epoch}"))?;

        let protocol_multi_signer =
            SignerBuilder::new(&genesis_signers, &genesis_protocol_parameters)
                .with_context(|| "Could not build a multi signer to compute the genesis avk")?
                .build_multi_signer();
        let genesis_avk = protocol_multi_signer.compute_aggregate_verification_key();

        let configuration = GenesisToolsConfiguration {
            network: dependencies.network,
            epoch,
            genesis_avk,
            genesis_protocol_parameters,
            mithril_era: dependencies.mithril_era,
        };

        Ok(Self::new(
            configuration,
            certificate_verifier,
            certificate_repository,
            dependencies.logger,
        ))
    }

    /// Export AVK of the genesis stake distribution to a payload file.
    ///
    /// Pythagoras writes the canonical protocol-message hash bytes (hex string); Lagrange writes
    /// the rigid protocol-message preimage so the offline sign tool can produce both the legacy
    /// Ed25519 signature and the SNARK-friendly Schnorr signature.
    pub fn export_payload_to_sign(&self, target_path: &Path) -> StdResult<()> {
        let mut target_file = File::create(target_path)?;
        let genesis_producer =
            CertificateGenesisProducer::new(None).with_logger(self.logger.clone());
        let protocol_message = genesis_producer.create_genesis_protocol_message(
            &self.configuration.genesis_protocol_parameters,
            &self.configuration.genesis_avk,
            &self.configuration.epoch,
            self.configuration.mithril_era,
        )?;
        match self.configuration.mithril_era {
            #[cfg(feature = "future_snark")]
            SupportedEra::Lagrange => {
                target_file.write_all(&protocol_message.rigid_preimage())?;
            }
            _ => {
                target_file.write_all(protocol_message.compute_hash().as_bytes())?;
            }
        }
        Ok(())
    }

    /// Import signature of the AVK of the genesis stake distribution from a file
    pub async fn import_payload_signature(
        &self,
        signed_payload_path: &Path,
        genesis_verification_key: &GenesisEd25519VerificationKey,
    ) -> StdResult<()> {
        let mut signed_payload_file = File::open(signed_payload_path).unwrap();
        let mut signed_payload_buffer = Vec::new();
        signed_payload_file.read_to_end(&mut signed_payload_buffer)?;
        let genesis_signature = GenesisEd25519Signature::from_bytes(&signed_payload_buffer)?;

        self.create_and_save_genesis_certificate(genesis_signature, genesis_verification_key)
            .await
    }

    /// Automatic bootstrap of the genesis certificate (test only).
    ///
    /// Pythagoras runs a single Ed25519 ceremony; Lagrange runs the dual ceremony with the SNARK
    /// signer attached to the producer.
    pub async fn bootstrap_test_genesis_certificate(
        &self,
        genesis_signer: GenesisSigner,
    ) -> StdResult<()> {
        #[cfg(feature = "future_snark")]
        let genesis_signer = if matches!(self.configuration.mithril_era, SupportedEra::Lagrange)
            && genesis_signer.schnorr.is_none()
        {
            GenesisSigner {
                ed25519: genesis_signer.ed25519,
                schnorr: Some(GenesisSchnorrSigner::create_non_deterministic_signer()),
            }
        } else {
            genesis_signer
        };
        genesis_signer.ensure_supports_era(self.configuration.mithril_era)?;
        let ed25519_verification_key = genesis_signer.ed25519.verification_key();
        let genesis_producer =
            CertificateGenesisProducer::new(Some(Arc::new(genesis_signer.ed25519)))
                .with_logger(self.logger.clone());
        #[cfg(feature = "future_snark")]
        let genesis_producer = match genesis_signer.schnorr {
            Some(schnorr_signer) => {
                genesis_producer.with_schnorr_genesis_signer(Arc::new(schnorr_signer))
            }
            None => genesis_producer,
        };
        let genesis_protocol_message = genesis_producer.create_genesis_protocol_message(
            &self.configuration.genesis_protocol_parameters,
            &self.configuration.genesis_avk,
            &self.configuration.epoch,
            self.configuration.mithril_era,
        )?;
        let genesis_signature =
            genesis_producer.sign_genesis_protocol_message(genesis_protocol_message)?;
        let genesis_certificate = genesis_producer.create_genesis_certificate(
            self.configuration.genesis_protocol_parameters.clone(),
            self.configuration.network,
            self.configuration.epoch,
            self.configuration.genesis_avk.clone(),
            genesis_signature,
            self.configuration.mithril_era,
        )?;
        self.certificate_verifier
            .verify_genesis_certificate(&genesis_certificate, &ed25519_verification_key)
            .await?;
        self.certificate_repository
            .create_certificate(genesis_certificate)
            .await
            .with_context(|| "Genesis tool can not save bootstrap genesis certificate")?;
        Ok(())
    }

    /// Sign the genesis certificate offline.
    ///
    /// Pythagoras signs the payload bytes with the ed25519 signer and writes the raw
    /// Ed25519 signature. Lagrange hashes the rigid preimage with SHA-256, signs the
    /// hex-encoded digest with the ed25519 signer (matching the `signed_message` bytes),
    /// signs the raw digest with the SNARK signer, and writes a versioned dual-signature
    /// envelope ([`crate::tools::GenesisSignedPayload`]).
    pub async fn sign_genesis_certificate(
        to_sign_payload_path: &Path,
        target_signed_payload_path: &Path,
        genesis_secret_key_path: &Path,
        mithril_era: SupportedEra,
    ) -> StdResult<()> {
        let genesis_signer = GenesisSigner::read_from_file(genesis_secret_key_path)?;
        genesis_signer.ensure_supports_era(mithril_era)?;

        let mut to_sign_payload_file = File::open(to_sign_payload_path)?;
        let mut to_sign_payload_buffer = Vec::new();
        to_sign_payload_file.read_to_end(&mut to_sign_payload_buffer)?;

        let signed_payload = match mithril_era {
            #[cfg(feature = "future_snark")]
            SupportedEra::Lagrange => {
                let digest = sha256_digest(&to_sign_payload_buffer);
                let signed_message = signed_message_from_digest(&digest);
                let ed25519 = genesis_signer.ed25519.sign(signed_message.as_bytes());
                let schnorr = genesis_signer
                    .schnorr
                    .as_ref()
                    .ok_or_else(|| {
                        anyhow::anyhow!("Lagrange genesis sign requires a SNARK signer")
                    })?
                    .sign_non_deterministic(&digest)?;
                GenesisSignedPayload::new(ed25519, schnorr).to_bytes()
            }
            _ => genesis_signer
                .ed25519
                .sign(&to_sign_payload_buffer)
                .to_bytes()
                .to_vec(),
        };

        let mut target_signed_payload_file = File::create(target_signed_payload_path)?;
        target_signed_payload_file.write_all(&signed_payload)?;

        Ok(())
    }

    async fn create_and_save_genesis_certificate(
        &self,
        genesis_signature: GenesisEd25519Signature,
        genesis_verification_key: &GenesisEd25519VerificationKey,
    ) -> StdResult<()> {
        let genesis_producer =
            CertificateGenesisProducer::new(None).with_logger(self.logger.clone());
        let genesis_certificate = genesis_producer.create_genesis_certificate(
            self.configuration.genesis_protocol_parameters.clone(),
            self.configuration.network,
            self.configuration.epoch,
            self.configuration.genesis_avk.clone(),
            genesis_signature,
            self.configuration.mithril_era,
        )?;
        self.certificate_verifier
            .verify_genesis_certificate(&genesis_certificate, genesis_verification_key)
            .await?;
        self.certificate_repository
            .create_certificate(genesis_certificate.clone())
            .await
            .with_context(|| {
                format!(
                    "Genesis tool can not create certificate with genesis signature: '{genesis_signature:?}'"
                )
            })?;
        Ok(())
    }

    /// Export the genesis keypair to a folder and return the paths to the files (secret key, verification key).
    ///
    /// Pythagoras writes the legacy single-Ed25519 JSON-hex layout; Lagrange writes a dual signing
    /// bundle plus its matching verification bundle as bytes-hex.
    pub fn create_and_save_genesis_keypair(
        keypair_path: &Path,
        mithril_era: SupportedEra,
    ) -> StdResult<(PathBuf, PathBuf)> {
        let genesis_secret_key_path = keypair_path.join("genesis.sk");
        let genesis_verification_key_path = keypair_path.join("genesis.vk");
        match mithril_era {
            SupportedEra::Pythagoras => {
                let signer = GenesisEd25519Signer::create_non_deterministic_signer();
                signer.secret_key().write_json_hex_to_file(&genesis_secret_key_path)?;
                signer
                    .verification_key()
                    .write_json_hex_to_file(&genesis_verification_key_path)?;
            }
            #[cfg(feature = "future_snark")]
            SupportedEra::Lagrange => {
                let ed25519 = GenesisEd25519Signer::create_non_deterministic_signer();
                let schnorr = GenesisSchnorrSigner::create_non_deterministic_signer();

                let signing_bundle =
                    GenesisSigningKeyBundle::new(ed25519.secret_key(), schnorr.secret_key());
                let verification_bundle = GenesisVerificationKeyBundle::new(
                    ed25519.verification_key(),
                    schnorr.verification_key(),
                );

                std::fs::write(
                    &genesis_secret_key_path,
                    ProtocolKey::new(signing_bundle).to_bytes_hex()?,
                )?;
                std::fs::write(
                    &genesis_verification_key_path,
                    ProtocolKey::new(verification_bundle).to_bytes_hex()?,
                )?;
            }
            #[cfg(not(feature = "future_snark"))]
            SupportedEra::Lagrange => {
                return Err(anyhow::anyhow!(
                    "Lagrange genesis keypair generation requires the 'future_snark' build feature"
                ));
            }
        }
        Ok((genesis_secret_key_path, genesis_verification_key_path))
    }
}

#[cfg(test)]
mod tests {
    use std::{fs::read_to_string, path::PathBuf};

    #[cfg(feature = "future_snark")]
    use mithril_common::crypto_helper::{
        BUNDLE_FIRST_HEX_CHAR, GenesisSchnorrSigner, GenesisSigningKeyBundle,
        GenesisVerificationKeyBundle,
    };
    use mithril_common::{
        certificate_chain::MithrilCertificateVerifier,
        crypto_helper::{
            GenesisEd25519SecretKey, GenesisEd25519Signer, GenesisEd25519VerificationKey,
            GenesisEd25519Verifier,
        },
        entities::Certificate,
        test::{TempDir, builder::MithrilFixtureBuilder, double::fake_data},
    };

    use crate::database::test_helper::main_db_connection;
    use crate::test::TestLogger;

    use super::*;

    fn get_temp_dir(dir_name: &str) -> PathBuf {
        TempDir::create("genesis", dir_name)
    }

    fn create_fake_genesis_avk() -> ProtocolAggregateVerificationKey {
        let fixture = MithrilFixtureBuilder::default().with_signers(5).build();

        fixture.compute_aggregate_verification_key()
    }

    fn build_tools(
        genesis_signer: &GenesisEd25519Signer,
    ) -> (
        GenesisTools,
        Arc<CertificateRepository>,
        Arc<GenesisEd25519Verifier>,
        Arc<dyn CertificateVerifier>,
    ) {
        build_tools_for_era(genesis_signer, SupportedEra::Pythagoras)
    }

    fn build_tools_for_era(
        genesis_signer: &GenesisEd25519Signer,
        mithril_era: SupportedEra,
    ) -> (
        GenesisTools,
        Arc<CertificateRepository>,
        Arc<GenesisEd25519Verifier>,
        Arc<dyn CertificateVerifier>,
    ) {
        let connection = main_db_connection().unwrap();
        let certificate_store = Arc::new(CertificateRepository::new(Arc::new(connection)));
        let certificate_verifier = Arc::new(MithrilCertificateVerifier::new(
            TestLogger::stdout(),
            certificate_store.clone(),
        ));
        let genesis_avk = create_fake_genesis_avk();
        let genesis_verifier = Arc::new(genesis_signer.create_verifier());
        let configuration = GenesisToolsConfiguration {
            network: fake_data::network(),
            epoch: Epoch(10),
            genesis_avk,
            genesis_protocol_parameters: fake_data::protocol_parameters(),
            mithril_era,
        };
        let genesis_tools = GenesisTools::new(
            configuration,
            certificate_verifier.clone(),
            certificate_store.clone(),
            TestLogger::stdout(),
        );

        (
            genesis_tools,
            certificate_store,
            genesis_verifier,
            certificate_verifier,
        )
    }

    #[tokio::test]
    async fn export_sign_then_import_genesis_payload() {
        let test_dir = get_temp_dir("export_payload_to_sign");
        let payload_path = test_dir.join("payload.txt");
        let signed_payload_path = test_dir.join("payload-signed.txt");
        let (genesis_secret_key_path, _) =
            GenesisTools::create_and_save_genesis_keypair(&test_dir, SupportedEra::Pythagoras)
                .expect("exporting the keypair should not fail");
        let genesis_secret_key = GenesisEd25519SecretKey::from_json_hex(
            &read_to_string(&genesis_secret_key_path)
                .expect("reading genesis secret key file should not fail"),
        )
        .expect("parsing genesis secret key should not fail");
        let genesis_signer = GenesisEd25519Signer::from_secret_key(genesis_secret_key);
        let (genesis_tools, certificate_store, genesis_verifier, certificate_verifier) =
            build_tools(&genesis_signer);

        genesis_tools
            .export_payload_to_sign(&payload_path)
            .expect("export_payload_to_sign should not fail");
        GenesisTools::sign_genesis_certificate(
            &payload_path,
            &signed_payload_path,
            &genesis_secret_key_path,
            SupportedEra::Pythagoras,
        )
        .await
        .expect("sign_genesis_certificate should not fail");
        genesis_tools
            .import_payload_signature(
                &signed_payload_path,
                &genesis_verifier.to_verification_key(),
            )
            .await
            .expect("import_payload_signature should not fail");

        let last_certificates = certificate_store.get_latest_certificates(10).await.unwrap();

        assert_eq!(1, last_certificates.len());
        certificate_verifier
            .verify_genesis_certificate(
                &last_certificates[0],
                &genesis_verifier.to_verification_key(),
            )
            .await
            .expect(
                "verify_genesis_certificate should successfully validate the genesis certificate",
            );
    }

    #[tokio::test]
    async fn bootstrap_test_genesis_certificate_works() {
        let ed25519_signer = GenesisEd25519Signer::create_deterministic_signer();
        let (genesis_tools, certificate_store, genesis_verifier, certificate_verifier) =
            build_tools(&ed25519_signer);

        genesis_tools
            .bootstrap_test_genesis_certificate(GenesisSigner::from_ed25519(ed25519_signer))
            .await
            .expect("bootstrap test genesis certificate should not fail");

        let last_certificates = certificate_store.get_latest_certificates(10).await.unwrap();

        assert_eq!(1, last_certificates.len());
        certificate_verifier
            .verify_genesis_certificate(
                &last_certificates[0],
                &genesis_verifier.to_verification_key(),
            )
            .await
            .expect(
                "verify_genesis_certificate should successfully validate the genesis certificate",
            );
    }

    #[test]
    fn create_and_save_genesis_keypair_pythagoras() {
        let temp_dir = get_temp_dir("create_and_save_genesis_keypair_pythagoras");
        let (genesis_secret_key_path, genesis_verification_key_path) =
            GenesisTools::create_and_save_genesis_keypair(&temp_dir, SupportedEra::Pythagoras)
                .expect("Failed to create and save genesis keypair");
        let genesis_secret_key = GenesisEd25519SecretKey::from_json_hex(
            &read_to_string(&genesis_secret_key_path)
                .expect("Failed to read genesis secret key file"),
        )
        .expect("Failed to parse genesis secret key");
        let genesis_verification_key = GenesisEd25519VerificationKey::from_json_hex(
            &read_to_string(&genesis_verification_key_path)
                .expect("Failed to read genesis verification key file"),
        )
        .expect("Failed to parse genesis verification key");
        let genesis_verifier =
            GenesisEd25519Signer::from_secret_key(genesis_secret_key).create_verifier();

        let expected_genesis_verification_key = genesis_verifier.to_verification_key();
        assert_eq!(expected_genesis_verification_key, genesis_verification_key);
    }

    #[cfg(feature = "future_snark")]
    #[test]
    fn create_and_save_genesis_keypair_lagrange() {
        let temp_dir = get_temp_dir("create_and_save_genesis_keypair_lagrange");
        let (genesis_secret_key_path, genesis_verification_key_path) =
            GenesisTools::create_and_save_genesis_keypair(&temp_dir, SupportedEra::Lagrange)
                .expect("Failed to create and save genesis keypair");

        let signing_hex = read_to_string(&genesis_secret_key_path).unwrap();
        let verification_hex = read_to_string(&genesis_verification_key_path).unwrap();

        assert_eq!(signing_hex.as_bytes()[0], BUNDLE_FIRST_HEX_CHAR);
        assert_eq!(verification_hex.as_bytes()[0], BUNDLE_FIRST_HEX_CHAR);

        let signing_bundle = GenesisSigningKeyBundle::try_from_hex(&signing_hex)
            .expect("signing bundle hex must round-trip");
        let verification_bundle =
            GenesisVerificationKeyBundle::try_from_hex_or_legacy(&verification_hex)
                .expect("verification bundle hex must round-trip");
        let expected_schnorr_verification_key =
            GenesisSchnorrSigner::from_secret_key(signing_bundle.schnorr).verification_key();

        assert_eq!(
            verification_bundle.schnorr.unwrap().to_bytes(),
            expected_schnorr_verification_key.to_bytes()
        );
    }

    #[cfg(feature = "future_snark")]
    mod bootstrap {
        use mithril_common::crypto_helper::{GenesisSchnorrSigner, GenesisSigningKeyBundle};

        use super::*;

        fn build_dual_signer() -> (GenesisEd25519Signer, GenesisSigner) {
            let ed25519 = GenesisEd25519Signer::create_deterministic_signer();
            let schnorr = GenesisSchnorrSigner::create_non_deterministic_signer();
            let bundle = GenesisSigningKeyBundle::new(ed25519.secret_key(), schnorr.secret_key());
            (ed25519.clone(), GenesisSigner::from_bundle(bundle))
        }

        #[tokio::test]
        async fn pythagoras_accepts_dual_signing_bundle() {
            let (ed25519_signer, signer) = build_dual_signer();
            let (genesis_tools, certificate_store, genesis_verifier, certificate_verifier) =
                build_tools_for_era(&ed25519_signer, SupportedEra::Pythagoras);

            genesis_tools
                .bootstrap_test_genesis_certificate(signer)
                .await
                .expect("Pythagoras must accept a dual bundle");

            let last = certificate_store.get_latest_certificates(10).await.unwrap();
            assert_eq!(1, last.len());
            certificate_verifier
                .verify_genesis_certificate(&last[0], &genesis_verifier.to_verification_key())
                .await
                .expect("Ed25519 verification must pass");
        }

        #[tokio::test]
        async fn lagrange_runs_dual_ceremony() {
            let (ed25519_signer, signer) = build_dual_signer();
            let (genesis_tools, certificate_store, genesis_verifier, certificate_verifier) =
                build_tools_for_era(&ed25519_signer, SupportedEra::Lagrange);

            genesis_tools
                .bootstrap_test_genesis_certificate(signer)
                .await
                .expect("Lagrange dual bootstrap must succeed");

            let last = certificate_store.get_latest_certificates(10).await.unwrap();
            assert_eq!(1, last.len());
            certificate_verifier
                .verify_genesis_certificate(&last[0], &genesis_verifier.to_verification_key())
                .await
                .expect("Ed25519 verification must pass on the dual variant");
        }

        #[tokio::test]
        async fn lagrange_auto_augments_a_legacy_signer_with_a_fresh_schnorr_signer() {
            let ed25519_signer = GenesisEd25519Signer::create_deterministic_signer();
            let (genesis_tools, certificate_store, _, _) =
                build_tools_for_era(&ed25519_signer, SupportedEra::Lagrange);

            genesis_tools
                .bootstrap_test_genesis_certificate(GenesisSigner::from_ed25519(ed25519_signer))
                .await
                .expect(
                    "Lagrange bootstrap must auto-augment a legacy signer for test convenience",
                );

            let last: Vec<Certificate> =
                certificate_store.get_latest_certificates(10).await.unwrap();
            assert_eq!(1, last.len());
        }
    }

    #[cfg(feature = "future_snark")]
    mod sign {
        use std::fs;

        use mithril_common::crypto_helper::{
            GenesisSchnorrVerifier, GenesisSigner, sha256_digest, signed_message_from_digest,
        };

        use crate::tools::GenesisSignedPayload;

        use super::*;

        fn write_signing_key(target_dir: &Path, era: SupportedEra) -> PathBuf {
            let (signing_key_path, _) =
                GenesisTools::create_and_save_genesis_keypair(target_dir, era)
                    .expect("keypair generation should not fail");
            signing_key_path
        }

        #[tokio::test]
        async fn lagrange_writes_dual_envelope_that_verifies_both_signatures() {
            let temp = get_temp_dir("lagrange_sign_writes_envelope");
            let preimage_path = temp.join("preimage.bin");
            let signed_path = temp.join("signed.bin");
            let signing_key_path = write_signing_key(&temp, SupportedEra::Lagrange);

            let preimage = vec![0x42u8; 190];
            fs::write(&preimage_path, &preimage).unwrap();

            GenesisTools::sign_genesis_certificate(
                &preimage_path,
                &signed_path,
                &signing_key_path,
                SupportedEra::Lagrange,
            )
            .await
            .expect("Lagrange sign should not fail");

            let envelope_bytes = fs::read(&signed_path).unwrap();
            let envelope = GenesisSignedPayload::try_from_bytes(&envelope_bytes)
                .expect("envelope must round-trip");

            let signer = GenesisSigner::read_from_file(&signing_key_path).unwrap();
            let digest = sha256_digest(&preimage);
            signer
                .ed25519
                .create_verifier()
                .verify(
                    signed_message_from_digest(&digest).as_bytes(),
                    &envelope.ed25519,
                )
                .expect("Ed25519 signature must verify");
            GenesisSchnorrVerifier::from_verification_key(
                signer.schnorr.as_ref().unwrap().verification_key(),
            )
            .verify(&digest, &envelope.schnorr)
            .expect("Schnorr signature must verify");
        }

        #[tokio::test]
        async fn lagrange_rejects_legacy_signing_key() {
            let temp = get_temp_dir("lagrange_sign_rejects_legacy_key");
            let preimage_path = temp.join("preimage.bin");
            let signed_path = temp.join("signed.bin");
            let signing_key_path = write_signing_key(&temp, SupportedEra::Pythagoras);
            fs::write(&preimage_path, vec![0xAAu8; 190]).unwrap();

            let error = GenesisTools::sign_genesis_certificate(
                &preimage_path,
                &signed_path,
                &signing_key_path,
                SupportedEra::Lagrange,
            )
            .await
            .unwrap_err();

            assert!(matches!(
                error.downcast_ref::<mithril_common::crypto_helper::GenesisBundleError>(),
                Some(mithril_common::crypto_helper::GenesisBundleError::LegacySigningKey)
            ));
        }

        #[tokio::test]
        async fn pythagoras_accepts_dual_signing_bundle() {
            let temp = get_temp_dir("pythagoras_sign_accepts_dual_bundle");
            let payload_path = temp.join("payload.txt");
            let signed_path = temp.join("signed.bin");
            let signing_key_path = write_signing_key(&temp, SupportedEra::Lagrange);

            let payload = b"protocol-message-hash";
            fs::write(&payload_path, payload).unwrap();

            GenesisTools::sign_genesis_certificate(
                &payload_path,
                &signed_path,
                &signing_key_path,
                SupportedEra::Pythagoras,
            )
            .await
            .expect("Pythagoras sign with a dual bundle should succeed");

            let signature_bytes = fs::read(&signed_path).unwrap();
            assert_eq!(signature_bytes.len(), 64);

            let signer = GenesisSigner::read_from_file(&signing_key_path).unwrap();
            let signature = GenesisEd25519Signature::from_bytes(&signature_bytes).unwrap();
            signer
                .ed25519
                .create_verifier()
                .verify(payload, &signature)
                .expect("Ed25519 signature must verify");
        }
    }
}
