use std::{
    fs::File,
    io::{Write, prelude::*},
    path::{Path, PathBuf},
    sync::Arc,
};

use anyhow::{Context, anyhow};
use mithril_common::{
    CardanoNetwork, StdResult,
    certificate_chain::{CertificateGenesisProducer, CertificateVerifier},
    crypto_helper::{
        ProtocolAggregateVerificationKey, ProtocolGenesisSecretKey, ProtocolGenesisSignature,
        ProtocolGenesisSigner, ProtocolGenesisVerificationKey,
    },
    entities::{Epoch, ProtocolParameters},
    protocol::SignerBuilder,
};

use crate::{
    database::repository::CertificateRepository,
    dependency_injection::GenesisCommandDependenciesContainer,
};

pub struct GenesisTools {
    network: CardanoNetwork,
    epoch: Epoch,
    genesis_avk: ProtocolAggregateVerificationKey,
    genesis_protocol_parameters: ProtocolParameters,
    certificate_verifier: Arc<dyn CertificateVerifier>,
    certificate_repository: Arc<CertificateRepository>,
}

impl GenesisTools {
    pub fn new(
        network: CardanoNetwork,
        epoch: Epoch,
        genesis_avk: ProtocolAggregateVerificationKey,
        genesis_protocol_parameters: ProtocolParameters,
        certificate_verifier: Arc<dyn CertificateVerifier>,
        certificate_repository: Arc<CertificateRepository>,
    ) -> Self {
        Self {
            network,
            epoch,
            genesis_avk,
            genesis_protocol_parameters,
            certificate_verifier,
            certificate_repository,
        }
    }

    pub async fn from_dependencies(
        dependencies: GenesisCommandDependenciesContainer,
    ) -> StdResult<Self> {
        let epoch = dependencies
            .chain_observer
            .get_current_epoch()
            .await?
            .ok_or(anyhow!("Chain observer can not retrieve current epoch"))?;
        let certificate_verifier = dependencies.certificate_verifier.clone();
        let certificate_repository = dependencies.certificate_repository.clone();
        let protocol_parameters_retriever = dependencies.protocol_parameters_retriever.clone();
        let genesis_avk_epoch = epoch.offset_to_next_signer_retrieval_epoch();
        let genesis_protocol_parameters = protocol_parameters_retriever
            .get_protocol_parameters(epoch.offset_to_signer_retrieval_epoch()?)
            .await?
            .ok_or_else(|| anyhow!("Missing protocol parameters for epoch {genesis_avk_epoch}"))?;
        let genesis_signers = dependencies
            .verification_key_store
            .get_signers(genesis_avk_epoch)
            .await?
            .ok_or_else(|| anyhow!("Missing signers for epoch {genesis_avk_epoch}"))?;

        let protocol_multi_signer =
            SignerBuilder::new(&genesis_signers, &genesis_protocol_parameters)
                .with_context(|| "Could not build a multi signer to compute the genesis avk")?
                .build_multi_signer();
        let genesis_avk = protocol_multi_signer.compute_aggregate_verification_key();

        Ok(Self::new(
            dependencies.network,
            epoch,
            genesis_avk,
            genesis_protocol_parameters,
            certificate_verifier,
            certificate_repository,
        ))
    }

    /// Export AVK of the genesis stake distribution to a payload file
    pub fn export_payload_to_sign(&self, target_path: &Path) -> StdResult<()> {
        let mut target_file = File::create(target_path)?;
        let protocol_message = CertificateGenesisProducer::create_genesis_protocol_message(
            &self.genesis_protocol_parameters,
            &self.genesis_avk,
            &self.epoch,
        )?;
        target_file.write_all(protocol_message.compute_hash().as_bytes())?;
        Ok(())
    }

    /// Import signature of the AVK of the genesis stake distribution from a file
    pub async fn import_payload_signature(
        &self,
        signed_payload_path: &Path,
        genesis_verification_key: &ProtocolGenesisVerificationKey,
    ) -> StdResult<()> {
        let mut signed_payload_file = File::open(signed_payload_path).unwrap();
        let mut signed_payload_buffer = Vec::new();
        signed_payload_file.read_to_end(&mut signed_payload_buffer)?;
        let genesis_signature = ProtocolGenesisSignature::from_bytes(&signed_payload_buffer)?;

        self.create_and_save_genesis_certificate(genesis_signature, genesis_verification_key)
            .await
    }

    /// Automatic bootstrap of the genesis certificate (test only)
    pub async fn bootstrap_test_genesis_certificate(
        &self,
        genesis_signer: ProtocolGenesisSigner,
    ) -> StdResult<()> {
        let genesis_verification_key = &genesis_signer.verification_key();
        let genesis_producer = CertificateGenesisProducer::new(Some(Arc::new(genesis_signer)));
        let genesis_protocol_message = CertificateGenesisProducer::create_genesis_protocol_message(
            &self.genesis_protocol_parameters,
            &self.genesis_avk,
            &self.epoch,
        )?;
        let genesis_signature =
            genesis_producer.sign_genesis_protocol_message(genesis_protocol_message)?;
        self.create_and_save_genesis_certificate(genesis_signature, genesis_verification_key)
            .await
    }

    /// Sign the genesis certificate
    pub async fn sign_genesis_certificate(
        to_sign_payload_path: &Path,
        target_signed_payload_path: &Path,
        genesis_secret_key_path: &Path,
    ) -> StdResult<()> {
        let genesis_secret_key =
            ProtocolGenesisSecretKey::read_json_hex_from_file(genesis_secret_key_path)?;
        let genesis_signer = ProtocolGenesisSigner::from_secret_key(genesis_secret_key);

        let mut to_sign_payload_file = File::open(to_sign_payload_path).unwrap();
        let mut to_sign_payload_buffer = Vec::new();
        to_sign_payload_file.read_to_end(&mut to_sign_payload_buffer)?;

        let genesis_signature = genesis_signer.sign(&to_sign_payload_buffer);
        let signed_payload = genesis_signature.to_bytes();

        let mut target_signed_payload_file = File::create(target_signed_payload_path)?;
        target_signed_payload_file.write_all(&signed_payload)?;

        Ok(())
    }

    async fn create_and_save_genesis_certificate(
        &self,
        genesis_signature: ProtocolGenesisSignature,
        genesis_verification_key: &ProtocolGenesisVerificationKey,
    ) -> StdResult<()> {
        let genesis_certificate = CertificateGenesisProducer::create_genesis_certificate(
            self.genesis_protocol_parameters.clone(),
            self.network,
            self.epoch,
            self.genesis_avk.clone(),
            genesis_signature,
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

    /// Export the genesis keypair to a folder and returns the paths to the files (secret key, verification_key)
    pub fn create_and_save_genesis_keypair(keypair_path: &Path) -> StdResult<(PathBuf, PathBuf)> {
        let genesis_signer = ProtocolGenesisSigner::create_non_deterministic_signer();
        let genesis_secret_key_path = keypair_path.join("genesis.sk");
        genesis_signer
            .secret_key()
            .write_json_hex_to_file(&genesis_secret_key_path)?;
        let genesis_verification_key_path = keypair_path.join("genesis.vk");
        genesis_signer
            .verification_key()
            .write_json_hex_to_file(&genesis_verification_key_path)?;

        Ok((genesis_secret_key_path, genesis_verification_key_path))
    }
}

#[cfg(test)]
mod tests {
    use std::{fs::read_to_string, path::PathBuf};

    use mithril_common::{
        certificate_chain::MithrilCertificateVerifier,
        crypto_helper::{
            ProtocolGenesisSecretKey, ProtocolGenesisSigner, ProtocolGenesisVerificationKey,
            ProtocolGenesisVerifier,
        },
        test::{TempDir, builder::MithrilFixtureBuilder, double::fake_data},
    };

    use super::*;
    use crate::database::test_helper::main_db_connection;
    use crate::test::TestLogger;

    fn get_temp_dir(dir_name: &str) -> PathBuf {
        TempDir::create("genesis", dir_name)
    }

    fn create_fake_genesis_avk() -> ProtocolAggregateVerificationKey {
        let fixture = MithrilFixtureBuilder::default().with_signers(5).build();

        fixture.compute_avk()
    }

    fn build_tools(
        genesis_signer: &ProtocolGenesisSigner,
    ) -> (
        GenesisTools,
        Arc<CertificateRepository>,
        Arc<ProtocolGenesisVerifier>,
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
        let genesis_tools = GenesisTools::new(
            fake_data::network(),
            Epoch(10),
            genesis_avk,
            fake_data::protocol_parameters(),
            certificate_verifier.clone(),
            certificate_store.clone(),
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
        let (genesis_secret_key_path, _) = GenesisTools::create_and_save_genesis_keypair(&test_dir)
            .expect("exporting the keypair should not fail");
        let genesis_secret_key = ProtocolGenesisSecretKey::from_json_hex(
            &read_to_string(&genesis_secret_key_path)
                .expect("reading genesis secret key file should not fail"),
        )
        .expect("parsing genesis secret key should not fail");
        let genesis_signer = ProtocolGenesisSigner::from_secret_key(genesis_secret_key);
        let (genesis_tools, certificate_store, genesis_verifier, certificate_verifier) =
            build_tools(&genesis_signer);

        genesis_tools
            .export_payload_to_sign(&payload_path)
            .expect("export_payload_to_sign should not fail");
        GenesisTools::sign_genesis_certificate(
            &payload_path,
            &signed_payload_path,
            &genesis_secret_key_path,
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
        let genesis_signer = ProtocolGenesisSigner::create_deterministic_signer();
        let (genesis_tools, certificate_store, genesis_verifier, certificate_verifier) =
            build_tools(&genesis_signer);

        genesis_tools
            .bootstrap_test_genesis_certificate(genesis_signer)
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
    fn test_create_and_save_genesis_keypair() {
        let temp_dir = get_temp_dir("test_create_and_save_genesis_keypair");
        let (genesis_secret_key_path, genesis_verification_key_path) =
            GenesisTools::create_and_save_genesis_keypair(&temp_dir)
                .expect("Failed to create and save genesis keypair");
        let genesis_secret_key = ProtocolGenesisSecretKey::from_json_hex(
            &read_to_string(&genesis_secret_key_path)
                .expect("Failed to read genesis secret key file"),
        )
        .expect("Failed to parse genesis secret key");
        let genesis_verification_key = ProtocolGenesisVerificationKey::from_json_hex(
            &read_to_string(&genesis_verification_key_path)
                .expect("Failed to read genesis verification key file"),
        )
        .expect("Failed to parse genesis verification key");
        let genesis_verifier =
            ProtocolGenesisSigner::from_secret_key(genesis_secret_key).create_verifier();

        let expected_genesis_verification_key = genesis_verifier.to_verification_key();
        assert_eq!(expected_genesis_verification_key, genesis_verification_key);
    }
}
