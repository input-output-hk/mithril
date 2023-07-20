use std::{fs::File, io::prelude::*, io::Write, path::Path, sync::Arc};
use tokio::sync::RwLock;

use mithril_common::{
    certificate_chain::{CertificateGenesisProducer, CertificateVerifier},
    crypto_helper::{
        key_decode_hex, ProtocolAggregateVerificationKey, ProtocolGenesisSignature,
        ProtocolGenesisSigner, ProtocolGenesisVerifier,
    },
    entities::{Beacon, ProtocolParameters},
    BeaconProvider, StdResult,
};

use crate::database::provider::CertificateRepository;
use crate::{MultiSigner, ProtocolParametersStorer};

pub struct GenesisToolsDependency {
    /// Multisigner service.
    pub multi_signer: Arc<RwLock<dyn MultiSigner>>,

    /// Beacon provider service.
    pub beacon_provider: Arc<dyn BeaconProvider>,

    /// Genesis signature verifier service.
    pub genesis_verifier: Arc<ProtocolGenesisVerifier>,

    /// Certificate verifier service.
    pub certificate_verifier: Arc<dyn CertificateVerifier>,

    /// Protocol parameter store.
    pub protocol_parameters_store: Arc<dyn ProtocolParametersStorer>,

    /// Certificate store.
    pub certificate_repository: Arc<CertificateRepository>,
}

pub struct GenesisTools {
    protocol_parameters: ProtocolParameters,
    beacon: Beacon,
    genesis_avk: ProtocolAggregateVerificationKey,
    genesis_verifier: Arc<ProtocolGenesisVerifier>,
    certificate_verifier: Arc<dyn CertificateVerifier>,
    certificate_repository: Arc<CertificateRepository>,
}

impl GenesisTools {
    pub fn new(
        protocol_parameters: ProtocolParameters,
        beacon: Beacon,
        genesis_avk: ProtocolAggregateVerificationKey,
        genesis_verifier: Arc<ProtocolGenesisVerifier>,
        certificate_verifier: Arc<dyn CertificateVerifier>,
        certificate_repository: Arc<CertificateRepository>,
    ) -> Self {
        Self {
            protocol_parameters,
            beacon,
            genesis_avk,
            genesis_verifier,
            certificate_verifier,
            certificate_repository,
        }
    }

    pub async fn from_dependencies(dependencies: GenesisToolsDependency) -> StdResult<Self> {
        let mut multi_signer = dependencies.multi_signer.write().await;
        let beacon_provider = dependencies.beacon_provider.clone();
        let beacon = beacon_provider.get_current_beacon().await?;
        multi_signer.update_current_beacon(beacon.clone()).await?;

        let genesis_verifier = dependencies.genesis_verifier.clone();
        let certificate_verifier = dependencies.certificate_verifier.clone();
        let certificate_repository = dependencies.certificate_repository.clone();
        let protocol_parameters_store = dependencies.protocol_parameters_store.clone();

        let protocol_parameters = protocol_parameters_store
            .get_protocol_parameters(beacon.epoch.offset_to_signer_retrieval_epoch()?)
            .await?
            .ok_or_else(|| "Missing protocol parameters".to_string())?;

        let genesis_avk = multi_signer
            .compute_next_stake_distribution_aggregate_verification_key()
            .await?;
        let genesis_avk: ProtocolAggregateVerificationKey = key_decode_hex(&genesis_avk)?;

        Ok(Self::new(
            protocol_parameters,
            beacon,
            genesis_avk,
            genesis_verifier,
            certificate_verifier,
            certificate_repository,
        ))
    }

    /// Export AVK of the genesis stake distribution to a payload file
    pub fn export_payload_to_sign(&self, target_path: &Path) -> StdResult<()> {
        let mut target_file = File::create(target_path)?;
        let protocol_message =
            CertificateGenesisProducer::create_genesis_protocol_message(&self.genesis_avk)?;
        target_file.write_all(protocol_message.compute_hash().as_bytes())?;
        Ok(())
    }

    /// Import signature of the AVK of the genesis stake distribution from a file
    pub async fn import_payload_signature(&self, signed_payload_path: &Path) -> StdResult<()> {
        let mut signed_payload_file = File::open(signed_payload_path).unwrap();
        let mut signed_payload_buffer = Vec::new();
        signed_payload_file.read_to_end(&mut signed_payload_buffer)?;
        let genesis_signature = ProtocolGenesisSignature::from_bytes(&signed_payload_buffer)?;

        self.create_and_save_genesis_certificate(genesis_signature)
            .await
    }

    /// Automatic bootstrap of the genesis certificate (test only)
    pub async fn bootstrap_test_genesis_certificate(
        &self,
        genesis_signer: ProtocolGenesisSigner,
    ) -> StdResult<()> {
        let genesis_producer = CertificateGenesisProducer::new(Some(Arc::new(genesis_signer)));
        let genesis_protocol_message =
            CertificateGenesisProducer::create_genesis_protocol_message(&self.genesis_avk)?;
        let genesis_signature =
            genesis_producer.sign_genesis_protocol_message(genesis_protocol_message)?;
        self.create_and_save_genesis_certificate(genesis_signature)
            .await
    }

    /// Sign the genesis certificate
    pub async fn sign_genesis_certificate(
        &self,
        to_sign_payload_path: &Path,
        target_signed_payload_path: &Path,
        genesis_secret_key_path: &Path,
    ) -> StdResult<()> {
        let mut genesis_secret_key_file = File::open(genesis_secret_key_path).unwrap();
        let mut genesis_secret_key_serialized = String::new();
        genesis_secret_key_file.read_to_string(&mut genesis_secret_key_serialized)?;

        let genesis_secret_key = key_decode_hex(&genesis_secret_key_serialized)?;
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
    ) -> StdResult<()> {
        let genesis_certificate = CertificateGenesisProducer::create_genesis_certificate(
            self.protocol_parameters.clone(),
            self.beacon.clone(),
            self.genesis_avk.clone(),
            genesis_signature,
        )?;
        self.certificate_verifier
            .verify_genesis_certificate(&genesis_certificate, &self.genesis_verifier)
            .await?;
        self.certificate_repository
            .create_certificate(genesis_certificate)
            .await?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::database::provider::apply_all_migrations_to_db;
    use mithril_common::{
        certificate_chain::MithrilCertificateVerifier,
        crypto_helper::{key_encode_hex, ProtocolClerk, ProtocolGenesisSigner},
        test_utils::{fake_data, MithrilFixtureBuilder},
    };
    use sqlite::Connection;
    use std::{fs, path::PathBuf};
    use tokio::sync::Mutex;

    use super::*;

    fn get_temp_dir(dir_name: &str) -> PathBuf {
        let dir = std::env::temp_dir()
            .join("mithril_test")
            .join("genesis")
            .join(dir_name);

        if dir.exists() {
            let _ = fs::remove_dir_all(&dir);
        }

        let _ = fs::create_dir_all(&dir);

        dir
    }

    fn create_fake_genesis_avk() -> ProtocolAggregateVerificationKey {
        let fixture = MithrilFixtureBuilder::default().with_signers(5).build();
        let first_signer = fixture.signers_fixture()[0].clone().protocol_signer;
        let clerk = ProtocolClerk::from_signer(&first_signer);
        clerk.compute_avk()
    }

    fn build_tools(
        genesis_signer: &ProtocolGenesisSigner,
    ) -> (
        GenesisTools,
        Arc<CertificateRepository>,
        Arc<ProtocolGenesisVerifier>,
        Arc<dyn CertificateVerifier>,
    ) {
        let connection = Connection::open(":memory:").unwrap();
        apply_all_migrations_to_db(&connection).unwrap();
        let certificate_store =
            Arc::new(CertificateRepository::new(Arc::new(Mutex::new(connection))));
        let certificate_verifier = Arc::new(MithrilCertificateVerifier::new(slog_scope::logger()));
        let genesis_avk = create_fake_genesis_avk();
        let genesis_verifier = Arc::new(genesis_signer.create_genesis_verifier());
        let genesis_tools = GenesisTools::new(
            fake_data::protocol_parameters(),
            fake_data::beacon(),
            genesis_avk,
            genesis_verifier.clone(),
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
        let genesis_secret_key_path = test_dir.join("genesis.sk");
        let genesis_signer = ProtocolGenesisSigner::create_deterministic_genesis_signer();
        let (genesis_tools, certificate_store, genesis_verifier, certificate_verifier) =
            build_tools(&genesis_signer);
        let mut genesis_secret_key_file = File::create(&genesis_secret_key_path).unwrap();
        genesis_secret_key_file
            .write_all(
                key_encode_hex(genesis_signer.secret_key.as_bytes())
                    .unwrap()
                    .as_bytes(),
            )
            .unwrap();

        genesis_tools
            .export_payload_to_sign(&payload_path)
            .expect("export_payload_to_sign should not fail");
        genesis_tools
            .sign_genesis_certificate(
                &payload_path,
                &signed_payload_path,
                &genesis_secret_key_path,
            )
            .await
            .expect("sign_genesis_certificate should not fail");
        genesis_tools
            .import_payload_signature(&signed_payload_path)
            .await
            .expect("import_payload_signature should not fail");

        let last_certificates = certificate_store.get_latest_certificates(10).await.unwrap();

        assert_eq!(1, last_certificates.len());
        certificate_verifier
            .verify_genesis_certificate(&last_certificates[0], &genesis_verifier)
            .await
            .expect(
                "verify_genesis_certificate should successfully validate the genesis certificate",
            );
    }

    #[tokio::test]
    async fn bootstrap_test_genesis_certificate_works() {
        let genesis_signer = ProtocolGenesisSigner::create_deterministic_genesis_signer();
        let (genesis_tools, certificate_store, genesis_verifier, certificate_verifier) =
            build_tools(&genesis_signer);

        genesis_tools
            .bootstrap_test_genesis_certificate(genesis_signer)
            .await
            .expect("bootstrap test genesis certificate should not fail");

        let last_certificates = certificate_store.get_latest_certificates(10).await.unwrap();

        assert_eq!(1, last_certificates.len());
        certificate_verifier
            .verify_genesis_certificate(&last_certificates[0], &genesis_verifier)
            .await
            .expect(
                "verify_genesis_certificate should successfully validate the genesis certificate",
            );
    }
}
