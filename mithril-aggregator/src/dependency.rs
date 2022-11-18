use mithril_common::certificate_chain::CertificateVerifier;
use mithril_common::chain_observer::ChainObserver;
use mithril_common::crypto_helper::ProtocolGenesisVerifier;
use mithril_common::digesters::{ImmutableDigester, ImmutableFileObserver};
use mithril_common::entities::{
    Certificate, Epoch, ProtocolParameters, Signer, SignerWithStake, StakeDistribution,
};
use mithril_common::store::{StakeStore, StakeStorer};
use mithril_common::BeaconProvider;

use std::collections::HashMap;
use std::ops::Range;
use std::sync::Arc;
use tokio::sync::RwLock;

use crate::configuration::*;
use crate::multi_signer::MultiSigner;
use crate::snapshot_stores::SnapshotStore;
use crate::snapshot_uploaders::SnapshotUploader;
use crate::{
    CertificatePendingStore, CertificateStore, ProtocolParametersStore, ProtocolParametersStorer,
    SingleSignatureStore, Snapshotter, VerificationKeyStore, VerificationKeyStorer,
};

/// MultiSignerWrapper wraps a MultiSigner
pub type MultiSignerWrapper = Arc<RwLock<dyn MultiSigner>>;

/// DependencyManager handles the dependencies
pub struct DependencyManager {
    /// Configuration structure.
    pub config: Configuration,

    /// Snapshot store.
    pub snapshot_store: Arc<dyn SnapshotStore>,

    /// Snapshot uploader service.
    pub snapshot_uploader: Arc<dyn SnapshotUploader>,

    /// Multisigner service.
    pub multi_signer: MultiSignerWrapper,

    /// Certificate pending store.
    pub certificate_pending_store: Arc<CertificatePendingStore>,

    /// Certificate store.
    pub certificate_store: Arc<CertificateStore>,

    /// Verification key store.
    pub verification_key_store: Arc<VerificationKeyStore>,

    /// Stake store.
    pub stake_store: Arc<StakeStore>,

    /// Signer single signature store.
    pub single_signature_store: Arc<SingleSignatureStore>,

    /// Protocol parameter store.
    pub protocol_parameters_store: Arc<ProtocolParametersStore>,

    /// Chain observer service.
    pub chain_observer: Arc<dyn ChainObserver>,

    /// Beacon provider service.
    pub beacon_provider: Arc<dyn BeaconProvider>,

    /// Immutable file observer service.
    pub immutable_file_observer: Arc<dyn ImmutableFileObserver>,

    /// Digester service.
    pub digester: Arc<dyn ImmutableDigester>,

    /// Snapshotter service.
    pub snapshotter: Arc<dyn Snapshotter>,

    /// Certificate verifier service.
    pub certificate_verifier: Arc<dyn CertificateVerifier>,

    /// Genesis signature verifier service.
    pub genesis_verifier: Arc<ProtocolGenesisVerifier>,
}

#[doc(hidden)]
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SimulateFromChainParams {
    /// If set it will search for epoch gap in the given chain and fill the ProtocolParameters and
    /// Stakes & VerificationKeys stores using the values from the closets latest certificate in each
    /// gap.
    FillSignersAndProtocolParamsForGap,

    /// Will set the multi_signer protocol parameters & beacon to the one in contained in the latest certificate.
    SetupMultiSigner,
}

impl SimulateFromChainParams {
    pub fn find_gap(epochs: &[Epoch]) -> Vec<Range<Epoch>> {
        if epochs.is_empty() {
            panic!("The given epochs slice must not be empty");
        }

        let mut result = vec![];
        let mut epochs = epochs.to_vec();
        epochs.sort();

        let mut previous = *epochs.first().unwrap();
        for current in epochs {
            if current - previous > Epoch(1) {
                result.push((previous + 1)..(current - 1));
            }
            previous = current;
        }

        result
    }
}

#[doc(hidden)]
impl DependencyManager {
    /// Get the first two epochs that will be used by a newly started aggregator
    async fn get_genesis_epochs(&self) -> (Epoch, Epoch) {
        let current_epoch = self
            .chain_observer
            .get_current_epoch()
            .await
            .expect("get_current_epoch should not fail")
            .expect("an epoch should've been set to the chain observer");
        let work_epoch = current_epoch
            .offset_to_signer_retrieval_epoch()
            .expect("epoch.offset_by SIGNER_EPOCH_RETRIEVAL_OFFSET should not fail");
        let epoch_to_sign = current_epoch
            .offset_to_next_signer_retrieval_epoch()
            .expect("epoch.offset_by NEXT_SIGNER_EPOCH_RETRIEVAL_OFFSET should not fail");

        (work_epoch, epoch_to_sign)
    }

    /// `TEST METHOD ONLY`
    ///
    /// Fill the stores of a [DependencyManager] in a way to simulate an aggregator genesis state
    /// using the data from a precomputed certificate_chain.
    ///
    /// Arguments:
    pub async fn simulate_genesis_from_chain(
        &self,
        certificate_chain: &[Certificate],
        additional_params: Vec<SimulateFromChainParams>,
    ) {
        if certificate_chain.is_empty() {
            panic!("The given certificate chain must contains at least one certificate");
        }

        let mut certificate_chain = certificate_chain.to_vec();
        certificate_chain.sort_by(|left, right| {
            left.beacon
                .partial_cmp(&right.beacon)
                .expect("The given certificates should all share the same network")
        });
        let last_certificate = certificate_chain.last().unwrap().clone();
        let last_beacon = last_certificate.beacon.clone();
        let last_protocol_parameters = last_certificate.metadata.protocol_parameters.clone();

        let mut parameters_per_epoch: HashMap<Epoch, (Vec<SignerWithStake>, ProtocolParameters)> =
            HashMap::new();
        for certificate in certificate_chain.iter() {
            if parameters_per_epoch.contains_key(&certificate.beacon.epoch) {
                continue;
            }

            parameters_per_epoch.insert(
                certificate.beacon.epoch,
                (
                    certificate.metadata.signers.clone(),
                    certificate.metadata.protocol_parameters.clone(),
                ),
            );
        }

        if additional_params.contains(&SimulateFromChainParams::FillSignersAndProtocolParamsForGap)
        {
            let epochs = parameters_per_epoch.clone().into_keys().collect::<Vec<_>>();

            for gap in SimulateFromChainParams::find_gap(&epochs) {
                let (closest_signers, closest_protocol_params) =
                    parameters_per_epoch.get(&(gap.end + 1)).unwrap().clone();

                for epoch in gap.start.0..=gap.end.0 {
                    parameters_per_epoch.insert(
                        Epoch(epoch),
                        (closest_signers.clone(), closest_protocol_params.clone()),
                    );
                }
            }
        }

        for (epoch, params) in parameters_per_epoch {
            self.fill_verification_key_store(epoch, &params.0).await;
            self.fill_stakes_store(epoch, &params.0).await;
            self.protocol_parameters_store
                .save_protocol_parameters(epoch, params.1)
                .await
                .expect("save_protocol_parameters should not fail");
        }

        for certificate in certificate_chain {
            self.certificate_store
                .save(certificate.to_owned())
                .await
                .expect("certificate_store::save should not fail");
        }

        if additional_params.contains(&SimulateFromChainParams::SetupMultiSigner) {
            let mut multi_signer = self.multi_signer.write().await;

            multi_signer
                .update_current_beacon(last_beacon)
                .await
                .expect("setting the beacon should not fail");
            multi_signer
                .update_protocol_parameters(&last_protocol_parameters.into())
                .await
                .expect("updating protocol parameters should not fail");
        }
    }

    /// `TEST METHOD ONLY`
    ///
    /// Fill the stores of a [DependencyManager] in a way to simulate an aggregator genesis state.
    ///
    /// For the current and the next epoch:
    /// * Fill the [VerificationKeyStore] with the given signers keys.
    /// * Fill the [StakeStore] with the given signers stakes.
    /// * Fill the [ProtocolParametersStore] with the given parameters.
    pub async fn simulate_genesis(
        &self,
        genesis_signers: Vec<SignerWithStake>,
        second_epoch_signers: Vec<SignerWithStake>,
        genesis_protocol_parameters: &ProtocolParameters,
    ) {
        let (work_epoch, epoch_to_sign) = self.get_genesis_epochs().await;
        for (epoch, signers) in [
            (work_epoch, genesis_signers),
            (epoch_to_sign, second_epoch_signers),
        ] {
            self.fill_verification_key_store(epoch, &signers).await;
            self.fill_stakes_store(epoch, &signers).await;
        }

        self.init_protocol_parameter_store(genesis_protocol_parameters)
            .await;
    }

    /// `TEST METHOD ONLY`
    ///
    /// Fill the first two epoch of the [ProtocolParametersStore] with the given value.
    pub async fn init_protocol_parameter_store(&self, protocol_parameters: &ProtocolParameters) {
        let (work_epoch, epoch_to_sign) = self.get_genesis_epochs().await;
        for epoch in [work_epoch, epoch_to_sign] {
            self.protocol_parameters_store
                .save_protocol_parameters(epoch, protocol_parameters.clone())
                .await
                .expect("save_protocol_parameters should not fail");
        }
    }

    async fn fill_verification_key_store(&self, target_epoch: Epoch, signers: &[SignerWithStake]) {
        for signer in signers
            .iter()
            .map(|s| s.to_owned().into())
            .collect::<Vec<Signer>>()
        {
            self.verification_key_store
                .save_verification_key(target_epoch, signer.clone())
                .await
                .expect("save_verification_key should not fail");
        }
    }

    async fn fill_stakes_store(&self, target_epoch: Epoch, signers: &[SignerWithStake]) {
        self.stake_store
            .save_stakes(
                target_epoch,
                signers
                    .iter()
                    .map(|s| s.into())
                    .collect::<StakeDistribution>(),
            )
            .await
            .expect("save_stakes should not fail");
    }
}

#[cfg(test)]
pub mod tests {
    use crate::dependency::SimulateFromChainParams;
    use crate::{
        AggregatorConfig, CertificatePendingStore, CertificateStore, Configuration,
        DependencyManager, DumbSnapshotUploader, DumbSnapshotter, LocalSnapshotStore,
        MultiSignerImpl, ProtocolParametersStore, SingleSignatureStore, SnapshotStoreType,
        SnapshotUploaderType, VerificationKeyStore,
    };
    use mithril_common::certificate_chain::MithrilCertificateVerifier;
    use mithril_common::crypto_helper::{key_encode_hex, ProtocolGenesisSigner};
    use mithril_common::digesters::{DumbImmutableDigester, DumbImmutableFileObserver};
    use mithril_common::entities::Epoch;
    use mithril_common::{
        chain_observer::FakeObserver,
        fake_data,
        store::{adapter::MemoryAdapter, StakeStore},
        BeaconProviderImpl, CardanoNetwork,
    };
    use std::{path::PathBuf, sync::Arc};
    use tokio::sync::RwLock;

    pub async fn initialize_dependencies() -> (DependencyManager, AggregatorConfig) {
        let genesis_signer = ProtocolGenesisSigner::create_deterministic_genesis_signer();
        let genesis_verifier = Arc::new(genesis_signer.create_genesis_verifier());
        let genesis_verification_key = genesis_verifier.to_verification_key();
        let config = Configuration {
            cardano_cli_path: PathBuf::new(),
            cardano_node_socket_path: PathBuf::new(),
            network_magic: Some(42),
            network: "whatever".to_string(),
            protocol_parameters: fake_data::protocol_parameters(),
            url_snapshot_manifest: "https://storage.googleapis.com/cardano-testnet/snapshots.json"
                .to_string(),
            snapshot_store_type: SnapshotStoreType::Local,
            snapshot_uploader_type: SnapshotUploaderType::Local,
            snapshot_bucket_name: None,
            server_ip: "0.0.0.0".to_string(),
            server_port: 8000,
            run_interval: 5000,
            db_directory: PathBuf::new(),
            snapshot_directory: PathBuf::new(),
            data_stores_directory: PathBuf::new(),
            genesis_verification_key: key_encode_hex(genesis_verification_key).unwrap(),
            store_retention_limit: None,
        };
        let snapshot_store = Arc::new(LocalSnapshotStore::new(
            Box::new(MemoryAdapter::new(None).unwrap()),
            20,
        ));
        let snapshot_uploader = Arc::new(DumbSnapshotUploader::new());
        let certificate_pending_store = Arc::new(CertificatePendingStore::new(Box::new(
            MemoryAdapter::new(None).unwrap(),
        )));
        let certificate_store = Arc::new(CertificateStore::new(Box::new(
            MemoryAdapter::new(None).unwrap(),
        )));
        let verification_key_store = Arc::new(VerificationKeyStore::new(
            Box::new(MemoryAdapter::new(None).unwrap()),
            config.store_retention_limit,
        ));
        let stake_store = Arc::new(StakeStore::new(
            Box::new(MemoryAdapter::new(None).unwrap()),
            config.store_retention_limit,
        ));
        let single_signature_store = Arc::new(SingleSignatureStore::new(
            Box::new(MemoryAdapter::new(None).unwrap()),
            config.store_retention_limit,
        ));
        let protocol_parameters_store = Arc::new(ProtocolParametersStore::new(
            Box::new(MemoryAdapter::new(None).unwrap()),
            None,
        ));
        let chain_observer = Arc::new(FakeObserver::default());
        let multi_signer = MultiSignerImpl::new(
            verification_key_store.clone(),
            stake_store.clone(),
            single_signature_store.clone(),
            protocol_parameters_store.clone(),
            chain_observer,
        );
        let multi_signer = Arc::new(RwLock::new(multi_signer));
        let immutable_file_observer = Arc::new(DumbImmutableFileObserver::default());
        let chain_observer = Arc::new(FakeObserver::default());
        let beacon_provider = Arc::new(BeaconProviderImpl::new(
            chain_observer.clone(),
            immutable_file_observer.clone(),
            CardanoNetwork::TestNet(42),
        ));
        let certificate_verifier = Arc::new(MithrilCertificateVerifier::new(slog_scope::logger()));
        let dependency_manager = DependencyManager {
            config,
            snapshot_store,
            snapshot_uploader,
            multi_signer,
            certificate_pending_store,
            certificate_store,
            verification_key_store,
            stake_store,
            single_signature_store,
            protocol_parameters_store,
            chain_observer,
            beacon_provider,
            immutable_file_observer,
            digester: Arc::new(DumbImmutableDigester::new("digest", true)),
            snapshotter: Arc::new(DumbSnapshotter::new()),
            certificate_verifier,
            genesis_verifier,
        };

        let config = AggregatorConfig::new(
            dependency_manager.config.run_interval,
            CardanoNetwork::TestNet(42),
            dependency_manager.config.db_directory.as_path(),
        );

        (dependency_manager, config)
    }

    #[test]
    fn test_find_epoch_gap() {
        let epochs = vec![Epoch(1), Epoch(2), Epoch(6), Epoch(7), Epoch(8), Epoch(20)];

        assert_eq!(
            SimulateFromChainParams::find_gap(&epochs),
            vec![(Epoch(3)..Epoch(5)), (Epoch(9)..Epoch(19))]
        );
    }
}
