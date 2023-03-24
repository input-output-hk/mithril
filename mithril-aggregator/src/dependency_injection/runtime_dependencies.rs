use std::sync::Arc;

use sqlite::Connection;
use tokio::sync::{Mutex, RwLock};

use crate::Configuration;

/// Service dependencies required by the main Runtime.
pub struct RuntimeDependencies {
    /// Configuration structure.
    pub config: Configuration,

    /// SQLite database connection
    /// This is not a real service but is is needed to instanciate all store
    /// services. Shall be private dependency.
    pub sqlite_connection: Arc<Mutex<Connection>>,

    /// Stake Store used by the StakeDistributionService
    /// It shall be a private dependency.
    pub stake_store: Arc<crate::database::provider::StakePoolStore>,

    /// Snapshot store.
    pub snapshot_store: Arc<dyn crate::SnapshotStore>,

    /// Snapshot uploader service.
    pub snapshot_uploader: Arc<dyn crate::SnapshotUploader>,

    /// Multisigner service.
    pub multi_signer: Arc<RwLock<dyn crate::MultiSigner>>,

    /// Certificate pending store.
    pub certificate_pending_store: Arc<crate::CertificatePendingStore>,

    /// Certificate store.
    pub certificate_store: Arc<crate::CertificateStore>,

    /// Verification key store.
    pub verification_key_store: Arc<crate::VerificationKeyStore>,

    /// Signer single signature store.
    pub single_signature_store: Arc<crate::SingleSignatureStore>,

    /// Protocol parameter store.
    pub protocol_parameters_store: Arc<crate::ProtocolParametersStore>,

    /// Chain observer service.
    pub chain_observer: Arc<dyn mithril_common::chain_observer::ChainObserver>,

    /// Beacon provider service.
    pub beacon_provider: Arc<dyn mithril_common::BeaconProvider>,

    /// Immutable file observer service.
    pub immutable_file_observer: Arc<dyn mithril_common::digesters::ImmutableFileObserver>,

    /// Digester service.
    pub digester: Arc<dyn mithril_common::digesters::ImmutableDigester>,

    /// Snapshotter service.
    pub snapshotter: Arc<dyn crate::Snapshotter>,

    /// Certificate verifier service.
    pub certificate_verifier: Arc<dyn mithril_common::certificate_chain::CertificateVerifier>,

    /// Genesis signature verifier service.
    pub genesis_verifier: Arc<mithril_common::crypto_helper::ProtocolGenesisVerifier>,

    /// Signer registerer service
    pub signer_registerer: Arc<dyn crate::SignerRegisterer>,

    /// Signer registration round opener service
    pub signer_registration_round_opener: Arc<dyn crate::SignerRegistrationRoundOpener>,

    /// Era checker service
    pub era_checker: Arc<mithril_common::era::EraChecker>,

    /// Era reader service
    pub era_reader: Arc<mithril_common::era::EraReader>,

    /// Event Transmitter Service
    pub event_transmitter:
        Arc<crate::event_store::TransmitterService<crate::event_store::EventMessage>>,

    /// API Version provider
    pub api_version_provider: Arc<mithril_common::api_version::APIVersionProvider>,

    /// Stake Distribution Service
    pub stake_distribution_service:
        Arc<dyn crate::stake_distribution_service::StakeDistributionService>,
}
