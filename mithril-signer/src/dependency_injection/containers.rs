use std::sync::Arc;

use mithril_common::api_version::APIVersionProvider;
use mithril_common::cardano_transactions_preloader::CardanoTransactionsPreloader;
use mithril_common::chain_observer::ChainObserver;
use mithril_common::digesters::ImmutableDigester;
use mithril_common::era::{EraChecker, EraReader};
use mithril_common::signable_builder::SignableBuilderService;
use mithril_common::signed_entity_type_lock::SignedEntityTypeLock;
use mithril_common::TickerService;
use mithril_persistence::store::StakeStore;
use tokio::sync::RwLock;

use crate::services::{
    AggregatorClient, CertifierService, EpochService, SingleSigner, UpkeepService,
};
use crate::store::ProtocolInitializerStorer;
use crate::MetricsService;

type StakeStoreService = Arc<StakeStore>;
type CertificateHandlerService = Arc<dyn AggregatorClient>;
type ChainObserverService = Arc<dyn ChainObserver>;
type DigesterService = Arc<dyn ImmutableDigester>;
type SingleSignerService = Arc<dyn SingleSigner>;
type TimePointProviderService = Arc<dyn TickerService>;
type ProtocolInitializerStoreService = Arc<dyn ProtocolInitializerStorer>;

/// EpochServiceWrapper wraps a [EpochService]
pub type EpochServiceWrapper = Arc<RwLock<dyn EpochService>>;

/// This structure groups all the dependencies required by the state machine.
pub struct SignerDependencyContainer {
    /// Time point provider service
    pub ticker_service: TimePointProviderService,

    /// Stake store service
    pub stake_store: StakeStoreService,

    /// Certificate handler service
    pub certificate_handler: CertificateHandlerService,

    /// Chain Observer service
    pub chain_observer: ChainObserverService,

    /// Digester service
    pub digester: DigesterService,

    /// SingleSigner service
    pub single_signer: SingleSignerService,

    /// ProtocolInitializer store
    pub protocol_initializer_store: ProtocolInitializerStoreService,

    /// Era checker service
    pub era_checker: Arc<EraChecker>,

    /// Era reader service
    pub era_reader: Arc<EraReader>,

    /// API version provider
    pub api_version_provider: Arc<APIVersionProvider>,

    /// Signable Builder Service
    pub signable_builder_service: Arc<dyn SignableBuilderService>,

    /// Metrics service
    pub metrics_service: Arc<MetricsService>,

    /// Signed entity type lock
    pub signed_entity_type_lock: Arc<SignedEntityTypeLock>,

    /// Cardano transactions preloader
    pub cardano_transactions_preloader: Arc<CardanoTransactionsPreloader>,

    /// Upkeep service
    pub upkeep_service: Arc<dyn UpkeepService>,

    /// Epoch service
    pub epoch_service: EpochServiceWrapper,

    /// Certifier service
    pub certifier: Arc<dyn CertifierService>,
}
