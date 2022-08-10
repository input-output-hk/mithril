use std::sync::Arc;

use mithril_common::{
    chain_observer::ChainObserver, digesters::ImmutableDigester, store::StakeStore, BeaconProvider,
};

use crate::{
    certificate_handler::CertificateHandler, single_signer::SingleSigner, ProtocolInitializerStorer,
};

type StakeStoreService = Arc<StakeStore>;
type CertificateHandlerService = Arc<dyn CertificateHandler>;
type ChainObserverService = Arc<dyn ChainObserver>;
type DigesterService = Arc<dyn ImmutableDigester>;
type SingleSignerService = Arc<dyn SingleSigner>;
type BeaconProviderService = Arc<dyn BeaconProvider>;
type ProtocolInitializerStoreService = Arc<dyn ProtocolInitializerStorer>;

pub trait ServiceBuilder {
    fn build(&self) -> SignerServices;
}

pub struct ProductionServiceBuilder;

impl ServiceBuilder for ProductionServiceBuilder {
    fn build(&self) -> SignerServices {
        todo!()
    }
}
pub struct SignerServices {
    pub beacon_provider: BeaconProviderService,
    pub stake_store: StakeStoreService,
    pub certificate_handler: CertificateHandlerService,
    pub chain_observer: ChainObserverService,
    pub digester: DigesterService,
    pub single_signer: SingleSignerService,
    pub protocol_initializer_store: ProtocolInitializerStoreService,
}
