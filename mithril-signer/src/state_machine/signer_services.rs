use std::sync::Arc;
use thiserror::Error;

use mithril_common::{
    chain_observer::ChainObserver, digesters::ImmutableDigester, store::StakeStore,
};

use crate::{certificate_handler::CertificateHandler, single_signer::SingleSigner};

type StakeStoreService = Arc<StakeStore>;
type CertificateHandlerService = Arc<Box<dyn CertificateHandler>>;
type ChainObserverService = Arc<Box<dyn ChainObserver>>;
type DigesterService = Arc<Box<dyn ImmutableDigester>>;
type SingleSignerService = Arc<Box<dyn SingleSigner>>;

#[derive(Error, Debug)]
pub enum ServiceError {
    #[error("Service not found: {0}")]
    ServiceNotRegistered(String),
}

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
    pub stake_store: StakeStoreService,
    pub certificate_handler: CertificateHandlerService,
    pub chain_observer: ChainObserverService,
    pub digester: DigesterService,
    pub single_signer: SingleSignerService,
}
