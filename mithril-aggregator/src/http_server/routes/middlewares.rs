use crate::certifier_service::CertifierService;
use crate::event_store::{EventMessage, TransmitterService};
use crate::signed_entity_service::SignedEntityService;
use crate::ticker_service::TickerService;
use crate::{
    dependency::MultiSignerWrapper, CertificatePendingStore, CertificateStore, Configuration,
    DependencyManager, ProtocolParametersStore, SignerRegisterer,
};
use mithril_common::era::EraChecker;
use mithril_common::BeaconProvider;
use std::convert::Infallible;
use std::sync::Arc;
use warp::Filter;

/// With certificate store middleware
pub fn with_certificate_store(
    dependency_manager: Arc<DependencyManager>,
) -> impl Filter<Extract = (Arc<CertificateStore>,), Error = Infallible> + Clone {
    warp::any().map(move || dependency_manager.certificate_store.clone())
}

/// With certificate pending store
pub(crate) fn with_certificate_pending_store(
    dependency_manager: Arc<DependencyManager>,
) -> impl Filter<Extract = (Arc<CertificatePendingStore>,), Error = Infallible> + Clone {
    warp::any().map(move || dependency_manager.certificate_pending_store.clone())
}

/// With protocol parameters store
pub(crate) fn with_protocol_parameters_store(
    dependency_manager: Arc<DependencyManager>,
) -> impl Filter<Extract = (Arc<ProtocolParametersStore>,), Error = Infallible> + Clone {
    warp::any().map(move || dependency_manager.protocol_parameters_store.clone())
}

/// With multi signer middleware
pub fn with_multi_signer(
    dependency_manager: Arc<DependencyManager>,
) -> impl Filter<Extract = (MultiSignerWrapper,), Error = Infallible> + Clone {
    warp::any().map(move || dependency_manager.multi_signer.clone())
}

/// With signer registerer middleware
pub fn with_signer_registerer(
    dependency_manager: Arc<DependencyManager>,
) -> impl Filter<Extract = (Arc<dyn SignerRegisterer>,), Error = Infallible> + Clone {
    warp::any().map(move || dependency_manager.signer_registerer.clone())
}

/// With config middleware
pub fn with_config(
    dependency_manager: Arc<DependencyManager>,
) -> impl Filter<Extract = (Configuration,), Error = Infallible> + Clone {
    warp::any().map(move || dependency_manager.config.clone())
}

/// With Event transmitter middleware
pub fn with_event_transmitter(
    dependency_manager: Arc<DependencyManager>,
) -> impl Filter<Extract = (Arc<TransmitterService<EventMessage>>,), Error = Infallible> + Clone {
    warp::any().map(move || dependency_manager.event_transmitter.clone())
}

/// With round_opener middleware
pub fn with_beacon_provider(
    dependency_manager: Arc<DependencyManager>,
) -> impl Filter<Extract = (Arc<dyn BeaconProvider>,), Error = Infallible> + Clone {
    warp::any().map(move || dependency_manager.beacon_provider.clone())
}

/// With certifier service middleware
pub fn with_certifier_service(
    dependency_manager: Arc<DependencyManager>,
) -> impl Filter<Extract = (Arc<dyn CertifierService>,), Error = Infallible> + Clone {
    warp::any().map(move || dependency_manager.certifier_service.clone())
}

/// With ticker service middleware
pub fn with_ticker_service(
    dependency_manager: Arc<DependencyManager>,
) -> impl Filter<Extract = (Arc<dyn TickerService>,), Error = Infallible> + Clone {
    warp::any().map(move || dependency_manager.ticker_service.clone())
}

/// With signed entity service middleware
pub fn with_signed_entity_service(
    dependency_manager: Arc<DependencyManager>,
) -> impl Filter<Extract = (Arc<dyn SignedEntityService>,), Error = Infallible> + Clone {
    warp::any().map(move || dependency_manager.signed_entity_service.clone())
}

/// With era checker middleware
pub fn with_era_checker(
    dependency_manager: Arc<DependencyManager>,
) -> impl Filter<Extract = (Arc<EraChecker>,), Error = Infallible> + Clone {
    warp::any().map(move || dependency_manager.era_checker.clone())
}
