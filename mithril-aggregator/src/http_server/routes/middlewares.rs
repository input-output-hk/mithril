use std::convert::Infallible;
use std::sync::Arc;

use warp::Filter;

use mithril_common::entities::SignedEntityConfig;
use mithril_common::{api_version::APIVersionProvider, TickerService};

use crate::database::repository::SignerGetter;
use crate::dependency_injection::EpochServiceWrapper;
use crate::event_store::{EventMessage, TransmitterService};
use crate::services::{CertifierService, MessageService, ProverService, SignedEntityService};
use crate::{
    CertificatePendingStore, Configuration, DependencyContainer, SignerRegisterer,
    VerificationKeyStorer,
};

/// With certificate pending store
pub(crate) fn with_certificate_pending_store(
    dependency_manager: Arc<DependencyContainer>,
) -> impl Filter<Extract = (Arc<CertificatePendingStore>,), Error = Infallible> + Clone {
    warp::any().map(move || dependency_manager.certificate_pending_store.clone())
}

/// With signer registerer middleware
pub fn with_signer_registerer(
    dependency_manager: Arc<DependencyContainer>,
) -> impl Filter<Extract = (Arc<dyn SignerRegisterer>,), Error = Infallible> + Clone {
    warp::any().map(move || dependency_manager.signer_registerer.clone())
}

/// With signer getter middleware
pub fn with_signer_getter(
    dependency_manager: Arc<DependencyContainer>,
) -> impl Filter<Extract = (Arc<dyn SignerGetter>,), Error = Infallible> + Clone {
    warp::any().map(move || dependency_manager.signer_getter.clone())
}

/// With config middleware
pub fn with_config(
    dependency_manager: Arc<DependencyContainer>,
) -> impl Filter<Extract = (Configuration,), Error = Infallible> + Clone {
    warp::any().map(move || dependency_manager.config.clone())
}

/// With signed entity config middleware
pub fn with_signed_entity_config(
    dependency_manager: Arc<DependencyContainer>,
) -> impl Filter<Extract = (SignedEntityConfig,), Error = Infallible> + Clone {
    warp::any().map(move || dependency_manager.signed_entity_config.clone())
}

/// With Event transmitter middleware
pub fn with_event_transmitter(
    dependency_manager: Arc<DependencyContainer>,
) -> impl Filter<Extract = (Arc<TransmitterService<EventMessage>>,), Error = Infallible> + Clone {
    warp::any().map(move || dependency_manager.event_transmitter.clone())
}

/// With certifier service middleware
pub fn with_certifier_service(
    dependency_manager: Arc<DependencyContainer>,
) -> impl Filter<Extract = (Arc<dyn CertifierService>,), Error = Infallible> + Clone {
    warp::any().map(move || dependency_manager.certifier_service.clone())
}

/// With ticker service middleware
pub fn with_ticker_service(
    dependency_manager: Arc<DependencyContainer>,
) -> impl Filter<Extract = (Arc<dyn TickerService>,), Error = Infallible> + Clone {
    warp::any().map(move || dependency_manager.ticker_service.clone())
}

/// With epoch service middleware
pub fn with_epoch_service(
    dependency_manager: Arc<DependencyContainer>,
) -> impl Filter<Extract = (EpochServiceWrapper,), Error = Infallible> + Clone {
    warp::any().map(move || dependency_manager.epoch_service.clone())
}

/// With signed entity service
pub fn with_signed_entity_service(
    dependency_manager: Arc<DependencyContainer>,
) -> impl Filter<Extract = (Arc<dyn SignedEntityService>,), Error = Infallible> + Clone {
    warp::any().map(move || dependency_manager.signed_entity_service.clone())
}

/// With verification key store
pub fn with_verification_key_store(
    dependency_manager: Arc<DependencyContainer>,
) -> impl Filter<Extract = (Arc<dyn VerificationKeyStorer>,), Error = Infallible> + Clone {
    warp::any().map(move || dependency_manager.verification_key_store.clone())
}

/// With API version provider
pub fn with_api_version_provider(
    dependency_manager: Arc<DependencyContainer>,
) -> impl Filter<Extract = (Arc<APIVersionProvider>,), Error = Infallible> + Clone {
    warp::any().map(move || dependency_manager.api_version_provider.clone())
}

/// With Message service
pub fn with_http_message_service(
    dependency_manager: Arc<DependencyContainer>,
) -> impl Filter<Extract = (Arc<dyn MessageService>,), Error = Infallible> + Clone {
    warp::any().map(move || dependency_manager.message_service.clone())
}

/// With Prover service
pub fn with_prover_service(
    dependency_manager: Arc<DependencyContainer>,
) -> impl Filter<Extract = (Arc<dyn ProverService>,), Error = Infallible> + Clone {
    warp::any().map(move || dependency_manager.prover_service.clone())
}

pub mod validators {
    use crate::http_server::validators::ProverTransactionsHashValidator;

    use super::*;

    /// With Prover Transactions Hash Validator
    pub fn with_prover_transations_hash_validator(
        dependency_manager: Arc<DependencyContainer>,
    ) -> impl Filter<Extract = (ProverTransactionsHashValidator,), Error = Infallible> + Clone {
        let max_hashes = dependency_manager
            .config
            .cardano_transactions_prover_max_hashes_allowed_by_request;

        warp::any().map(move || ProverTransactionsHashValidator::new(max_hashes))
    }
}
