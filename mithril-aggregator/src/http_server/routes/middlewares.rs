use slog::{debug, Logger};
use std::collections::BTreeSet;
use std::convert::Infallible;
use std::sync::Arc;
use warp::Filter;

use mithril_common::api_version::APIVersionProvider;
use mithril_common::entities::SignedEntityTypeDiscriminants;

use crate::database::repository::SignerGetter;
use crate::dependency_injection::EpochServiceWrapper;
use crate::event_store::{EventMessage, TransmitterService};
use crate::http_server::routes::http_server_child_logger;
use crate::services::{CertifierService, MessageService, ProverService, SignedEntityService};
use crate::{
    CertificatePendingStore, Configuration, DependencyContainer, MetricsService, SignerRegisterer,
    SingleSignatureAuthenticator, VerificationKeyStorer,
};

/// With logger middleware
pub(crate) fn with_logger(
    dependency_manager: &DependencyContainer,
) -> impl Filter<Extract = (Logger,), Error = Infallible> + Clone {
    let logger = http_server_child_logger(&dependency_manager.root_logger);
    warp::any().map(move || logger.clone())
}

/// Log to apply each time a route is called
///
/// Example of log produced: `POST /aggregator/register-signatures 202 Accepted`
pub(crate) fn log_route_call(
    dependency_manager: &DependencyContainer,
) -> warp::log::Log<impl Fn(warp::log::Info<'_>) + Clone> {
    let logger = http_server_child_logger(&dependency_manager.root_logger);
    warp::log::custom(move |info| {
        debug!(
            logger,
            "{} {} {}",
            info.method(),
            info.path(),
            info.status()
        )
    })
}

/// With certificate pending store
pub(crate) fn with_certificate_pending_store(
    dependency_manager: &DependencyContainer,
) -> impl Filter<Extract = (Arc<CertificatePendingStore>,), Error = Infallible> + Clone {
    let certificate_pending_store = dependency_manager.certificate_pending_store.clone();
    warp::any().map(move || certificate_pending_store.clone())
}

/// With signer registerer middleware
pub fn with_signer_registerer(
    dependency_manager: &DependencyContainer,
) -> impl Filter<Extract = (Arc<dyn SignerRegisterer>,), Error = Infallible> + Clone {
    let signer_register = dependency_manager.signer_registerer.clone();
    warp::any().map(move || signer_register.clone())
}

/// With signer getter middleware
pub fn with_signer_getter(
    dependency_manager: &DependencyContainer,
) -> impl Filter<Extract = (Arc<dyn SignerGetter>,), Error = Infallible> + Clone {
    let signer_getter = dependency_manager.signer_getter.clone();
    warp::any().map(move || signer_getter.clone())
}

/// With config middleware
pub fn with_config(
    dependency_manager: &DependencyContainer,
) -> impl Filter<Extract = (Configuration,), Error = Infallible> + Clone {
    let config = dependency_manager.config.clone();
    warp::any().map(move || config.clone())
}

/// With allowed signed entity discriminants middleware
pub fn with_allowed_signed_entity_type_discriminants(
    dependency_manager: &DependencyContainer,
) -> impl Filter<Extract = (BTreeSet<SignedEntityTypeDiscriminants>,), Error = Infallible> + Clone {
    let allowed_discriminants = dependency_manager.allowed_discriminants.clone();
    warp::any().map(move || allowed_discriminants.clone())
}

/// With Event transmitter middleware
pub fn with_event_transmitter(
    dependency_manager: &DependencyContainer,
) -> impl Filter<Extract = (Arc<TransmitterService<EventMessage>>,), Error = Infallible> + Clone {
    let event_transmitter = dependency_manager.event_transmitter.clone();
    warp::any().map(move || event_transmitter.clone())
}

/// With certifier service middleware
pub fn with_certifier_service(
    dependency_manager: &DependencyContainer,
) -> impl Filter<Extract = (Arc<dyn CertifierService>,), Error = Infallible> + Clone {
    let certifier_service = dependency_manager.certifier_service.clone();
    warp::any().map(move || certifier_service.clone())
}

/// With epoch service middleware
pub fn with_epoch_service(
    dependency_manager: &DependencyContainer,
) -> impl Filter<Extract = (EpochServiceWrapper,), Error = Infallible> + Clone {
    let epoch_service = dependency_manager.epoch_service.clone();
    warp::any().map(move || epoch_service.clone())
}

/// With signed entity service
pub fn with_signed_entity_service(
    dependency_manager: &DependencyContainer,
) -> impl Filter<Extract = (Arc<dyn SignedEntityService>,), Error = Infallible> + Clone {
    let signed_entity_service = dependency_manager.signed_entity_service.clone();
    warp::any().map(move || signed_entity_service.clone())
}

/// With verification key store
pub fn with_verification_key_store(
    dependency_manager: &DependencyContainer,
) -> impl Filter<Extract = (Arc<dyn VerificationKeyStorer>,), Error = Infallible> + Clone {
    let verification_key_store = dependency_manager.verification_key_store.clone();
    warp::any().map(move || verification_key_store.clone())
}

/// With API version provider
pub fn with_api_version_provider(
    dependency_manager: &DependencyContainer,
) -> impl Filter<Extract = (Arc<APIVersionProvider>,), Error = Infallible> + Clone {
    let api_version_provider = dependency_manager.api_version_provider.clone();
    warp::any().map(move || api_version_provider.clone())
}

/// With Message service
pub fn with_http_message_service(
    dependency_manager: &DependencyContainer,
) -> impl Filter<Extract = (Arc<dyn MessageService>,), Error = Infallible> + Clone {
    let message_service = dependency_manager.message_service.clone();
    warp::any().map(move || message_service.clone())
}

/// With Prover service
pub fn with_prover_service(
    dependency_manager: &DependencyContainer,
) -> impl Filter<Extract = (Arc<dyn ProverService>,), Error = Infallible> + Clone {
    let prover_service = dependency_manager.prover_service.clone();
    warp::any().map(move || prover_service.clone())
}

/// With Single Signature Authenticator
pub fn with_single_signature_authenticator(
    dependency_manager: &DependencyContainer,
) -> impl Filter<Extract = (Arc<SingleSignatureAuthenticator>,), Error = Infallible> + Clone {
    let single_signer_authenticator = dependency_manager.single_signer_authenticator.clone();
    warp::any().map(move || single_signer_authenticator.clone())
}

/// With Metrics service
pub fn with_metrics_service(
    dependency_manager: &DependencyContainer,
) -> impl Filter<Extract = (Arc<MetricsService>,), Error = Infallible> + Clone {
    let metrics_service = dependency_manager.metrics_service.clone();
    warp::any().map(move || metrics_service.clone())
}

pub mod validators {
    use crate::http_server::validators::ProverTransactionsHashValidator;

    use super::*;

    /// With Prover Transactions Hash Validator
    pub fn with_prover_transactions_hash_validator(
        dependency_manager: &DependencyContainer,
    ) -> impl Filter<Extract = (ProverTransactionsHashValidator,), Error = Infallible> + Clone {
        let max_hashes = dependency_manager
            .config
            .cardano_transactions_prover_max_hashes_allowed_by_request;

        warp::any().map(move || ProverTransactionsHashValidator::new(max_hashes))
    }
}
