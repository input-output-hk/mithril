use mithril_common::MITHRIL_ORIGIN_TAG_HEADER;
use slog::{debug, Logger};
use std::convert::Infallible;
use std::sync::Arc;
use warp::Filter;

use mithril_common::api_version::APIVersionProvider;

use crate::database::repository::SignerGetter;
use crate::dependency_injection::EpochServiceWrapper;
use crate::event_store::{EventMessage, TransmitterService};
use crate::http_server::routes::http_server_child_logger;
use crate::http_server::routes::router::{RouterConfig, RouterState};
use crate::services::{CertifierService, MessageService, ProverService, SignedEntityService};
use crate::{
    MetricsService, SignerRegisterer, SingleSignatureAuthenticator, VerificationKeyStorer,
};

/// Extract a value from the configuration
pub fn extract_config<D: Clone + Send>(
    state: &RouterState,
    extract: fn(&RouterConfig) -> D,
) -> impl Filter<Extract = (D,), Error = Infallible> + Clone {
    let config_value = extract(&state.configuration);
    warp::any().map(move || config_value.clone())
}

/// With logger middleware
pub(crate) fn with_logger(
    router_state: &RouterState,
) -> impl Filter<Extract = (Logger,), Error = Infallible> + Clone {
    let logger = http_server_child_logger(&router_state.dependencies.root_logger);
    warp::any().map(move || logger.clone())
}

/// Log to apply each time a route is called
///
/// Example of log produced: `POST /aggregator/register-signatures 202 Accepted`
pub(crate) fn log_route_call(
    router_state: &RouterState,
) -> warp::log::Log<impl Fn(warp::log::Info<'_>) + Clone> {
    let logger = http_server_child_logger(&router_state.dependencies.root_logger);
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

/// With signer registerer middleware
pub fn with_signer_registerer(
    router_state: &RouterState,
) -> impl Filter<Extract = (Arc<dyn SignerRegisterer>,), Error = Infallible> + Clone {
    let signer_register = router_state.dependencies.signer_registerer.clone();
    warp::any().map(move || signer_register.clone())
}

/// With signer getter middleware
pub fn with_signer_getter(
    router_state: &RouterState,
) -> impl Filter<Extract = (Arc<dyn SignerGetter>,), Error = Infallible> + Clone {
    let signer_getter = router_state.dependencies.signer_getter.clone();
    warp::any().map(move || signer_getter.clone())
}

/// With Event transmitter middleware
pub fn with_event_transmitter(
    router_state: &RouterState,
) -> impl Filter<Extract = (Arc<TransmitterService<EventMessage>>,), Error = Infallible> + Clone {
    let event_transmitter = router_state.dependencies.event_transmitter.clone();
    warp::any().map(move || event_transmitter.clone())
}

/// With certifier service middleware
pub fn with_certifier_service(
    router_state: &RouterState,
) -> impl Filter<Extract = (Arc<dyn CertifierService>,), Error = Infallible> + Clone {
    let certifier_service = router_state.dependencies.certifier_service.clone();
    warp::any().map(move || certifier_service.clone())
}

/// With epoch service middleware
pub fn with_epoch_service(
    router_state: &RouterState,
) -> impl Filter<Extract = (EpochServiceWrapper,), Error = Infallible> + Clone {
    let epoch_service = router_state.dependencies.epoch_service.clone();
    warp::any().map(move || epoch_service.clone())
}

/// With signed entity service
pub fn with_signed_entity_service(
    router_state: &RouterState,
) -> impl Filter<Extract = (Arc<dyn SignedEntityService>,), Error = Infallible> + Clone {
    let signed_entity_service = router_state.dependencies.signed_entity_service.clone();
    warp::any().map(move || signed_entity_service.clone())
}

/// With verification key store
pub fn with_verification_key_store(
    router_state: &RouterState,
) -> impl Filter<Extract = (Arc<dyn VerificationKeyStorer>,), Error = Infallible> + Clone {
    let verification_key_store = router_state.dependencies.verification_key_store.clone();
    warp::any().map(move || verification_key_store.clone())
}

/// With API version provider
pub fn with_api_version_provider(
    router_state: &RouterState,
) -> impl Filter<Extract = (Arc<APIVersionProvider>,), Error = Infallible> + Clone {
    let api_version_provider = router_state.dependencies.api_version_provider.clone();
    warp::any().map(move || api_version_provider.clone())
}

/// With Message service
pub fn with_http_message_service(
    router_state: &RouterState,
) -> impl Filter<Extract = (Arc<dyn MessageService>,), Error = Infallible> + Clone {
    let message_service = router_state.dependencies.message_service.clone();
    warp::any().map(move || message_service.clone())
}

/// With Prover service
pub fn with_prover_service(
    router_state: &RouterState,
) -> impl Filter<Extract = (Arc<dyn ProverService>,), Error = Infallible> + Clone {
    let prover_service = router_state.dependencies.prover_service.clone();
    warp::any().map(move || prover_service.clone())
}

/// With Single Signature Authenticator
pub fn with_single_signature_authenticator(
    router_state: &RouterState,
) -> impl Filter<Extract = (Arc<SingleSignatureAuthenticator>,), Error = Infallible> + Clone {
    let single_signer_authenticator = router_state
        .dependencies
        .single_signer_authenticator
        .clone();
    warp::any().map(move || single_signer_authenticator.clone())
}

/// With Metrics service
pub fn with_metrics_service(
    router_state: &RouterState,
) -> impl Filter<Extract = (Arc<MetricsService>,), Error = Infallible> + Clone {
    let metrics_service = router_state.dependencies.metrics_service.clone();
    warp::any().map(move || metrics_service.clone())
}

/// With origin tag of the request
pub fn with_origin_tag(
    router_state: &RouterState,
) -> impl Filter<Extract = (Option<String>,), Error = warp::reject::Rejection> + Clone {
    let white_list = router_state.configuration.origin_tag_white_list.clone();

    warp::header::optional::<String>(MITHRIL_ORIGIN_TAG_HEADER).map(move |name: Option<String>| {
        name.filter(|tag| white_list.contains(tag))
            .or(Some("NA".to_string()))
    })
}

pub mod validators {
    use crate::http_server::validators::ProverTransactionsHashValidator;

    use super::*;

    /// With Prover Transactions Hash Validator
    pub fn with_prover_transactions_hash_validator(
        router_state: &RouterState,
    ) -> impl Filter<Extract = (ProverTransactionsHashValidator,), Error = Infallible> + Clone {
        let max_hashes = router_state
            .configuration
            .cardano_transactions_prover_max_hashes_allowed_by_request;

        warp::any().map(move || ProverTransactionsHashValidator::new(max_hashes))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::Value;
    use std::convert::Infallible;
    use warp::{
        http::{Method, StatusCode},
        test::request,
        Filter,
    };

    use crate::http_server::routes::reply;

    mod origin_tag {
        use std::{collections::HashSet, path::PathBuf};

        use mithril_common::temp_dir;

        use crate::initialize_dependencies;

        use super::*;

        fn route_with_origin_tag(
            router_state: &RouterState,
        ) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
            warp::path!("route")
                .and(warp::get())
                .and(with_origin_tag(router_state))
                .and_then(route_handler)
        }

        async fn router_state_with_origin_whitelist(
            temp_dir: PathBuf,
            tags: &[&str],
        ) -> RouterState {
            let origin_tag_white_list: HashSet<String> =
                tags.iter().map(ToString::to_string).collect();

            RouterState::new(
                Arc::new(initialize_dependencies(temp_dir).await),
                RouterConfig {
                    origin_tag_white_list,
                    ..RouterConfig::dummy()
                },
            )
        }

        async fn route_handler(origin_tag: Option<String>) -> Result<impl warp::Reply, Infallible> {
            Ok(reply::json(&origin_tag, StatusCode::OK))
        }

        #[tokio::test]
        async fn test_origin_tag_with_value_in_white_list_return_the_tag() {
            let router_state =
                router_state_with_origin_whitelist(temp_dir!(), &["CLIENT_TAG"]).await;

            let response = request()
                .header(MITHRIL_ORIGIN_TAG_HEADER, "CLIENT_TAG")
                .method(Method::GET.as_str())
                .path("/route")
                .reply(&route_with_origin_tag(&router_state))
                .await;

            let result: &Value = &serde_json::from_slice(response.body()).unwrap();
            assert_eq!(Some("CLIENT_TAG"), result.as_str());
        }

        #[tokio::test]
        async fn test_origin_tag_with_value_not_in_white_list_return_na() {
            let router_state =
                router_state_with_origin_whitelist(temp_dir!(), &["CLIENT_TAG"]).await;

            let response = request()
                .header(MITHRIL_ORIGIN_TAG_HEADER, "UNKNOWN_TAG")
                .method(Method::GET.as_str())
                .path("/route")
                .reply(&route_with_origin_tag(&router_state))
                .await;

            let result: &Value = &serde_json::from_slice(response.body()).unwrap();
            assert_eq!(Some("NA"), result.as_str());
        }

        #[tokio::test]
        async fn test_without_origin_tag() {
            let router_state =
                router_state_with_origin_whitelist(temp_dir!(), &["CLIENT_TAG"]).await;

            let response = request()
                .method(Method::GET.as_str())
                .path("/route")
                .reply(&route_with_origin_tag(&router_state))
                .await;

            let result: &Value = &serde_json::from_slice(response.body()).unwrap();
            assert_eq!(Some("NA"), result.as_str());
        }
    }
}
