use slog::{Logger, debug};
use std::convert::Infallible;
use std::sync::Arc;
use warp::Filter;

use mithril_common::api_version::APIVersionProvider;
use mithril_common::entities::Epoch;
use mithril_common::{MITHRIL_CLIENT_TYPE_HEADER, MITHRIL_ORIGIN_TAG_HEADER};

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
) -> impl Filter<Extract = (D,), Error = Infallible> + Clone + use<D> {
    let config_value = extract(&state.configuration);
    warp::any().map(move || config_value.clone())
}

/// With logger middleware
pub(crate) fn with_logger(
    router_state: &RouterState,
) -> impl Filter<Extract = (Logger,), Error = Infallible> + Clone + use<> {
    let logger = http_server_child_logger(&router_state.dependencies.root_logger);
    warp::any().map(move || logger.clone())
}

/// Log to apply each time a route is called
///
/// Example of log produced: `POST /aggregator/register-signatures 202 Accepted`
pub(crate) fn log_route_call(
    router_state: &RouterState,
) -> warp::log::Log<impl Fn(warp::log::Info<'_>) + Clone + use<>> {
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
) -> impl Filter<Extract = (Arc<dyn SignerRegisterer>,), Error = Infallible> + Clone + use<> {
    let signer_register = router_state.dependencies.signer_registerer.clone();
    warp::any().map(move || signer_register.clone())
}

/// With signer getter middleware
pub fn with_signer_getter(
    router_state: &RouterState,
) -> impl Filter<Extract = (Arc<dyn SignerGetter>,), Error = Infallible> + Clone + use<> {
    let signer_getter = router_state.dependencies.signer_getter.clone();
    warp::any().map(move || signer_getter.clone())
}

/// With Event transmitter middleware
pub fn with_event_transmitter(
    router_state: &RouterState,
) -> impl Filter<Extract = (Arc<TransmitterService<EventMessage>>,), Error = Infallible> + Clone + use<>
{
    let event_transmitter = router_state.dependencies.event_transmitter.clone();
    warp::any().map(move || event_transmitter.clone())
}

/// With certifier service middleware
pub fn with_certifier_service(
    router_state: &RouterState,
) -> impl Filter<Extract = (Arc<dyn CertifierService>,), Error = Infallible> + Clone + use<> {
    let certifier_service = router_state.dependencies.certifier_service.clone();
    warp::any().map(move || certifier_service.clone())
}

/// With epoch service middleware
pub fn with_epoch_service(
    router_state: &RouterState,
) -> impl Filter<Extract = (EpochServiceWrapper,), Error = Infallible> + Clone + use<> {
    let epoch_service = router_state.dependencies.epoch_service.clone();
    warp::any().map(move || epoch_service.clone())
}

/// With signed entity service
pub fn with_signed_entity_service(
    router_state: &RouterState,
) -> impl Filter<Extract = (Arc<dyn SignedEntityService>,), Error = Infallible> + Clone + use<> {
    let signed_entity_service = router_state.dependencies.signed_entity_service.clone();
    warp::any().map(move || signed_entity_service.clone())
}

/// With verification key store
pub fn with_verification_key_store(
    router_state: &RouterState,
) -> impl Filter<Extract = (Arc<dyn VerificationKeyStorer>,), Error = Infallible> + Clone + use<> {
    let verification_key_store = router_state.dependencies.verification_key_store.clone();
    warp::any().map(move || verification_key_store.clone())
}

/// With API version provider
pub fn with_api_version_provider(
    router_state: &RouterState,
) -> impl Filter<Extract = (Arc<APIVersionProvider>,), Error = Infallible> + Clone + use<> {
    let api_version_provider = router_state.dependencies.api_version_provider.clone();
    warp::any().map(move || api_version_provider.clone())
}

/// With Message service
pub fn with_http_message_service(
    router_state: &RouterState,
) -> impl Filter<Extract = (Arc<dyn MessageService>,), Error = Infallible> + Clone + use<> {
    let message_service = router_state.dependencies.message_service.clone();
    warp::any().map(move || message_service.clone())
}

/// With Prover service
pub fn with_prover_service(
    router_state: &RouterState,
) -> impl Filter<Extract = (Arc<dyn ProverService>,), Error = Infallible> + Clone + use<> {
    let prover_service = router_state.dependencies.prover_service.clone();
    warp::any().map(move || prover_service.clone())
}

/// With Single Signature Authenticator
pub fn with_single_signature_authenticator(
    router_state: &RouterState,
) -> impl Filter<Extract = (Arc<SingleSignatureAuthenticator>,), Error = Infallible> + Clone + use<>
{
    let single_signer_authenticator = router_state.dependencies.single_signer_authenticator.clone();
    warp::any().map(move || single_signer_authenticator.clone())
}

/// With Metrics service
pub fn with_metrics_service(
    router_state: &RouterState,
) -> impl Filter<Extract = (Arc<MetricsService>,), Error = Infallible> + Clone + use<> {
    let metrics_service = router_state.dependencies.metrics_service.clone();
    warp::any().map(move || metrics_service.clone())
}

/// With origin tag of the request
pub fn with_origin_tag(
    router_state: &RouterState,
) -> impl Filter<Extract = (Option<String>,), Error = warp::reject::Rejection> + Clone + use<> {
    let white_list = router_state.configuration.origin_tag_white_list.clone();

    warp::header::optional::<String>(MITHRIL_ORIGIN_TAG_HEADER).map(move |name: Option<String>| {
        name.filter(|tag| white_list.contains(tag)).or(Some("NA".to_string()))
    })
}

pub fn with_client_type()
-> impl Filter<Extract = (Option<String>,), Error = warp::reject::Rejection> + Clone {
    let authorized_client_types = ["CLI", "WASM", "LIBRARY", "NA"];

    warp::header::optional::<String>(MITHRIL_CLIENT_TYPE_HEADER).map(
        move |client_type: Option<String>| {
            client_type
                .filter(|ct| authorized_client_types.contains(&ct.as_str()))
                .or(Some("NA".to_string()))
        },
    )
}

pub fn with_client_metadata(
    router_state: &RouterState,
) -> impl Filter<Extract = (ClientMetadata,), Error = warp::reject::Rejection> + Clone + use<> {
    with_origin_tag(router_state).and(with_client_type()).map(
        |origin_tag: Option<String>, client_type: Option<String>| ClientMetadata {
            origin_tag,
            client_type,
        },
    )
}

pub struct ClientMetadata {
    pub origin_tag: Option<String>,
    pub client_type: Option<String>,
}

pub mod validators {
    use crate::http_server::validators::ProverTransactionsHashValidator;

    use super::*;

    /// With Prover Transactions Hash Validator
    pub fn with_prover_transactions_hash_validator(
        router_state: &RouterState,
    ) -> impl Filter<Extract = (ProverTransactionsHashValidator,), Error = Infallible> + Clone + use<>
    {
        let max_hashes = router_state
            .configuration
            .cardano_transactions_prover_max_hashes_allowed_by_request;

        warp::any().map(move || ProverTransactionsHashValidator::new(max_hashes))
    }
}

pub mod parameters {
    use anyhow::Context;
    use std::ops::Deref;

    use mithril_common::StdResult;

    use super::*;

    /// An epoch parsed from a http request parameter
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub enum ExpandedEpoch {
        /// Epoch was explicitly provided as a number (e.g., "123")
        Parsed(Epoch),
        /// Epoch was provided as "latest" (e.g., "latest")
        Latest(Epoch),
        /// Epoch was provided as "latest-{offset}" (e.g., "latest-100")
        LatestMinusOffset(Epoch, u64),
    }

    impl Deref for ExpandedEpoch {
        type Target = Epoch;

        fn deref(&self) -> &Self::Target {
            match self {
                ExpandedEpoch::Parsed(epoch)
                | ExpandedEpoch::Latest(epoch)
                | ExpandedEpoch::LatestMinusOffset(epoch, _) => epoch,
            }
        }
    }

    impl ExpandedEpoch {
        /// Returns true if this epoch was expanded from 'latest-{offset}' and the offset
        /// is greater than the given value.
        pub fn has_offset_greater_than(&self, value: u64) -> bool {
            match self {
                ExpandedEpoch::Parsed(_) | ExpandedEpoch::Latest(_) => false,
                ExpandedEpoch::LatestMinusOffset(_, offset) => offset > &value,
            }
        }

        /// Apply an additional negative offset to the epoch, but only if it was derived from 'latest'.
        pub fn apply_offset_for_latest(&self, additional_offset: u64) -> Epoch {
            match self {
                ExpandedEpoch::Parsed(epoch) => *epoch,
                ExpandedEpoch::Latest(epoch) | ExpandedEpoch::LatestMinusOffset(epoch, ..) => {
                    Epoch(epoch.saturating_sub(additional_offset))
                }
            }
        }
    }

    /// Parse the string into an [Epoch] if it's a number, or if it's 'latest{-{offset}}' expand
    /// into the actual epoch minus the optional given offset.
    ///
    /// Return the expanded epoch and the eventual offset that was applied
    pub async fn expand_epoch(
        epoch_str: &str,
        epoch_service: EpochServiceWrapper,
    ) -> StdResult<ExpandedEpoch> {
        let epoch_str = epoch_str.to_lowercase();
        if epoch_str == "latest" {
            epoch_service
                .read()
                .await
                .epoch_of_current_data()
                .map(ExpandedEpoch::Latest)
        } else if epoch_str.starts_with("latest-") {
            let (_, offset_str) = epoch_str.split_at("latest-".len());
            let offset = offset_str
                .parse::<u64>()
                .with_context(|| "Invalid epoch offset: must be a number")?;

            epoch_service.read().await.epoch_of_current_data().map(|epoch| {
                ExpandedEpoch::LatestMinusOffset(Epoch(epoch.saturating_sub(offset)), offset)
            })
        } else {
            epoch_str
                .parse::<u64>()
                .map(|epoch| ExpandedEpoch::Parsed(Epoch(epoch)))
                .with_context(|| "Invalid epoch: must be a number or 'latest'")
        }
    }
}

#[cfg(test)]
mod tests {
    use serde_json::Value;
    use std::convert::Infallible;
    use warp::{
        Filter,
        http::{Method, Response, StatusCode},
        hyper::body::Bytes,
        test::request,
    };

    use mithril_common::test::double::Dummy;

    use crate::http_server::routes::reply;
    use crate::initialize_dependencies;

    use super::*;

    async fn route_handler(value: Option<String>) -> Result<impl warp::Reply, Infallible> {
        Ok(reply::json(&value, StatusCode::OK))
    }

    fn get_body(response: Response<Bytes>) -> Option<String> {
        let result: &Value = &serde_json::from_slice(response.body()).unwrap();
        result.as_str().map(|s| s.to_string())
    }

    mod origin_tag {
        use super::*;
        use std::{collections::HashSet, path::PathBuf};

        use mithril_common::temp_dir;

        fn route_with_origin_tag(
            router_state: &RouterState,
        ) -> impl Filter<Extract = (impl warp::Reply + use<>,), Error = warp::Rejection> + Clone + use<>
        {
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

            assert_eq!(Some("CLIENT_TAG".to_string()), get_body(response));
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

            assert_eq!(Some("NA".to_string()), get_body(response));
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

            assert_eq!(Some("NA".to_string()), get_body(response));
        }
    }

    mod client_type {
        use super::*;

        async fn request_with_client_type(client_type_header_value: &str) -> Response<Bytes> {
            request()
                .method(Method::GET.as_str())
                .header(MITHRIL_CLIENT_TYPE_HEADER, client_type_header_value)
                .path("/route")
                .reply(&route_with_client_type())
                .await
        }

        fn route_with_client_type()
        -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
            warp::path!("route")
                .and(warp::get())
                .and(with_client_type())
                .and_then(route_handler)
        }

        #[tokio::test]
        async fn test_with_client_type_use_na_as_default_value_if_header_not_set() {
            let response = request()
                .method(Method::GET.as_str())
                .path("/route")
                .reply(&route_with_client_type())
                .await;

            assert_eq!(Some("NA".to_string()), get_body(response));
        }

        #[tokio::test]
        async fn test_with_client_type_only_authorize_specific_values() {
            let response: Response<Bytes> = request_with_client_type("CLI").await;
            assert_eq!(Some("CLI".to_string()), get_body(response));

            let response: Response<Bytes> = request_with_client_type("WASM").await;
            assert_eq!(Some("WASM".to_string()), get_body(response));

            let response: Response<Bytes> = request_with_client_type("LIBRARY").await;
            assert_eq!(Some("LIBRARY".to_string()), get_body(response));

            let response: Response<Bytes> = request_with_client_type("NA").await;
            assert_eq!(Some("NA".to_string()), get_body(response));

            let response: Response<Bytes> = request_with_client_type("UNKNOWN").await;
            assert_eq!(Some("NA".to_string()), get_body(response));
        }
    }

    mod expand_epoch {
        use tokio::sync::RwLock;

        use crate::services::FakeEpochServiceBuilder;

        use super::{parameters::ExpandedEpoch, *};

        fn fake_epoch_service(returned_epoch: Epoch) -> EpochServiceWrapper {
            Arc::new(RwLock::new(
                FakeEpochServiceBuilder::dummy(returned_epoch).build(),
            ))
        }

        #[tokio::test]
        async fn use_given_epoch_if_valid_number() {
            let epoch_service = fake_epoch_service(Epoch(300));
            let expanded_epoch = parameters::expand_epoch("456", epoch_service).await.unwrap();

            assert_eq!(expanded_epoch, ExpandedEpoch::Parsed(Epoch(456)));
        }

        #[tokio::test]
        async fn use_epoch_service_current_epoch_if_latest() {
            let epoch_service = fake_epoch_service(Epoch(89));
            let expanded_epoch = parameters::expand_epoch("latest", epoch_service).await.unwrap();

            assert_eq!(expanded_epoch, ExpandedEpoch::Latest(Epoch(89)));
        }

        #[tokio::test]
        async fn use_offset_epoch_service_current_epoch_if_latest_minus_a_number() {
            let epoch_service = fake_epoch_service(Epoch(89));
            let expanded_epoch = parameters::expand_epoch("latest-13", epoch_service.clone())
                .await
                .unwrap();
            assert_eq!(
                expanded_epoch,
                ExpandedEpoch::LatestMinusOffset(Epoch(76), 13)
            );

            let expanded_epoch = parameters::expand_epoch("latest-0", epoch_service).await.unwrap();
            assert_eq!(
                expanded_epoch,
                ExpandedEpoch::LatestMinusOffset(Epoch(89), 0)
            );
        }

        #[tokio::test]
        async fn error_if_given_epoch_is_not_a_number_nor_latest_nor_latest_minus_a_number() {
            let epoch_service = fake_epoch_service(Epoch(78));
            for invalid_epoch in [
                "invalid",
                "latest-",
                "latest+",
                "latest+0",
                "latest+293",
                "latest-4.5",
                "latest+2.9",
                "latest-invalid",
            ] {
                parameters::expand_epoch(invalid_epoch, epoch_service.clone())
                    .await
                    .expect_err(
                        "Should fail if epoch is not a number nor 'latest' nor 'latest-{offset}'",
                    );
            }
        }

        #[tokio::test]
        async fn dont_overflow_if_epoch_minus_offset_is_negative() {
            let epoch_service = fake_epoch_service(Epoch(89));
            let expanded_epoch =
                parameters::expand_epoch(&format!("latest-{}", u64::MAX), epoch_service)
                    .await
                    .unwrap();

            assert_eq!(
                expanded_epoch,
                ExpandedEpoch::LatestMinusOffset(Epoch(0), u64::MAX)
            );
        }

        #[test]
        fn test_expanded_epoch_is_parsed_offset_greater_than() {
            assert!(!ExpandedEpoch::Parsed(Epoch(90)).has_offset_greater_than(15));
            assert!(!ExpandedEpoch::Latest(Epoch(90)).has_offset_greater_than(15));
            assert!(!ExpandedEpoch::LatestMinusOffset(Epoch(90), 10).has_offset_greater_than(10));
            assert!(ExpandedEpoch::LatestMinusOffset(Epoch(90), 11).has_offset_greater_than(10));
        }

        #[test]
        fn test_expanded_epoch_apply_additional_offset() {
            assert_eq!(
                ExpandedEpoch::Parsed(Epoch(90)).apply_offset_for_latest(15),
                Epoch(90)
            );
            assert_eq!(
                ExpandedEpoch::Latest(Epoch(90)).apply_offset_for_latest(15),
                Epoch(75)
            );
            assert_eq!(
                ExpandedEpoch::LatestMinusOffset(Epoch(90), 13).apply_offset_for_latest(15),
                Epoch(75)
            );
            assert_eq!(
                ExpandedEpoch::Latest(Epoch(90)).apply_offset_for_latest(u64::MAX),
                Epoch(0)
            );
        }
    }
}
