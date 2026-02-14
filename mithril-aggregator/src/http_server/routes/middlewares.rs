use serde::de::DeserializeOwned;
use slog::{Logger, debug};
use std::convert::Infallible;
use std::sync::Arc;
use warp::{Filter, Rejection};

use mithril_common::api_version::APIVersionProvider;
use mithril_common::{MITHRIL_CLIENT_TYPE_HEADER, MITHRIL_ORIGIN_TAG_HEADER};

use crate::database::repository::SignerGetter;
use crate::dependency_injection::EpochServiceWrapper;
use crate::event_store::{EventMessage, TransmitterService};
use crate::http_server::routes::http_server_child_logger;
use crate::http_server::routes::router::{RouterConfig, RouterState};
use crate::services::{CertifierService, LegacyProverService, MessageService, SignedEntityService};
use crate::{
    MetricsService, SignerRegisterer, SingleSignatureAuthenticator, VerificationKeyStorer,
};

/// Extract a value from the body with a maximum length limit
pub(crate) fn json_with_max_length<T: DeserializeOwned + Send>(
    max_length: u64,
) -> impl Filter<Extract = (T,), Error = Rejection> + Copy {
    warp::body::content_length_limit(max_length).and(warp::body::json())
}

/// Extract a value from the configuration
pub(crate) fn extract_config<D: Clone + Send>(
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
) -> impl Filter<Extract = (Arc<dyn LegacyProverService>,), Error = Infallible> + Clone + use<> {
    let prover_service = router_state.dependencies.legacy_prover_service.clone();
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
    use crate::http_server::validators::{
        ProverBlockHashValidator, ProverTransactionsHashValidator,
    };

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

    /// With Prover Block Hash Validator
    pub fn with_prover_block_hash_validator(
        router_state: &RouterState,
    ) -> impl Filter<Extract = (ProverBlockHashValidator,), Error = Infallible> + Clone + use<>
    {
        let max_hashes = router_state
            .configuration
            .cardano_transactions_prover_max_hashes_allowed_by_request;

        warp::any().map(move || ProverBlockHashValidator::new(max_hashes))
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

    mod json_with_max_length {
        use warp::test::RequestBuilder;

        use super::*;

        #[derive(Debug, Clone, serde::Serialize, serde::Deserialize, PartialEq)]
        struct TestPayload {
            d: String,
        }

        impl TestPayload {
            fn new(payload: &str) -> Self {
                TestPayload {
                    d: payload.to_string(),
                }
            }

            fn json_bytes_len(&self) -> usize {
                serde_json::to_string(self).unwrap().len()
            }
        }

        fn route_with_json_limit(
            max_length: u64,
        ) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
            warp::path!("json-route")
                .and(warp::post())
                .and(json_with_max_length(max_length))
                .then(|json: TestPayload| async move { reply::json(&json, StatusCode::OK) })
        }

        fn test_request(json: &TestPayload) -> RequestBuilder {
            // Note: `.json` set both `content-type` and `content-length` headers
            request().method(Method::POST.as_str()).json(json).path("/json-route")
        }

        #[tokio::test]
        async fn test_accepts_request_with_content_length_just_below_threshold() {
            let payload = TestPayload::new("test");
            let threshold = payload.json_bytes_len();
            let response = test_request(&payload)
                .reply(&route_with_json_limit(threshold as u64))
                .await;

            assert_eq!(response.status(), StatusCode::OK);
        }

        #[tokio::test]
        async fn test_rejects_request_with_content_length_too_large() {
            let payload = TestPayload::new("test");
            let threshold = payload.json_bytes_len() - 1;
            let response = test_request(&payload)
                .reply(&route_with_json_limit(threshold as u64))
                .await;

            assert_eq!(response.status(), StatusCode::PAYLOAD_TOO_LARGE);
        }

        #[tokio::test]
        async fn test_rejects_request_missing_content_length_header() {
            let payload = TestPayload::new("test");
            let response = test_request(&payload)
                // Unset content-length header
                .header("content-length", "")
                .reply(&route_with_json_limit(100))
                .await;

            assert_eq!(response.status(), StatusCode::LENGTH_REQUIRED);
        }
    }
}
