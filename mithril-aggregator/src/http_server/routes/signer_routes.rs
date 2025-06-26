use anyhow::Context;
use slog::warn;
use warp::Filter;

use mithril_common::entities::Epoch;
use mithril_common::StdResult;

use crate::dependency_injection::EpochServiceWrapper;
use crate::http_server::routes::middlewares;
use crate::http_server::routes::router::RouterState;

const MITHRIL_SIGNER_VERSION_HEADER: &str = "signer-node-version";

pub fn routes(
    router_state: &RouterState,
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
    register_signer(router_state)
        .or(registered_signers(router_state))
        .or(signers_tickers(router_state))
}

/// POST /register-signer
fn register_signer(
    router_state: &RouterState,
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
    warp::path!("register-signer")
        .and(warp::post())
        .and(middlewares::with_origin_tag(router_state))
        .and(warp::header::optional::<String>(
            MITHRIL_SIGNER_VERSION_HEADER,
        ))
        .and(warp::body::json())
        .and(middlewares::with_logger(router_state))
        .and(middlewares::with_signer_registerer(router_state))
        .and(middlewares::with_event_transmitter(router_state))
        .and(middlewares::with_epoch_service(router_state))
        .and(middlewares::with_metrics_service(router_state))
        .and_then(handlers::register_signer)
}

/// Get /signers/tickers
fn signers_tickers(
    router_state: &RouterState,
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
    warp::path!("signers" / "tickers")
        .and(warp::get())
        .and(middlewares::with_logger(router_state))
        .and(middlewares::extract_config(router_state, |config| {
            config.network.to_string()
        }))
        .and(middlewares::with_signer_getter(router_state))
        .and_then(handlers::signers_tickers)
}

/// Get /signers/registered/:epoch
fn registered_signers(
    router_state: &RouterState,
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
    warp::path!("signers" / "registered" / String)
        .and(warp::get())
        .and(middlewares::with_logger(router_state))
        .and(middlewares::with_epoch_service(router_state))
        .and(middlewares::with_verification_key_store(router_state))
        .and_then(handlers::registered_signers)
}

async fn fetch_epoch_header_value(
    epoch_service: EpochServiceWrapper,
    logger: &slog::Logger,
) -> String {
    match epoch_service.read().await.epoch_of_current_data() {
        Ok(epoch) => format!("{epoch}"),
        Err(e) => {
            warn!(logger, "Could not fetch epoch header value from Epoch service"; "error" => ?e);
            String::new()
        }
    }
}

async fn compute_registration_epoch(
    registered_at: &str,
    epoch_service: EpochServiceWrapper,
) -> StdResult<Epoch> {
    if registered_at.to_lowercase() == "latest" {
        epoch_service.read().await.epoch_of_current_data()
    } else {
        registered_at
            .parse::<u64>()
            .map(Epoch)
            .with_context(|| "Invalid epoch: must be a number or 'latest'")
    }
}

mod handlers {
    use crate::database::repository::SignerGetter;
    use crate::dependency_injection::EpochServiceWrapper;
    use crate::entities::{
        SignerRegistrationsMessage, SignerTickerListItemMessage, SignersTickersMessage,
    };
    use crate::event_store::{EventMessage, TransmitterService};
    use crate::http_server::routes::signer_routes::{
        compute_registration_epoch, fetch_epoch_header_value,
    };
    use crate::{http_server::routes::reply, SignerRegisterer, SignerRegistrationError};
    use crate::{FromRegisterSignerAdapter, MetricsService, VerificationKeyStorer};
    use mithril_common::messages::{RegisterSignerMessage, TryFromMessageAdapter};
    use slog::{debug, warn, Logger};
    use std::convert::Infallible;
    use std::sync::Arc;
    use warp::http::StatusCode;

    /// Register Signer
    #[allow(clippy::too_many_arguments)]
    pub async fn register_signer(
        origin_tag: Option<String>,
        signer_node_version: Option<String>,
        register_signer_message: RegisterSignerMessage,
        logger: Logger,
        signer_registerer: Arc<dyn SignerRegisterer>,
        event_transmitter: Arc<TransmitterService<EventMessage>>,
        epoch_service: EpochServiceWrapper,
        metrics_service: Arc<MetricsService>,
    ) -> Result<impl warp::Reply, Infallible> {
        debug!(logger, ">> register_signer"; "payload" => ?register_signer_message);

        metrics_service
            .get_signer_registration_total_received_since_startup()
            .increment(&[origin_tag.as_deref().unwrap_or_default()]);

        let registration_epoch = register_signer_message.epoch;

        let signer = match FromRegisterSignerAdapter::try_adapt(register_signer_message) {
            Ok(signer) => signer,
            Err(err) => {
                warn!(logger,"register_signer::payload decoding error"; "error" => ?err);
                return Ok(reply::bad_request(
                    "Could not decode signer payload".to_string(),
                    err.to_string(),
                ));
            }
        };

        let epoch_str = fetch_epoch_header_value(epoch_service, &logger).await;

        match signer_registerer.register_signer(registration_epoch, &signer).await {
            Ok(signer_with_stake) => {
                event_transmitter.send(EventMessage::signer_registration(
                    "HTTP::signer_register",
                    &signer_with_stake,
                    signer_node_version,
                    epoch_str.as_str(),
                ));

                Ok(reply::empty(StatusCode::CREATED))
            }
            Err(SignerRegistrationError::ExistingSigner(signer_with_stake)) => {
                debug!(logger, "register_signer::already_registered");

                event_transmitter.send(EventMessage::signer_registration(
                    "HTTP::signer_register",
                    &signer_with_stake,
                    signer_node_version,
                    epoch_str.as_str(),
                ));

                Ok(reply::empty(StatusCode::CREATED))
            }
            Err(SignerRegistrationError::FailedSignerRegistration(err)) => {
                warn!(logger,"register_signer::failed_signer_registration"; "error" => ?err);
                Ok(reply::bad_request(
                    "failed_signer_registration".to_string(),
                    err.to_string(),
                ))
            }
            Err(SignerRegistrationError::RegistrationRoundNotYetOpened) => {
                warn!(logger, "register_signer::registration_round_not_yed_opened");
                Ok(reply::server_error(
                    SignerRegistrationError::RegistrationRoundNotYetOpened,
                ))
            }
            Err(err) => {
                warn!(logger,"register_signer::error"; "error" => ?err);
                Ok(reply::server_error(err))
            }
        }
    }

    /// Get Registered Signers for a given epoch
    pub async fn registered_signers(
        registered_at: String,
        logger: Logger,
        epoch_service: EpochServiceWrapper,
        verification_key_store: Arc<dyn VerificationKeyStorer>,
    ) -> Result<impl warp::Reply, Infallible> {
        let registered_at_epoch =
            match compute_registration_epoch(&registered_at, epoch_service).await {
                Ok(epoch) => epoch,
                Err(err) => {
                    warn!(logger,"registered_signers::invalid_epoch"; "error" => ?err);
                    return Ok(reply::bad_request(
                        "invalid_epoch".to_string(),
                        err.to_string(),
                    ));
                }
            };

        // The given epoch is the epoch at which the signer registered, the store works on
        // the recording epoch so we need to offset.
        match verification_key_store
            .get_signers(registered_at_epoch.offset_to_recording_epoch())
            .await
        {
            Ok(Some(signers)) => {
                let message = SignerRegistrationsMessage::new(registered_at_epoch, signers);
                Ok(reply::json(&message, StatusCode::OK))
            }
            Ok(None) => {
                warn!(logger, "registered_signers::not_found");
                Ok(reply::empty(StatusCode::NOT_FOUND))
            }
            Err(err) => {
                warn!(logger,"registered_signers::error"; "error" => ?err);
                Ok(reply::server_error(err))
            }
        }
    }

    pub async fn signers_tickers(
        logger: Logger,
        network: String,
        signer_getter: Arc<dyn SignerGetter>,
    ) -> Result<impl warp::Reply, Infallible> {
        match signer_getter.get_all().await {
            Ok(signers) => {
                let signers: Vec<_> = signers
                    .into_iter()
                    .map(|s| SignerTickerListItemMessage {
                        party_id: s.signer_id,
                        pool_ticker: s.pool_ticker,
                        has_registered: s.last_registered_at.is_some(),
                    })
                    .collect();
                Ok(reply::json(
                    &SignersTickersMessage { network, signers },
                    StatusCode::OK,
                ))
            }
            Err(err) => {
                warn!(logger,"registered_signers::error"; "error" => ?err);
                Ok(reply::server_error(err))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use anyhow::anyhow;
    use mockall::predicate::eq;
    use serde_json::Value::Null;
    use std::sync::Arc;
    use tokio::sync::RwLock;
    use warp::{
        http::{Method, StatusCode},
        test::request,
    };

    use mithril_api_spec::APISpec;
    use mithril_common::{
        crypto_helper::ProtocolRegistrationError,
        entities::Epoch,
        messages::RegisterSignerMessage,
        test_utils::{fake_data, MithrilFixtureBuilder},
        MITHRIL_ORIGIN_TAG_HEADER,
    };

    use crate::{
        database::{record::SignerRecord, repository::MockSignerGetter},
        http_server::routes::reply::MithrilStatusCode,
        initialize_dependencies,
        services::{FakeEpochService, MockSignerRegisterer},
        store::MockVerificationKeyStorer,
        test_tools::TestLogger,
        SignerRegistrationError,
    };

    use super::*;

    fn setup_router(
        state: RouterState,
    ) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
        let cors = warp::cors()
            .allow_any_origin()
            .allow_headers(vec!["content-type"])
            .allow_methods(vec![Method::GET, Method::POST, Method::OPTIONS]);

        warp::any().and(routes(&state).with(cors))
    }

    #[tokio::test]
    async fn test_register_signer_post_ok() {
        let signer_with_stake = fake_data::signers_with_stakes(1).pop().unwrap();
        let mut mock_signer_registerer = MockSignerRegisterer::new();
        mock_signer_registerer
            .expect_register_signer()
            .return_once(|_, _| Ok(signer_with_stake));
        let mut dependency_manager = initialize_dependencies!().await;
        dependency_manager.signer_registerer = Arc::new(mock_signer_registerer);

        let signer: RegisterSignerMessage = RegisterSignerMessage::dummy();

        let method = Method::POST.as_str();
        let path = "/register-signer";

        let response = request()
            .method(method)
            .path(path)
            .json(&signer)
            .reply(&setup_router(RouterState::new_with_dummy_config(Arc::new(
                dependency_manager,
            ))))
            .await;

        APISpec::verify_conformity(
            APISpec::get_default_spec_file_from(crate::http_server::API_SPEC_LOCATION),
            method,
            path,
            "application/json",
            &signer,
            &response,
            &StatusCode::CREATED,
        )
        .unwrap();
    }

    #[tokio::test]
    async fn test_register_signer_post_increments_signer_registration_total_received_since_startup_metric(
    ) {
        let method = Method::POST.as_str();
        let path = "/register-signer";
        let dependency_manager = Arc::new(initialize_dependencies!().await);
        let initial_counter_value = dependency_manager
            .metrics_service
            .get_signer_registration_total_received_since_startup()
            .get(&["TEST"]);

        request()
            .method(method)
            .path(path)
            .json(&RegisterSignerMessage::dummy())
            .header(MITHRIL_ORIGIN_TAG_HEADER, "TEST")
            .reply(&setup_router(RouterState::new_with_origin_tag_white_list(
                dependency_manager.clone(),
                &["TEST"],
            )))
            .await;

        assert_eq!(
            initial_counter_value + 1,
            dependency_manager
                .metrics_service
                .get_signer_registration_total_received_since_startup()
                .get(&["TEST"])
        );
    }

    #[tokio::test]
    async fn test_register_signer_post_ok_existing() {
        let signer_with_stake = fake_data::signers_with_stakes(1).pop().unwrap();
        let mut mock_signer_registerer = MockSignerRegisterer::new();
        mock_signer_registerer.expect_register_signer().return_once(|_, _| {
            Err(SignerRegistrationError::ExistingSigner(Box::new(
                signer_with_stake,
            )))
        });
        let mut dependency_manager = initialize_dependencies!().await;
        dependency_manager.signer_registerer = Arc::new(mock_signer_registerer);

        let signer: RegisterSignerMessage = RegisterSignerMessage::dummy();

        let method = Method::POST.as_str();
        let path = "/register-signer";

        let response = request()
            .method(method)
            .path(path)
            .json(&signer)
            .reply(&setup_router(RouterState::new_with_dummy_config(Arc::new(
                dependency_manager,
            ))))
            .await;

        APISpec::verify_conformity(
            APISpec::get_default_spec_file_from(crate::http_server::API_SPEC_LOCATION),
            method,
            path,
            "application/json",
            &signer,
            &response,
            &StatusCode::CREATED,
        )
        .unwrap();
    }

    #[tokio::test]
    async fn test_register_signer_post_ko_400() {
        let mut mock_signer_registerer = MockSignerRegisterer::new();
        mock_signer_registerer.expect_register_signer().return_once(|_, _| {
            Err(SignerRegistrationError::FailedSignerRegistration(anyhow!(
                ProtocolRegistrationError::OpCertInvalid
            )))
        });
        let mut dependency_manager = initialize_dependencies!().await;
        dependency_manager.signer_registerer = Arc::new(mock_signer_registerer);

        let signer: RegisterSignerMessage = RegisterSignerMessage::dummy();

        let method = Method::POST.as_str();
        let path = "/register-signer";

        let response = request()
            .method(method)
            .path(path)
            .json(&signer)
            .reply(&setup_router(RouterState::new_with_dummy_config(Arc::new(
                dependency_manager,
            ))))
            .await;

        APISpec::verify_conformity(
            APISpec::get_default_spec_file_from(crate::http_server::API_SPEC_LOCATION),
            method,
            path,
            "application/json",
            &signer,
            &response,
            &StatusCode::BAD_REQUEST,
        )
        .unwrap();
    }

    #[tokio::test]
    async fn test_register_signer_post_ko_500() {
        let mut mock_signer_registerer = MockSignerRegisterer::new();
        mock_signer_registerer.expect_register_signer().return_once(|_, _| {
            Err(SignerRegistrationError::FailedSignerRecorder(
                "an error occurred".to_string(),
            ))
        });
        let mut dependency_manager = initialize_dependencies!().await;
        dependency_manager.signer_registerer = Arc::new(mock_signer_registerer);

        let signer: RegisterSignerMessage = RegisterSignerMessage::dummy();
        let method = Method::POST.as_str();
        let path = "/register-signer";

        let response = request()
            .method(method)
            .path(path)
            .json(&signer)
            .reply(&setup_router(RouterState::new_with_dummy_config(Arc::new(
                dependency_manager,
            ))))
            .await;

        APISpec::verify_conformity(
            APISpec::get_default_spec_file_from(crate::http_server::API_SPEC_LOCATION),
            method,
            path,
            "application/json",
            &signer,
            &response,
            &StatusCode::INTERNAL_SERVER_ERROR,
        )
        .unwrap();
    }

    #[tokio::test]
    async fn test_register_signer_post_ko_550() {
        let mut mock_signer_registerer = MockSignerRegisterer::new();
        mock_signer_registerer
            .expect_register_signer()
            .return_once(|_, _| Err(SignerRegistrationError::RegistrationRoundNotYetOpened));
        let mut dependency_manager = initialize_dependencies!().await;
        dependency_manager.signer_registerer = Arc::new(mock_signer_registerer);

        let signer: RegisterSignerMessage = RegisterSignerMessage::dummy();
        let method = Method::POST.as_str();
        let path = "/register-signer";

        let response = request()
            .method(method)
            .path(path)
            .json(&signer)
            .reply(&setup_router(RouterState::new_with_dummy_config(Arc::new(
                dependency_manager,
            ))))
            .await;

        APISpec::verify_conformity(
            APISpec::get_default_spec_file_from(crate::http_server::API_SPEC_LOCATION),
            method,
            path,
            "application/json",
            &signer,
            &response,
            &MithrilStatusCode::registration_round_not_yet_opened(),
        )
        .unwrap();
    }

    #[tokio::test]
    async fn test_registered_signers_get_offset_given_epoch_to_registration_epoch() {
        let asked_epoch = Epoch(1);
        let expected_retrieval_epoch = asked_epoch.offset_to_recording_epoch();
        let mut mock_verification_key_store = MockVerificationKeyStorer::new();
        mock_verification_key_store
            .expect_get_signers()
            .with(eq(expected_retrieval_epoch))
            .return_once(|_| Ok(Some(fake_data::signers_with_stakes(3))))
            .once();
        let mut dependency_manager = initialize_dependencies!().await;
        dependency_manager.verification_key_store = Arc::new(mock_verification_key_store);

        let method = Method::GET.as_str();
        let base_path = "/signers/registered";

        let response = request()
            .method(method)
            .path(&format!("{base_path}/{asked_epoch}"))
            .reply(&setup_router(RouterState::new_with_dummy_config(Arc::new(
                dependency_manager,
            ))))
            .await;

        assert!(
            response.status().is_success(),
            "expected the response to succeed, was: {response:#?}"
        );
    }

    #[tokio::test]
    async fn test_registered_signers_get_ok() {
        let mut mock_verification_key_store = MockVerificationKeyStorer::new();
        mock_verification_key_store
            .expect_get_signers()
            .return_once(|_| Ok(Some(fake_data::signers_with_stakes(3))))
            .once();
        let mut dependency_manager = initialize_dependencies!().await;
        dependency_manager.verification_key_store = Arc::new(mock_verification_key_store);

        let base_path = "/signers/registered";
        let method = Method::GET.as_str();

        let response = request()
            .method(method)
            .path(&format!("{base_path}/1"))
            .reply(&setup_router(RouterState::new_with_dummy_config(Arc::new(
                dependency_manager,
            ))))
            .await;

        APISpec::verify_conformity(
            APISpec::get_default_spec_file_from(crate::http_server::API_SPEC_LOCATION),
            method,
            &format!("{base_path}/{{epoch}}"),
            "application/json",
            &Null,
            &response,
            &StatusCode::OK,
        )
        .unwrap();
    }

    #[tokio::test]
    async fn test_registered_signers_returns_404_not_found_when_no_registration() {
        let mut mock_verification_key_store = MockVerificationKeyStorer::new();
        mock_verification_key_store
            .expect_get_signers()
            .return_once(|_| Ok(None))
            .once();
        let mut dependency_manager = initialize_dependencies!().await;
        dependency_manager.verification_key_store = Arc::new(mock_verification_key_store);

        let method = Method::GET.as_str();
        let base_path = "/signers/registered";

        let response = request()
            .method(method)
            .path(&format!("{base_path}/3"))
            .reply(&setup_router(RouterState::new_with_dummy_config(Arc::new(
                dependency_manager,
            ))))
            .await;

        APISpec::verify_conformity(
            APISpec::get_default_spec_file_from(crate::http_server::API_SPEC_LOCATION),
            method,
            &format!("{base_path}/{{epoch}}"),
            "application/json",
            &Null,
            &response,
            &StatusCode::NOT_FOUND,
        )
        .unwrap();
    }

    #[tokio::test]
    async fn test_registered_signers_get_ko() {
        let mut mock_verification_key_store = MockVerificationKeyStorer::new();
        mock_verification_key_store
            .expect_get_signers()
            .return_once(|_| Err(anyhow!("invalid query")));
        let mut dependency_manager = initialize_dependencies!().await;
        dependency_manager.verification_key_store = Arc::new(mock_verification_key_store);

        let method = Method::GET.as_str();
        let base_path = "/signers/registered";

        let response = request()
            .method(method)
            .path(&format!("{base_path}/1"))
            .reply(&setup_router(RouterState::new_with_dummy_config(Arc::new(
                dependency_manager,
            ))))
            .await;

        APISpec::verify_conformity(
            APISpec::get_default_spec_file_from(crate::http_server::API_SPEC_LOCATION),
            method,
            &format!("{base_path}/{{epoch}}"),
            "application/json",
            &Null,
            &response,
            &StatusCode::INTERNAL_SERVER_ERROR,
        )
        .unwrap();
    }

    #[tokio::test]
    async fn test_signers_tickers_get_ok() {
        let mut mock_signer_getter = MockSignerGetter::new();
        mock_signer_getter
            .expect_get_all()
            .return_once(|| {
                Ok(vec![
                    SignerRecord {
                        signer_id: "pool_without_ticker".to_string(),
                        pool_ticker: None,
                        created_at: Default::default(),
                        updated_at: Default::default(),
                        last_registered_at: None,
                    },
                    SignerRecord {
                        signer_id: "pool_with_ticker".to_string(),
                        pool_ticker: Some("pool_ticker".to_string()),
                        created_at: Default::default(),
                        updated_at: Default::default(),
                        last_registered_at: None,
                    },
                ])
            })
            .once();
        let mut dependency_manager = initialize_dependencies!().await;
        dependency_manager.signer_getter = Arc::new(mock_signer_getter);

        let method = Method::GET.as_str();
        let path = "/signers/tickers";

        let response = request()
            .method(method)
            .path(path)
            .reply(&setup_router(RouterState::new_with_dummy_config(Arc::new(
                dependency_manager,
            ))))
            .await;

        APISpec::verify_conformity(
            APISpec::get_default_spec_file_from(crate::http_server::API_SPEC_LOCATION),
            method,
            path,
            "application/json",
            &Null,
            &response,
            &StatusCode::OK,
        )
        .unwrap();
    }

    #[tokio::test]
    async fn test_signers_tickers_get_ko() {
        let mut mock_signer_getter = MockSignerGetter::new();
        mock_signer_getter
            .expect_get_all()
            .return_once(|| Err(anyhow!("an error")))
            .once();
        let mut dependency_manager = initialize_dependencies!().await;
        dependency_manager.signer_getter = Arc::new(mock_signer_getter);

        let method = Method::GET.as_str();
        let path = "/signers/tickers";

        let response = request()
            .method(method)
            .path(path)
            .reply(&setup_router(RouterState::new_with_dummy_config(Arc::new(
                dependency_manager,
            ))))
            .await;

        APISpec::verify_conformity(
            APISpec::get_default_spec_file_from(crate::http_server::API_SPEC_LOCATION),
            method,
            path,
            "application/json",
            &Null,
            &response,
            &StatusCode::INTERNAL_SERVER_ERROR,
        )
        .unwrap();
    }

    #[tokio::test]
    async fn test_fetch_epoch_header_value_when_epoch_service_return_epoch() {
        let fixture = MithrilFixtureBuilder::default().build();
        let epoch_service = Arc::new(RwLock::new(FakeEpochService::from_fixture(
            Epoch(84),
            &fixture,
        )));

        let epoch_str = fetch_epoch_header_value(epoch_service, &TestLogger::stdout()).await;

        assert_eq!(epoch_str, "84".to_string());
    }

    #[tokio::test]
    async fn test_fetch_epoch_header_value_when_epoch_service_error_return_empty_string() {
        let epoch_service = Arc::new(RwLock::new(FakeEpochService::without_data()));

        let epoch_str = fetch_epoch_header_value(epoch_service, &TestLogger::stdout()).await;

        assert_eq!(epoch_str, "".to_string());
    }

    mod registered_signers_registration_epoch {
        use crate::services::FakeEpochServiceBuilder;

        use super::*;

        #[tokio::test]
        async fn use_given_epoch_if_valid_number() {
            let epoch_service = Arc::new(RwLock::new(
                FakeEpochServiceBuilder::dummy(Epoch(89)).build(),
            ));
            let epoch = compute_registration_epoch("456", epoch_service).await.unwrap();

            assert_eq!(epoch, Epoch(456));
        }

        #[tokio::test]
        async fn use_epoch_service_current_epoch_if_latest() {
            let epoch_service = Arc::new(RwLock::new(
                FakeEpochServiceBuilder::dummy(Epoch(89)).build(),
            ));
            let epoch = compute_registration_epoch("latest", epoch_service).await.unwrap();

            assert_eq!(epoch, Epoch(89));
        }

        #[tokio::test]
        async fn error_if_given_epoch_is_not_a_number_nor_latest() {
            let epoch_service = Arc::new(RwLock::new(
                FakeEpochServiceBuilder::dummy(Epoch(89)).build(),
            ));

            compute_registration_epoch("invalid", epoch_service)
                .await
                .expect_err("Should fail if epoch is not a number nor 'latest'");
        }
    }
}
