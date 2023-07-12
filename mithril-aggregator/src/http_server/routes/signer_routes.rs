use crate::http_server::routes::middlewares;
use crate::DependencyContainer;
use std::sync::Arc;
use warp::Filter;

const MITHRIL_SIGNER_VERSION_HEADER: &str = "signer-node-version";

pub fn routes(
    dependency_manager: Arc<DependencyContainer>,
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
    register_signer(dependency_manager)
}

/// POST /register-signer
fn register_signer(
    dependency_manager: Arc<DependencyContainer>,
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
    warp::path!("register-signer")
        .and(warp::post())
        .and(warp::header::optional::<String>(
            MITHRIL_SIGNER_VERSION_HEADER,
        ))
        .and(warp::body::json())
        .and(middlewares::with_signer_registerer(
            dependency_manager.clone(),
        ))
        .and(middlewares::with_event_transmitter(
            dependency_manager.clone(),
        ))
        .and(middlewares::with_beacon_provider(dependency_manager))
        .and_then(handlers::register_signer)
}

mod handlers {
    use crate::event_store::{EventMessage, TransmitterService};
    use crate::FromRegisterSignerAdapter;
    use crate::{http_server::routes::reply, SignerRegisterer, SignerRegistrationError};
    use mithril_common::messages::{FromMessageAdapter, RegisterSignerMessage};
    use mithril_common::BeaconProvider;
    use slog_scope::{debug, warn};
    use std::convert::Infallible;
    use std::sync::Arc;
    use warp::http::StatusCode;

    /// Register Signer
    pub async fn register_signer(
        signer_node_version: Option<String>,
        register_signer_message: RegisterSignerMessage,
        signer_registerer: Arc<dyn SignerRegisterer>,
        event_transmitter: Arc<TransmitterService<EventMessage>>,
        beacon_provider: Arc<dyn BeaconProvider>,
    ) -> Result<impl warp::Reply, Infallible> {
        debug!(
            "⇄ HTTP SERVER: register_signer/{:?}",
            register_signer_message
        );

        let registration_epoch = match register_signer_message.epoch {
            Some(epoch) => epoch,
            None => match signer_registerer.get_current_round().await {
                Some(round) => round.epoch,
                None => {
                    let err = SignerRegistrationError::RegistrationRoundNotYetOpened;
                    warn!("register_signer::error"; "error" => ?err);
                    return Ok(reply::internal_server_error(err.to_string()));
                }
            },
        };

        let signer = FromRegisterSignerAdapter::adapt(register_signer_message);
        let mut headers: Vec<(&str, &str)> = match signer_node_version.as_ref() {
            Some(version) => vec![("signer-node-version", version)],
            None => Vec::new(),
        };

        let epoch_str = match beacon_provider.get_current_beacon().await {
            Ok(beacon) => format!("{}", beacon.epoch),
            Err(e) => {
                warn!("Could not read beacon to add in event: {e}");

                String::new()
            }
        };
        if !epoch_str.is_empty() {
            headers.push(("epoch", epoch_str.as_str()));
        }

        match signer_registerer
            .register_signer(registration_epoch, &signer)
            .await
        {
            Ok(signer_with_stake) => {
                let _ = event_transmitter.send_event_message(
                    "HTTP::signer_register",
                    "register_signer",
                    &signer_with_stake,
                    headers,
                );

                Ok(reply::empty(StatusCode::CREATED))
            }
            Err(SignerRegistrationError::ExistingSigner(signer_with_stake)) => {
                debug!("register_signer::already_registered");
                let _ = event_transmitter.send_event_message(
                    "HTTP::signer_register",
                    "register_signer",
                    &signer_with_stake,
                    headers,
                );
                Ok(reply::empty(StatusCode::CREATED))
            }
            Err(SignerRegistrationError::Codec(err)) => {
                warn!("register_signer::failed_signer_decoding"; "error" => ?err);
                Ok(reply::bad_request(
                    "failed_signer_decoding".to_string(),
                    err,
                ))
            }
            Err(SignerRegistrationError::FailedSignerRegistration(err)) => {
                warn!("register_signer::failed_signer_registration"; "error" => ?err);
                Ok(reply::bad_request(
                    "failed_signer_registration".to_string(),
                    err.to_string(),
                ))
            }
            Err(err) => {
                warn!("register_signer::error"; "error" => ?err);
                Ok(reply::internal_server_error(err.to_string()))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::crypto_helper::ProtocolRegistrationError;
    use mithril_common::messages::RegisterSignerMessage;
    use mithril_common::test_utils::apispec::APISpec;
    use mithril_common::test_utils::fake_data;
    use warp::http::Method;
    use warp::test::request;

    use super::*;
    use crate::http_server::SERVER_BASE_PATH;
    use crate::signer_registerer::MockSignerRegisterer;
    use crate::{initialize_dependencies, SignerRegistrationError};

    fn setup_router(
        dependency_manager: Arc<DependencyContainer>,
    ) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
        let cors = warp::cors()
            .allow_any_origin()
            .allow_headers(vec!["content-type"])
            .allow_methods(vec![Method::GET, Method::POST, Method::OPTIONS]);

        warp::any()
            .and(warp::path(SERVER_BASE_PATH))
            .and(routes(dependency_manager).with(cors))
    }

    #[tokio::test]
    async fn test_register_signer_post_ok() {
        let signer_with_stake = fake_data::signers_with_stakes(1).pop().unwrap();
        let mut mock_signer_registerer = MockSignerRegisterer::new();
        mock_signer_registerer
            .expect_register_signer()
            .return_once(|_, _| Ok(signer_with_stake));
        mock_signer_registerer
            .expect_get_current_round()
            .return_once(|| None);
        let mut dependency_manager = initialize_dependencies().await;
        dependency_manager.signer_registerer = Arc::new(mock_signer_registerer);

        let signer: RegisterSignerMessage = RegisterSignerMessage::dummy();

        let method = Method::POST.as_str();
        let path = "/register-signer";

        let response = request()
            .method(method)
            .path(&format!("/{SERVER_BASE_PATH}{path}"))
            .json(&signer)
            .reply(&setup_router(Arc::new(dependency_manager)))
            .await;

        APISpec::verify_conformity(
            APISpec::get_all_spec_files(),
            method,
            path,
            "application/json",
            &signer,
            &response,
        );
    }

    #[tokio::test]
    async fn test_register_signer_post_ok_existing() {
        let signer_with_stake = fake_data::signers_with_stakes(1).pop().unwrap();
        let mut mock_signer_registerer = MockSignerRegisterer::new();
        mock_signer_registerer
            .expect_register_signer()
            .return_once(|_, _| Err(SignerRegistrationError::ExistingSigner(signer_with_stake)));
        mock_signer_registerer
            .expect_get_current_round()
            .return_once(|| None);
        let mut dependency_manager = initialize_dependencies().await;
        dependency_manager.signer_registerer = Arc::new(mock_signer_registerer);

        let signer: RegisterSignerMessage = RegisterSignerMessage::dummy();

        let method = Method::POST.as_str();
        let path = "/register-signer";

        let response = request()
            .method(method)
            .path(&format!("/{SERVER_BASE_PATH}{path}"))
            .json(&signer)
            .reply(&setup_router(Arc::new(dependency_manager)))
            .await;

        APISpec::verify_conformity(
            APISpec::get_all_spec_files(),
            method,
            path,
            "application/json",
            &signer,
            &response,
        );
    }

    #[tokio::test]
    async fn test_register_signer_post_ko_400() {
        let mut mock_signer_registerer = MockSignerRegisterer::new();
        mock_signer_registerer
            .expect_register_signer()
            .return_once(|_, _| {
                Err(SignerRegistrationError::FailedSignerRegistration(
                    ProtocolRegistrationError::OpCertInvalid,
                ))
            });
        mock_signer_registerer
            .expect_get_current_round()
            .return_once(|| None);
        let mut dependency_manager = initialize_dependencies().await;
        dependency_manager.signer_registerer = Arc::new(mock_signer_registerer);

        let signer: RegisterSignerMessage = RegisterSignerMessage::dummy();

        let method = Method::POST.as_str();
        let path = "/register-signer";

        let response = request()
            .method(method)
            .path(&format!("/{SERVER_BASE_PATH}{path}"))
            .json(&signer)
            .reply(&setup_router(Arc::new(dependency_manager)))
            .await;

        APISpec::verify_conformity(
            APISpec::get_all_spec_files(),
            method,
            path,
            "application/json",
            &signer,
            &response,
        );
    }

    #[tokio::test]
    async fn test_register_signer_post_ko_500() {
        let mut mock_signer_registerer = MockSignerRegisterer::new();
        mock_signer_registerer
            .expect_register_signer()
            .return_once(|_, _| {
                Err(SignerRegistrationError::ChainObserver(
                    "an error occurred".to_string(),
                ))
            });
        mock_signer_registerer
            .expect_get_current_round()
            .return_once(|| None);
        let mut dependency_manager = initialize_dependencies().await;
        dependency_manager.signer_registerer = Arc::new(mock_signer_registerer);

        let signer: RegisterSignerMessage = RegisterSignerMessage::dummy();
        let method = Method::POST.as_str();
        let path = "/register-signer";

        let response = request()
            .method(method)
            .path(&format!("/{SERVER_BASE_PATH}{path}"))
            .json(&signer)
            .reply(&setup_router(Arc::new(dependency_manager)))
            .await;

        APISpec::verify_conformity(
            APISpec::get_all_spec_files(),
            method,
            path,
            "application/json",
            &signer,
            &response,
        );
    }
}
