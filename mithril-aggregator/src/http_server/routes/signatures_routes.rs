use crate::http_server::routes::middlewares;
use crate::DependencyContainer;
use std::sync::Arc;
use warp::Filter;

pub fn routes(
    dependency_manager: Arc<DependencyContainer>,
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
    register_signatures(dependency_manager)
}

/// POST /register-signatures
fn register_signatures(
    dependency_manager: Arc<DependencyContainer>,
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
    warp::path!("register-signatures")
        .and(warp::post())
        .and(warp::body::json())
        .and(middlewares::with_certifier_service(
            dependency_manager.clone(),
        ))
        .and(middlewares::with_ticker_service(dependency_manager))
        .and_then(handlers::register_signatures)
}

mod handlers {
    use mithril_common::{
        entities::SignedEntityType,
        messages::{FromMessageAdapter, RegisterSignatureMessage},
    };

    use slog_scope::{debug, warn};
    use std::convert::Infallible;
    use std::sync::Arc;
    use warp::http::StatusCode;

    use crate::{
        http_server::routes::reply,
        message_adapters::FromRegisterSingleSignatureAdapter,
        services::{CertifierService, CertifierServiceError, TickerService},
    };

    /// Register Signatures
    pub async fn register_signatures(
        message: RegisterSignatureMessage,
        certifier_service: Arc<dyn CertifierService>,
        ticker_service: Arc<dyn TickerService>,
    ) -> Result<impl warp::Reply, Infallible> {
        debug!("⇄ HTTP SERVER: register_signatures/{:?}", message);

        let signed_entity_type = match message.signed_entity_type.clone() {
            Some(signed_entity_type) => Ok(signed_entity_type),
            None => ticker_service
                .get_current_immutable_beacon()
                .await
                .map(SignedEntityType::CardanoImmutableFilesFull),
        };

        match signed_entity_type {
            Ok(signed_entity_type) => {
                let signature = FromRegisterSingleSignatureAdapter::adapt(message);
                match certifier_service
                    .register_single_signature(&signed_entity_type, &signature)
                    .await
                {
                    Err(err) => match err.downcast_ref::<CertifierServiceError>() {
                        Some(CertifierServiceError::AlreadyCertified(signed_entity_type)) => {
                            debug!("register_signatures::open_message_already_certified"; "signed_entity_type" => ?signed_entity_type);
                            Ok(reply::empty(StatusCode::GONE))
                        }
                        Some(CertifierServiceError::NotFound(signed_entity_type)) => {
                            debug!("register_signatures::not_found"; "signed_entity_type" => ?signed_entity_type);
                            Ok(reply::empty(StatusCode::NOT_FOUND))
                        }
                        Some(_) | None => {
                            warn!("register_signatures::error"; "error" => ?err);
                            Ok(reply::internal_server_error(err.to_string()))
                        }
                    },
                    Ok(()) => Ok(reply::empty(StatusCode::CREATED)),
                }
            }
            Err(err) => {
                warn!("register_signatures::cant_retrieve_signed_entity_type"; "error" => ?err);
                Ok(reply::internal_server_error(err.to_string()))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use warp::http::Method;
    use warp::test::request;

    use mithril_common::{
        entities::SignedEntityType, messages::RegisterSignatureMessage,
        test_utils::apispec::APISpec,
    };

    use crate::{
        http_server::SERVER_BASE_PATH,
        initialize_dependencies,
        services::{CertifierServiceError, MockCertifierService},
        ProtocolError,
    };

    use super::*;

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
    async fn test_register_signatures_post_ok() {
        let mut mock_certifier_service = MockCertifierService::new();
        mock_certifier_service
            .expect_register_single_signature()
            .return_once(move |_, _| Ok(()));
        let mut dependency_manager = initialize_dependencies().await;
        dependency_manager.certifier_service = Arc::new(mock_certifier_service);

        let message = RegisterSignatureMessage::dummy();

        let method = Method::POST.as_str();
        let path = "/register-signatures";

        let response = request()
            .method(method)
            .path(&format!("/{SERVER_BASE_PATH}{path}"))
            .json(&message)
            .reply(&setup_router(Arc::new(dependency_manager)))
            .await;

        APISpec::verify_conformity(
            APISpec::get_all_spec_files(),
            method,
            path,
            "application/json",
            &message,
            &response,
        );
    }

    #[tokio::test]
    async fn test_register_signatures_post_ko_400() {
        let mut mock_certifier_service = MockCertifierService::new();
        mock_certifier_service
            .expect_register_single_signature()
            .return_once(move |_, _| Ok(()));
        let mut dependency_manager = initialize_dependencies().await;
        dependency_manager.certifier_service = Arc::new(mock_certifier_service);

        let mut message = RegisterSignatureMessage::dummy();
        message.signature = "invalid-signature".to_string();

        let method = Method::POST.as_str();
        let path = "/register-signatures";

        let response = request()
            .method(method)
            .path(&format!("/{SERVER_BASE_PATH}{path}"))
            .json(&message)
            .reply(&setup_router(Arc::new(dependency_manager)))
            .await;

        APISpec::verify_conformity(
            APISpec::get_all_spec_files(),
            method,
            path,
            "application/json",
            &message,
            &response,
        );
    }

    #[tokio::test]
    async fn test_register_signatures_post_ko_404() {
        let signed_entity_type = SignedEntityType::dummy();
        let message = RegisterSignatureMessage::dummy();
        let mut mock_certifier_service = MockCertifierService::new();
        mock_certifier_service
            .expect_register_single_signature()
            .return_once(move |_, _| {
                Err(Box::new(CertifierServiceError::NotFound(
                    signed_entity_type,
                )))
            });
        let mut dependency_manager = initialize_dependencies().await;
        dependency_manager.certifier_service = Arc::new(mock_certifier_service);

        let method = Method::POST.as_str();
        let path = "/register-signatures";

        let response = request()
            .method(method)
            .path(&format!("/{SERVER_BASE_PATH}{path}"))
            .json(&message)
            .reply(&setup_router(Arc::new(dependency_manager)))
            .await;

        APISpec::verify_conformity(
            APISpec::get_all_spec_files(),
            method,
            path,
            "application/json",
            &message,
            &response,
        );
    }

    #[tokio::test]
    async fn test_register_signatures_post_ko_410() {
        let signed_entity_type = SignedEntityType::dummy();
        let message = RegisterSignatureMessage::dummy();
        let mut mock_certifier_service = MockCertifierService::new();
        mock_certifier_service
            .expect_register_single_signature()
            .return_once(move |_, _| {
                Err(Box::new(CertifierServiceError::AlreadyCertified(
                    signed_entity_type,
                )))
            });
        let mut dependency_manager = initialize_dependencies().await;
        dependency_manager.certifier_service = Arc::new(mock_certifier_service);

        let method = Method::POST.as_str();
        let path = "/register-signatures";

        let response = request()
            .method(method)
            .path(&format!("/{SERVER_BASE_PATH}{path}"))
            .json(&message)
            .reply(&setup_router(Arc::new(dependency_manager)))
            .await;

        APISpec::verify_conformity(
            APISpec::get_all_spec_files(),
            method,
            path,
            "application/json",
            &message,
            &response,
        );
    }

    #[tokio::test]
    async fn test_register_signatures_post_ko_500() {
        let mut mock_certifier_service = MockCertifierService::new();
        mock_certifier_service
            .expect_register_single_signature()
            .return_once(move |_, _| {
                Err(Box::new(ProtocolError::Core(
                    "an error occurred".to_string(),
                )))
            });
        let mut dependency_manager = initialize_dependencies().await;
        dependency_manager.certifier_service = Arc::new(mock_certifier_service);

        let message = RegisterSignatureMessage::dummy();

        let method = Method::POST.as_str();
        let path = "/register-signatures";

        let response = request()
            .method(method)
            .path(&format!("/{SERVER_BASE_PATH}{path}"))
            .json(&message)
            .reply(&setup_router(Arc::new(dependency_manager)))
            .await;

        APISpec::verify_conformity(
            APISpec::get_all_spec_files(),
            method,
            path,
            "application/json",
            &message,
            &response,
        );
    }
}
