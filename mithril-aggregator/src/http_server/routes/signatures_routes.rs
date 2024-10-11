use crate::http_server::routes::middlewares;
use crate::DependencyContainer;
use warp::Filter;

pub fn routes(
    dependency_manager: &DependencyContainer,
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
    register_signatures(dependency_manager)
}

/// POST /register-signatures
fn register_signatures(
    dependency_manager: &DependencyContainer,
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
    warp::path!("register-signatures")
        .and(warp::post())
        .and(warp::body::json())
        .and(middlewares::with_certifier_service(dependency_manager))
        .and(middlewares::with_single_signature_authenticator(
            dependency_manager,
        ))
        .and_then(handlers::register_signatures)
}

mod handlers {
    use slog_scope::{debug, trace, warn};
    use std::convert::Infallible;
    use std::sync::Arc;
    use warp::http::StatusCode;

    use mithril_common::messages::{RegisterSignatureMessage, TryFromMessageAdapter};

    use crate::{
        http_server::routes::reply,
        message_adapters::FromRegisterSingleSignatureAdapter,
        services::{CertifierService, CertifierServiceError, SignatureRegistrationStatus},
        unwrap_to_internal_server_error, SingleSignatureAuthenticator,
    };

    /// Register Signatures
    pub async fn register_signatures(
        message: RegisterSignatureMessage,
        certifier_service: Arc<dyn CertifierService>,
        single_signer_authenticator: Arc<SingleSignatureAuthenticator>,
    ) -> Result<impl warp::Reply, Infallible> {
        debug!("⇄ HTTP SERVER: register_signatures/{:?}", message);
        trace!("⇄ HTTP SERVER: register_signatures"; "complete_message" => #?message );

        let signed_entity_type = message.signed_entity_type.clone();
        let signed_message = message.signed_message.clone();

        let mut signatures = match FromRegisterSingleSignatureAdapter::try_adapt(message) {
            Ok(signature) => signature,
            Err(err) => {
                warn!("register_signatures::payload decoding error"; "error" => ?err);

                return Ok(reply::bad_request(
                    "Could not decode signature payload".to_string(),
                    err.to_string(),
                ));
            }
        };

        if let Some(signed_message) = signed_message {
            unwrap_to_internal_server_error!(
                single_signer_authenticator
                    .authenticate(&mut signatures, &signed_message)
                    .await,
                "single_signer_authenticator::error"
            );

            if !signatures.is_authenticated() {
                debug!("register_signatures::unauthenticated_signature");
                return Ok(reply::bad_request(
                    "Could not authenticate signature".to_string(),
                    "Signature could not be authenticated".to_string(),
                ));
            }
        }

        match certifier_service
            .register_single_signature(&signed_entity_type, &signatures)
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
                    Ok(reply::server_error(err))
                }
            },
            Ok(SignatureRegistrationStatus::Registered) => Ok(reply::empty(StatusCode::CREATED)),
            Ok(SignatureRegistrationStatus::Buffered) => Ok(reply::empty(StatusCode::ACCEPTED)),
        }
    }
}

#[cfg(test)]
mod tests {
    use anyhow::anyhow;
    use std::sync::Arc;
    use warp::http::{Method, StatusCode};
    use warp::test::request;

    use mithril_common::{
        entities::SignedEntityType, messages::RegisterSignatureMessage,
        test_utils::apispec::APISpec,
    };

    use crate::{
        http_server::SERVER_BASE_PATH,
        initialize_dependencies,
        services::{CertifierServiceError, MockCertifierService, SignatureRegistrationStatus},
        SingleSignatureAuthenticator,
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
            .and(routes(&dependency_manager).with(cors))
    }

    #[tokio::test]
    async fn test_register_signatures_try_to_authenticate_signature_with_signed_message() {
        let mut mock_certifier_service = MockCertifierService::new();
        mock_certifier_service
            .expect_register_single_signature()
            .withf(|_, signature| signature.is_authenticated())
            .once()
            .return_once(move |_, _| Ok(SignatureRegistrationStatus::Registered));
        let mut dependency_manager = initialize_dependencies().await;
        dependency_manager.certifier_service = Arc::new(mock_certifier_service);
        dependency_manager.single_signer_authenticator =
            Arc::new(SingleSignatureAuthenticator::new_that_authenticate_everything());

        let message = RegisterSignatureMessage {
            signed_message: Some("message".to_string()),
            ..RegisterSignatureMessage::dummy()
        };

        let method = Method::POST.as_str();
        let path = "/register-signatures";

        request()
            .method(method)
            .path(&format!("/{SERVER_BASE_PATH}{path}"))
            .json(&message)
            .reply(&setup_router(Arc::new(dependency_manager)))
            .await;
    }

    #[tokio::test]
    async fn test_register_signatures_return_400_if_authentication_fail() {
        let mut mock_certifier_service = MockCertifierService::new();
        mock_certifier_service
            .expect_register_single_signature()
            .never();
        let mut dependency_manager = initialize_dependencies().await;
        dependency_manager.certifier_service = Arc::new(mock_certifier_service);
        dependency_manager.single_signer_authenticator =
            Arc::new(SingleSignatureAuthenticator::new_that_reject_everything());

        let message = RegisterSignatureMessage {
            signed_message: Some("message".to_string()),
            ..RegisterSignatureMessage::dummy()
        };

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
            &StatusCode::BAD_REQUEST,
        )
        .unwrap();
    }

    #[tokio::test]
    async fn test_register_signatures_post_ok_201() {
        let mut mock_certifier_service = MockCertifierService::new();
        mock_certifier_service
            .expect_register_single_signature()
            .return_once(move |_, _| Ok(SignatureRegistrationStatus::Registered));
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
            &StatusCode::CREATED,
        )
        .unwrap();
    }

    #[tokio::test]
    async fn test_register_signatures_post_ok_202() {
        let mut mock_certifier_service = MockCertifierService::new();
        mock_certifier_service
            .expect_register_single_signature()
            .return_once(move |_, _| Ok(SignatureRegistrationStatus::Buffered));
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
            &StatusCode::ACCEPTED,
        )
        .unwrap();
    }

    #[tokio::test]
    async fn test_register_signatures_post_ko_400() {
        let mut mock_certifier_service = MockCertifierService::new();
        mock_certifier_service
            .expect_register_single_signature()
            .return_once(move |_, _| Ok(SignatureRegistrationStatus::Registered));
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
            &StatusCode::BAD_REQUEST,
        )
        .unwrap();
    }

    #[tokio::test]
    async fn test_register_signatures_post_ko_404() {
        let signed_entity_type = SignedEntityType::dummy();
        let message = RegisterSignatureMessage::dummy();
        let mut mock_certifier_service = MockCertifierService::new();
        mock_certifier_service
            .expect_register_single_signature()
            .return_once(move |_, _| {
                Err(CertifierServiceError::NotFound(signed_entity_type).into())
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
            &StatusCode::NOT_FOUND,
        )
        .unwrap();
    }

    #[tokio::test]
    async fn test_register_signatures_post_ko_410() {
        let signed_entity_type = SignedEntityType::dummy();
        let message = RegisterSignatureMessage::dummy();
        let mut mock_certifier_service = MockCertifierService::new();
        mock_certifier_service
            .expect_register_single_signature()
            .return_once(move |_, _| {
                Err(CertifierServiceError::AlreadyCertified(signed_entity_type).into())
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
            &StatusCode::GONE,
        )
        .unwrap();
    }

    #[tokio::test]
    async fn test_register_signatures_post_ko_500() {
        let mut mock_certifier_service = MockCertifierService::new();
        mock_certifier_service
            .expect_register_single_signature()
            .return_once(move |_, _| Err(anyhow!("an error occurred")));
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
            &StatusCode::INTERNAL_SERVER_ERROR,
        )
        .unwrap();
    }
}
