use crate::http_server::routes::middlewares;
use crate::DependencyManager;
use std::sync::Arc;
use warp::Filter;

pub fn routes(
    dependency_manager: Arc<DependencyManager>,
) -> impl Filter<Extract = impl warp::Reply, Error = warp::Rejection> + Clone {
    register_signer(dependency_manager)
}

/// POST /register-signer
fn register_signer(
    dependency_manager: Arc<DependencyManager>,
) -> impl Filter<Extract = impl warp::Reply, Error = warp::Rejection> + Clone {
    warp::path!("register-signer")
        .and(warp::post())
        .and(warp::body::json())
        .and(middlewares::with_signer_registerer(dependency_manager))
        .and_then(handlers::register_signer)
}

mod handlers {
    use crate::{http_server::routes::reply, SignerRegisterer, SignerRegistrationError};
    use mithril_common::entities;
    use slog_scope::{debug, warn};
    use std::convert::Infallible;
    use std::sync::Arc;
    use warp::http::StatusCode;

    /// Register Signer
    pub async fn register_signer(
        signer: entities::Signer,
        signer_registerer: Arc<dyn SignerRegisterer>,
    ) -> Result<impl warp::Reply, Infallible> {
        debug!("⇄ HTTP SERVER: register_signer/{:?}", signer);

        match signer_registerer.register_signer(&signer).await {
            Ok(()) => Ok(reply::empty(StatusCode::CREATED)),
            Err(SignerRegistrationError::ExistingSigner()) => {
                debug!("register_signer::already_registered");
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
    const API_SPEC_FILE: &str = "../openapi.yaml";

    use mithril_common::apispec::APISpec;
    use mithril_common::crypto_helper::ProtocolRegistrationError;
    use mithril_common::fake_data;
    use warp::http::Method;
    use warp::test::request;

    use super::*;
    use crate::http_server::SERVER_BASE_PATH;
    use crate::signer_registerer::MockSignerRegisterer;
    use crate::{initialize_dependencies, SignerRegistrationError};

    fn setup_router(
        dependency_manager: Arc<DependencyManager>,
    ) -> impl Filter<Extract = impl warp::Reply, Error = warp::Rejection> + Clone {
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
        let mut mock_signer_registerer = MockSignerRegisterer::new();
        mock_signer_registerer
            .expect_register_signer()
            .return_once(|_| Ok(()));
        let (mut dependency_manager, _) = initialize_dependencies().await;
        dependency_manager.signer_registerer = Arc::new(mock_signer_registerer);

        let signer = &fake_data::signers(1)[0];

        let method = Method::POST.as_str();
        let path = "/register-signer";

        let response = request()
            .method(method)
            .path(&format!("/{}{}", SERVER_BASE_PATH, path))
            .json(signer)
            .reply(&setup_router(Arc::new(dependency_manager)))
            .await;

        APISpec::from_file(API_SPEC_FILE)
            .method(method)
            .path(path)
            .validate_request(&signer)
            .unwrap()
            .validate_response(&response)
            .expect("OpenAPI error");
    }

    #[tokio::test]
    async fn test_register_signer_post_ok_existing() {
        let mut mock_signer_registerer = MockSignerRegisterer::new();
        mock_signer_registerer
            .expect_register_signer()
            .return_once(|_| Err(SignerRegistrationError::ExistingSigner()));
        let (mut dependency_manager, _) = initialize_dependencies().await;
        dependency_manager.signer_registerer = Arc::new(mock_signer_registerer);

        let signer = &fake_data::signers(1)[0];

        let method = Method::POST.as_str();
        let path = "/register-signer";

        let response = request()
            .method(method)
            .path(&format!("/{}{}", SERVER_BASE_PATH, path))
            .json(signer)
            .reply(&setup_router(Arc::new(dependency_manager)))
            .await;

        APISpec::from_file(API_SPEC_FILE)
            .method(method)
            .path(path)
            .validate_request(&signer)
            .unwrap()
            .validate_response(&response)
            .expect("OpenAPI error");
    }

    #[tokio::test]
    async fn test_register_signer_post_ko_400() {
        let mut mock_signer_registerer = MockSignerRegisterer::new();
        mock_signer_registerer
            .expect_register_signer()
            .return_once(|_| {
                Err(SignerRegistrationError::FailedSignerRegistration(
                    ProtocolRegistrationError::OpCertInvalid,
                ))
            });
        let (mut dependency_manager, _) = initialize_dependencies().await;
        dependency_manager.signer_registerer = Arc::new(mock_signer_registerer);

        let signer = fake_data::signers(1)[0].clone();

        let method = Method::POST.as_str();
        let path = "/register-signer";

        let response = request()
            .method(method)
            .path(&format!("/{}{}", SERVER_BASE_PATH, path))
            .json(&signer)
            .reply(&setup_router(Arc::new(dependency_manager)))
            .await;

        APISpec::from_file(API_SPEC_FILE)
            .method(method)
            .path(path)
            .validate_request(&signer)
            .unwrap()
            .validate_response(&response)
            .expect("OpenAPI error");
    }

    #[tokio::test]
    async fn test_register_signer_post_ko_500() {
        let mut mock_signer_registerer = MockSignerRegisterer::new();
        mock_signer_registerer
            .expect_register_signer()
            .return_once(|_| {
                Err(SignerRegistrationError::ChainObserver(
                    "an error occurred".to_string(),
                ))
            });
        let (mut dependency_manager, _) = initialize_dependencies().await;
        dependency_manager.signer_registerer = Arc::new(mock_signer_registerer);

        let signer = &fake_data::signers(1)[0];

        let method = Method::POST.as_str();
        let path = "/register-signer";

        let response = request()
            .method(method)
            .path(&format!("/{}{}", SERVER_BASE_PATH, path))
            .json(signer)
            .reply(&setup_router(Arc::new(dependency_manager)))
            .await;

        APISpec::from_file(API_SPEC_FILE)
            .method(method)
            .path(path)
            .validate_request(&signer)
            .unwrap()
            .validate_response(&response)
            .expect("OpenAPI error");
    }
}
