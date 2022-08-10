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
        .and(middlewares::with_multi_signer(dependency_manager))
        .and_then(handlers::register_signer)
}

mod handlers {
    use crate::dependency::MultiSignerWrapper;
    use crate::http_server::routes::reply;
    use crate::ProtocolError;
    use mithril_common::crypto_helper::{key_decode_hex, ProtocolPartyId};
    use mithril_common::entities;
    use slog_scope::{debug, warn};
    use std::convert::Infallible;
    use warp::http::StatusCode;

    /// Register Signer
    pub async fn register_signer(
        signer: entities::Signer,
        multi_signer: MultiSignerWrapper,
    ) -> Result<impl warp::Reply, Infallible> {
        debug!("register_signer/{:?}", signer);

        let mut multi_signer = multi_signer.write().await;
        match key_decode_hex(&signer.verification_key) {
            Ok(verification_key) => {
                match multi_signer
                    .register_signer(signer.party_id as ProtocolPartyId, &verification_key)
                    .await
                {
                    Ok(()) => Ok(reply::empty(StatusCode::CREATED)),
                    Err(ProtocolError::ExistingSigner()) => {
                        debug!("register_signer::already_registered");
                        Ok(reply::empty(StatusCode::CREATED))
                    }
                    Err(err) => {
                        warn!("register_signer::error"; "error" => ?err);
                        Ok(reply::internal_server_error(err.to_string()))
                    }
                }
            }
            Err(err) => {
                warn!("register_signer::key_decode_hex::error"; "error" => ?err);
                Ok(reply::empty(StatusCode::BAD_REQUEST))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    const API_SPEC_FILE: &str = "../openapi.yaml";

    use mithril_common::apispec::APISpec;
    use mithril_common::fake_data;
    use tokio::sync::RwLock;
    use warp::http::Method;
    use warp::test::request;

    use super::*;
    use crate::http_server::SERVER_BASE_PATH;
    use crate::multi_signer::MockMultiSigner;
    use crate::{initialize_dependencies, ProtocolError};

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
        let mut mock_multi_signer = MockMultiSigner::new();
        mock_multi_signer
            .expect_register_signer()
            .return_once(|_, _| Ok(()));
        let (mut dependency_manager, _) = initialize_dependencies().await;
        dependency_manager.multi_signer = Arc::new(RwLock::new(mock_multi_signer));

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
        let mut mock_multi_signer = MockMultiSigner::new();
        mock_multi_signer
            .expect_register_signer()
            .return_once(|_, _| Err(ProtocolError::ExistingSigner()));
        let (mut dependency_manager, _) = initialize_dependencies().await;
        dependency_manager.multi_signer = Arc::new(RwLock::new(mock_multi_signer));

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
        let mock_multi_signer = MockMultiSigner::new();
        let (mut dependency_manager, _) = initialize_dependencies().await;
        dependency_manager.multi_signer = Arc::new(RwLock::new(mock_multi_signer));

        let mut signer = fake_data::signers(1)[0].clone();
        signer.verification_key = "invalid-key".to_string();

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
        let mut mock_multi_signer = MockMultiSigner::new();
        mock_multi_signer
            .expect_register_signer()
            .return_once(|_, _| Err(ProtocolError::Core("an error occurred".to_string())));
        let (mut dependency_manager, _) = initialize_dependencies().await;
        dependency_manager.multi_signer = Arc::new(RwLock::new(mock_multi_signer));

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
