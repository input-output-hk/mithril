use crate::http_server::routes::middlewares;
use crate::DependencyManager;
use std::sync::Arc;
use warp::Filter;

pub fn routes(
    dependency_manager: Arc<DependencyManager>,
) -> impl Filter<Extract = impl warp::Reply, Error = warp::Rejection> + Clone {
    register_signatures(dependency_manager)
}

/// POST /register-signatures
fn register_signatures(
    dependency_manager: Arc<DependencyManager>,
) -> impl Filter<Extract = impl warp::Reply, Error = warp::Rejection> + Clone {
    warp::path!("register-signatures")
        .and(warp::post())
        .and(warp::body::json())
        .and(middlewares::with_multi_signer(dependency_manager))
        .and_then(handlers::register_signatures)
}

mod handlers {
    use crate::dependency::MultiSignerWrapper;
    use crate::http_server::routes::reply;
    use crate::ProtocolError;
    use mithril_common::crypto_helper::{key_decode_hex, ProtocolLotteryIndex, ProtocolPartyId};
    use mithril_common::entities;
    use slog_scope::debug;
    use std::convert::Infallible;
    use warp::http::StatusCode;

    /// Register Signatures
    pub async fn register_signatures(
        signatures: Vec<entities::SingleSignature>,
        multi_signer: MultiSignerWrapper,
    ) -> Result<impl warp::Reply, Infallible> {
        debug!("register_signatures/{:?}", signatures);

        let mut multi_signer = multi_signer.write().await;
        for signature in &signatures {
            match key_decode_hex(&signature.signature) {
                Ok(single_signature) => {
                    match multi_signer
                        .register_single_signature(
                            signature.party_id.clone() as ProtocolPartyId,
                            &single_signature,
                            signature.index as ProtocolLotteryIndex,
                        )
                        .await
                    {
                        Err(ProtocolError::ExistingSingleSignature(_)) => {
                            return Ok(reply::empty(StatusCode::CONFLICT));
                        }
                        Err(err) => {
                            return Ok(reply::internal_server_error(err.to_string()));
                        }
                        _ => {}
                    }
                }
                Err(_) => {
                    return Ok(reply::empty(StatusCode::BAD_REQUEST));
                }
            }
        }
        Ok(reply::empty(StatusCode::CREATED))
    }
}

#[cfg(test)]
mod tests {
    const API_SPEC_FILE: &str = "../openapi.yaml";

    use crate::http_server::SERVER_BASE_PATH;
    use mithril_common::apispec::APISpec;
    use mithril_common::fake_data;
    use tokio::sync::RwLock;
    use warp::http::Method;
    use warp::test::request;

    use super::*;
    use crate::multi_signer::MockMultiSigner;
    use crate::ProtocolError;

    fn setup_dependency_manager() -> DependencyManager {
        DependencyManager::fake()
    }

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
    async fn test_register_signatures_post_ok() {
        let mut mock_multi_signer = MockMultiSigner::new();
        mock_multi_signer
            .expect_update_current_message()
            .return_once(|_| Ok(()));
        mock_multi_signer
            .expect_register_single_signature()
            .return_once(|_, _, _| Ok(()));
        let mut dependency_manager = setup_dependency_manager();
        dependency_manager.with_multi_signer(Arc::new(RwLock::new(mock_multi_signer)));

        let signatures = &fake_data::single_signatures(1);

        let method = Method::POST.as_str();
        let path = "/register-signatures";

        let response = request()
            .method(method)
            .path(&format!("/{}{}", SERVER_BASE_PATH, path))
            .json(signatures)
            .reply(&setup_router(Arc::new(dependency_manager)))
            .await;

        APISpec::from_file(API_SPEC_FILE)
            .method(method)
            .path(path)
            .validate_request(&signatures)
            .unwrap()
            .validate_response(&response)
            .expect("OpenAPI error");
    }

    #[tokio::test]
    async fn test_register_signatures_post_ko_400() {
        let mut mock_multi_signer = MockMultiSigner::new();
        mock_multi_signer
            .expect_update_current_message()
            .return_once(|_| Ok(()));
        let mut dependency_manager = setup_dependency_manager();
        dependency_manager.with_multi_signer(Arc::new(RwLock::new(mock_multi_signer)));

        let mut signatures = fake_data::single_signatures(1);
        signatures[0].signature = "invalid-signature".to_string();

        let method = Method::POST.as_str();
        let path = "/register-signatures";

        let response = request()
            .method(method)
            .path(&format!("/{}{}", SERVER_BASE_PATH, path))
            .json(&signatures)
            .reply(&setup_router(Arc::new(dependency_manager)))
            .await;

        APISpec::from_file(API_SPEC_FILE)
            .method(method)
            .path(path)
            .validate_request(&signatures)
            .unwrap()
            .validate_response(&response)
            .expect("OpenAPI error");
    }

    #[tokio::test]
    async fn test_register_signatures_post_ko_409() {
        let mut mock_multi_signer = MockMultiSigner::new();
        mock_multi_signer
            .expect_update_current_message()
            .return_once(|_| Ok(()));
        mock_multi_signer
            .expect_register_single_signature()
            .return_once(|_, _, _| Err(ProtocolError::ExistingSingleSignature(1)));
        let mut dependency_manager = setup_dependency_manager();
        dependency_manager.with_multi_signer(Arc::new(RwLock::new(mock_multi_signer)));

        let signatures = &fake_data::single_signatures(1);

        let method = Method::POST.as_str();
        let path = "/register-signatures";

        let response = request()
            .method(method)
            .path(&format!("/{}{}", SERVER_BASE_PATH, path))
            .json(signatures)
            .reply(&setup_router(Arc::new(dependency_manager)))
            .await;

        APISpec::from_file(API_SPEC_FILE)
            .method(method)
            .path(path)
            .validate_request(&signatures)
            .unwrap()
            .validate_response(&response)
            .expect("OpenAPI error");
    }

    #[tokio::test]
    async fn test_register_signatures_post_ko_500() {
        let mut mock_multi_signer = MockMultiSigner::new();
        mock_multi_signer
            .expect_update_current_message()
            .return_once(|_| Ok(()));
        mock_multi_signer
            .expect_register_single_signature()
            .return_once(|_, _, _| Err(ProtocolError::Core("an error occurred".to_string())));
        let mut dependency_manager = setup_dependency_manager();
        dependency_manager.with_multi_signer(Arc::new(RwLock::new(mock_multi_signer)));

        let signatures = &fake_data::single_signatures(1);

        let method = Method::POST.as_str();
        let path = "/register-signatures";

        let response = request()
            .method(method)
            .path(&format!("/{}{}", SERVER_BASE_PATH, path))
            .json(signatures)
            .reply(&setup_router(Arc::new(dependency_manager)))
            .await;

        APISpec::from_file(API_SPEC_FILE)
            .method(method)
            .path(path)
            .validate_request(&signatures)
            .unwrap()
            .validate_response(&response)
            .expect("OpenAPI error");
    }
}
