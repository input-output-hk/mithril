use crate::http_server::routes::middlewares;
use crate::DependencyManager;
use std::sync::Arc;
use warp::Filter;

pub fn routes(
    dependency_manager: Arc<DependencyManager>,
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
    register_signatures(dependency_manager)
}

/// POST /register-signatures
fn register_signatures(
    dependency_manager: Arc<DependencyManager>,
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone {
    warp::path!("register-signatures")
        .and(warp::post())
        .and(warp::body::bytes())
        .and(middlewares::with_multi_signer(dependency_manager))
        .and_then(handlers::register_signatures)
}

mod handlers {
    use crate::dependency::MultiSignerWrapper;
    use crate::http_server::routes::reply;
    use crate::ProtocolError;
    use mithril_common::entities::SingleSignatures;
    use mithril_common::messages::{
        MessageDecoder, SingleSignatureMessage, SINGLE_SIGNATURE_MESSAGE_SCHEMA,
    };
    use slog_scope::{debug, warn};
    use std::convert::Infallible;
    use warp::http::StatusCode;
    use warp::hyper::body::Bytes;

    /// Register Signatures
    pub async fn register_signatures(
        encoded_signature: Bytes,
        multi_signer: MultiSignerWrapper,
    ) -> Result<impl warp::Reply, Infallible> {
        debug!("⇄ HTTP SERVER: register_signatures");

        match read_signature_message(encoded_signature) {
            Ok(signature) => {
                debug!("⇄ HTTP SERVER: decoded signature: {:?}", signature);

                let mut multi_signer = multi_signer.write().await;
                match multi_signer.register_single_signature(&signature).await {
                    Err(ProtocolError::ExistingSingleSignature(party_id)) => {
                        debug!("register_signatures::already_exist"; "party_id" => ?party_id);
                        Ok(reply::empty(StatusCode::CONFLICT))
                    }
                    Err(err) => {
                        warn!("register_signatures::error"; "error" => ?err);
                        Ok(reply::internal_server_error(err.to_string()))
                    }
                    Ok(()) => Ok(reply::empty(StatusCode::CREATED)),
                }
            }
            Err(err) => {
                warn!("register_signatures::error"; "error" => ?err);
                Ok(reply::internal_server_error(err.to_string()))
            }
        }
    }

    fn read_signature_message(encoded_signature: Bytes) -> Result<SingleSignatures, String> {
        let decoder = MessageDecoder::with_schema(SINGLE_SIGNATURE_MESSAGE_SCHEMA)
            .map_err(|e| format!("Could not create message decoder: {}", e))?;
        let message = decoder
            .decode_one::<SingleSignatureMessage>(encoded_signature.to_vec())
            .map_err(|e| format!("Could not decode SingleSignatureMessage: {}", e))?;
        let signature = message
            .try_into()
            .map_err(|e| format!("Could not convert message to SingleSignature: {}", e))?;

        Ok(signature)
    }
}

#[cfg(test)]
mod tests {
    const API_SPEC_FILE: &str = "../openapi.yaml";

    use crate::http_server::SERVER_BASE_PATH;
    use mithril_common::test_utils::apispec::APISpec;
    use mithril_common::test_utils::fake_data;
    use tokio::sync::RwLock;
    use warp::http::Method;
    use warp::test::request;

    use super::*;
    use crate::multi_signer::MockMultiSigner;
    use crate::{initialize_dependencies, ProtocolError};

    fn setup_router(
        dependency_manager: Arc<DependencyManager>,
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
        let mut mock_multi_signer = MockMultiSigner::new();
        mock_multi_signer
            .expect_update_current_message()
            .return_once(|_| Ok(()));
        mock_multi_signer
            .expect_register_single_signature()
            .return_once(|_| Ok(()));
        let (mut dependency_manager, _) = initialize_dependencies().await;
        dependency_manager.multi_signer = Arc::new(RwLock::new(mock_multi_signer));

        let signatures = &fake_data::single_signatures(vec![1]);

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
            .expect_register_single_signature()
            .return_once(|_| Ok(()));
        let (mut dependency_manager, _) = initialize_dependencies().await;
        dependency_manager.multi_signer = Arc::new(RwLock::new(mock_multi_signer));

        let mut signatures = fake_data::single_signatures(vec![1]);
        signatures.signature = "invalid-signature".to_string();

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
        let signatures = fake_data::single_signatures(vec![1]);
        let party_id = signatures.party_id.clone();
        let mut mock_multi_signer = MockMultiSigner::new();
        mock_multi_signer
            .expect_update_current_message()
            .return_once(|_| Ok(()));
        mock_multi_signer
            .expect_register_single_signature()
            .return_once(move |_| Err(ProtocolError::ExistingSingleSignature(party_id)));
        let (mut dependency_manager, _) = initialize_dependencies().await;
        dependency_manager.multi_signer = Arc::new(RwLock::new(mock_multi_signer));

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
    async fn test_register_signatures_post_ko_500() {
        let mut mock_multi_signer = MockMultiSigner::new();
        mock_multi_signer
            .expect_update_current_message()
            .return_once(|_| Ok(()));
        mock_multi_signer
            .expect_register_single_signature()
            .return_once(|_| Err(ProtocolError::Core("an error occurred".to_string())));
        let (mut dependency_manager, _) = initialize_dependencies().await;
        dependency_manager.multi_signer = Arc::new(RwLock::new(mock_multi_signer));

        let signatures = &fake_data::single_signatures(vec![1]);

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
