use crate::http_server::routes::middlewares;
use crate::DependencyManager;
use std::sync::Arc;
use warp::Filter;

pub fn routes(
    dependency_manager: Arc<DependencyManager>,
) -> impl Filter<Extract = impl warp::Reply, Error = warp::Rejection> + Clone {
    certificate_pending(dependency_manager.clone())
        .or(certificate_certificate_hash(dependency_manager))
}

/// GET /certificate-pending
fn certificate_pending(
    dependency_manager: Arc<DependencyManager>,
) -> impl Filter<Extract = impl warp::Reply, Error = warp::Rejection> + Clone {
    warp::path!("certificate-pending")
        .and(warp::get())
        .and(middlewares::with_certificate_pending_store(
            dependency_manager,
        ))
        .and_then(handlers::certificate_pending)
}

/// GET /certificate/{certificate_hash}
fn certificate_certificate_hash(
    dependency_manager: Arc<DependencyManager>,
) -> impl Filter<Extract = impl warp::Reply, Error = warp::Rejection> + Clone {
    warp::path!("certificate" / String)
        .and(warp::get())
        .and(middlewares::with_certificate_store(dependency_manager))
        .and_then(handlers::certificate_certificate_hash)
}

mod handlers {
    use crate::dependency::{CertificatePendingStoreWrapper, CertificateStoreWrapper};
    use crate::http_server::routes::reply;
    use slog_scope::{debug, warn};
    use std::convert::Infallible;
    use warp::http::StatusCode;

    /// Certificate Pending
    pub async fn certificate_pending(
        certificate_pending_store: CertificatePendingStoreWrapper,
    ) -> Result<impl warp::Reply, Infallible> {
        debug!("certificate_pending");

        match certificate_pending_store.get().await {
            Ok(Some(certificate_pending)) => Ok(reply::json(&certificate_pending, StatusCode::OK)),
            Ok(None) => Ok(reply::empty(StatusCode::NO_CONTENT)),
            Err(err) => {
                warn!("certificate_pending::error"; "error" => ?err);
                Ok(reply::internal_server_error(err.to_string()))
            }
        }
    }

    /// Certificate by certificate hash
    pub async fn certificate_certificate_hash(
        certificate_hash: String,
        certificate_store: CertificateStoreWrapper,
    ) -> Result<impl warp::Reply, Infallible> {
        debug!("certificate_certificate_hash/{}", certificate_hash);

        match certificate_store.get_from_hash(&certificate_hash).await {
            Ok(Some(certificate)) => Ok(reply::json(&certificate, StatusCode::OK)),
            Ok(None) => Ok(reply::empty(StatusCode::NOT_FOUND)),
            Err(err) => {
                warn!("certificate_certificate_hash::error"; "error" => ?err);
                Ok(reply::internal_server_error(err.to_string()))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    const API_SPEC_FILE: &str = "../openapi.yaml";

    use crate::http_server::SERVER_BASE_PATH;
    use mithril_common::apispec::APISpec;
    use mithril_common::store::adapter::FailStoreAdapter;
    use mithril_common::{entities, fake_data};
    use serde_json::Value::Null;
    use warp::http::Method;
    use warp::test::request;

    use super::*;
    use crate::initialize_dependencies;
    use crate::store::CertificateStore;

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
    async fn test_certificate_pending_get_ok() {
        let method = Method::GET.as_str();
        let path = "/certificate-pending";
        let (dependency_manager, _) = initialize_dependencies().await;

        let response = request()
            .method(method)
            .path(&format!("/{}{}", SERVER_BASE_PATH, path))
            .reply(&setup_router(Arc::new(dependency_manager)))
            .await;

        APISpec::from_file(API_SPEC_FILE)
            .method(method)
            .path(path)
            .validate_request(&Null)
            .unwrap()
            .validate_response(&response)
            .expect("OpenAPI error");
    }

    #[tokio::test]
    async fn test_certificate_pending_get_ok_204() {
        let (dependency_manager, _) = initialize_dependencies().await;

        let method = Method::GET.as_str();
        let path = "/certificate-pending";

        let response = request()
            .method(method)
            .path(&format!("/{}{}", SERVER_BASE_PATH, path))
            .reply(&setup_router(Arc::new(dependency_manager)))
            .await;

        APISpec::from_file(API_SPEC_FILE)
            .method(method)
            .path(path)
            .validate_request(&Null)
            .unwrap()
            .validate_response(&response)
            .expect("OpenAPI error");
    }

    #[tokio::test]
    async fn test_certificate_pending_get_ko_500() {
        let method = Method::GET.as_str();
        let path = "/certificate-pending";
        let (dependency_manager, _) = initialize_dependencies().await;

        let response = request()
            .method(method)
            .path(&format!("/{}{}", SERVER_BASE_PATH, path))
            .reply(&setup_router(Arc::new(dependency_manager)))
            .await;

        APISpec::from_file(API_SPEC_FILE)
            .method(method)
            .path(path)
            .validate_request(&Null)
            .unwrap()
            .validate_response(&response)
            .expect("OpenAPI error");
    }

    #[tokio::test]
    async fn test_certificate_certificate_hash_get_ok() {
        let (dependency_manager, _) = initialize_dependencies().await;
        dependency_manager
            .certificate_store
            .save(fake_data::certificate("{certificate_hash}".to_string()))
            .await
            .expect("certificate store save should have succeeded");

        let method = Method::GET.as_str();
        let path = "/certificate/{certificate_hash}";

        let response = request()
            .method(method)
            .path(&format!("/{}{}", SERVER_BASE_PATH, path))
            .reply(&setup_router(Arc::new(dependency_manager)))
            .await;

        APISpec::from_file(API_SPEC_FILE)
            .method(method)
            .path(path)
            .validate_request(&Null)
            .unwrap()
            .validate_response(&response)
            .expect("OpenAPI error");
    }

    #[tokio::test]
    async fn test_certificate_certificate_hash_get_ok_404() {
        let (dependency_manager, _) = initialize_dependencies().await;

        let method = Method::GET.as_str();
        let path = "/certificate/{certificate_hash}";

        let response = request()
            .method(method)
            .path(&format!("/{}{}", SERVER_BASE_PATH, path))
            .reply(&setup_router(Arc::new(dependency_manager)))
            .await;

        APISpec::from_file(API_SPEC_FILE)
            .method(method)
            .path(path)
            .validate_request(&Null)
            .unwrap()
            .validate_response(&response)
            .expect("OpenAPI error");
    }

    #[tokio::test]
    async fn test_certificate_certificate_hash_get_ko() {
        let (mut dependency_manager, _) = initialize_dependencies().await;
        let certificate_store = CertificateStore::new(Box::new(FailStoreAdapter::<
            String,
            entities::Certificate,
        >::new()));
        dependency_manager.certificate_store = Arc::new(certificate_store);

        let method = Method::GET.as_str();
        let path = "/certificate/{certificate_hash}";

        let response = request()
            .method(method)
            .path(&format!("/{}{}", SERVER_BASE_PATH, path))
            .reply(&setup_router(Arc::new(dependency_manager)))
            .await;

        APISpec::from_file(API_SPEC_FILE)
            .method(method)
            .path(path)
            .validate_request(&Null)
            .unwrap()
            .validate_response(&response)
            .expect("OpenAPI error");
    }
}
