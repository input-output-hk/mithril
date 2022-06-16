use mithril_common::crypto_helper::{key_decode_hex, ProtocolLotteryIndex, ProtocolPartyId};
use mithril_common::entities;
use serde_json::Value::Null;
use slog_scope::{debug, info};
use std::convert::Infallible;
use std::net::IpAddr;
use std::sync::Arc;
use warp::Future;
use warp::{http::Method, http::StatusCode, Filter};

use crate::dependency::{
    CertificatePendingStoreWrapper, CertificateStoreWrapper, MultiSignerWrapper,
};
use crate::{multi_signer, DependencyManager};

pub const SERVER_BASE_PATH: &str = "aggregator";

/// Server
pub struct Server {
    ip: IpAddr,
    port: u16,
    dependency_manager: Arc<DependencyManager>,
}

impl Server {
    /// Server factory
    pub fn new(ip: String, port: u16, dependency_manager: Arc<DependencyManager>) -> Self {
        Self {
            ip: ip.parse::<IpAddr>().unwrap(),
            port,
            dependency_manager,
        }
    }

    /// Start
    pub async fn start(&self, shutdown_signal: impl Future<Output = ()> + Send + 'static) {
        info!("Start Aggregator Http Server");
        let routes = router::routes(self.dependency_manager.clone());
        let (_, server) =
            warp::serve(routes).bind_with_graceful_shutdown((self.ip, self.port), shutdown_signal);
        tokio::spawn(server).await.unwrap();
    }
}

mod router {
    use super::*;
    use crate::http_server::{middlewares, signatures_routes, signer_routes, snapshot_routes};

    /// Routes
    pub fn routes(
        dependency_manager: Arc<DependencyManager>,
    ) -> impl Filter<Extract = impl warp::Reply, Error = warp::Rejection> + Clone {
        let cors = warp::cors()
            .allow_any_origin()
            .allow_headers(vec!["content-type"])
            .allow_methods(vec![Method::GET, Method::POST, Method::OPTIONS]);

        warp::any().and(warp::path(SERVER_BASE_PATH)).and(
            certificate_pending(dependency_manager.clone())
                .or(certificate_certificate_hash(dependency_manager.clone()))
                .or(snapshot_routes::routes(dependency_manager.clone()))
                .or(signer_routes::routes(dependency_manager.clone()))
                .or(signatures_routes::routes(dependency_manager))
                .with(cors),
        )
    }

    /// GET /certificate-pending
    pub fn certificate_pending(
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
    pub fn certificate_certificate_hash(
        dependency_manager: Arc<DependencyManager>,
    ) -> impl Filter<Extract = impl warp::Reply, Error = warp::Rejection> + Clone {
        warp::path!("certificate" / String)
            .and(warp::get())
            .and(middlewares::with_certificate_store(dependency_manager))
            .and_then(handlers::certificate_certificate_hash)
    }
}

mod handlers {
    use super::*;

    /// Certificate Pending
    pub async fn certificate_pending(
        certificate_pending_store: CertificatePendingStoreWrapper,
    ) -> Result<impl warp::Reply, Infallible> {
        debug!("certificate_pending");

        let certificate_pending_store = certificate_pending_store.read().await;

        match certificate_pending_store.get().await {
            Ok(Some(certificate_pending)) => Ok(warp::reply::with_status(
                warp::reply::json(&certificate_pending),
                StatusCode::OK,
            )),
            Ok(None) => Ok(warp::reply::with_status(
                warp::reply::json(&Null),
                StatusCode::NO_CONTENT,
            )),
            Err(err) => Ok(warp::reply::with_status(
                warp::reply::json(&entities::Error::new(
                    "MITHRIL-E0006".to_string(),
                    err.to_string(),
                )),
                StatusCode::INTERNAL_SERVER_ERROR,
            )),
        }
    }

    /// Certificate by certificate hash
    pub async fn certificate_certificate_hash(
        certificate_hash: String,
        certificate_store: CertificateStoreWrapper,
    ) -> Result<impl warp::Reply, Infallible> {
        debug!("certificate_certificate_hash/{}", certificate_hash);

        let certificate_store = certificate_store.read().await;
        match certificate_store.get_from_hash(&certificate_hash).await {
            Ok(Some(certificate)) => Ok(warp::reply::with_status(
                warp::reply::json(&certificate),
                StatusCode::OK,
            )),
            Ok(None) => Ok(warp::reply::with_status(
                warp::reply::json(&Null),
                StatusCode::NOT_FOUND,
            )),
            Err(err) => Ok(warp::reply::with_status(
                warp::reply::json(&entities::Error::new(
                    "MITHRIL-E0005".to_string(),
                    err.to_string(),
                )),
                StatusCode::INTERNAL_SERVER_ERROR,
            )),
        }
    }
}

#[cfg(test)]
mod tests {
    const API_SPEC_FILE: &str = "../openapi.yaml";

    use mithril_common::apispec::APISpec;
    use mithril_common::fake_data;
    use mithril_common::store::adapter::DumbStoreAdapter;
    use mithril_common::store::adapter::FailStoreAdapter;
    use serde_json::Value::Null;
    use tokio::sync::RwLock;
    use warp::test::request;

    use super::*;
    use crate::multi_signer::MockMultiSigner;
    use crate::multi_signer::ProtocolError;
    use crate::store::CertificateStore;
    use crate::CertificatePendingStore;

    fn setup_dependency_manager() -> DependencyManager {
        DependencyManager::fake()
    }

    #[tokio::test]
    async fn test_certificate_pending_get_ok() {
        let method = Method::GET.as_str();
        let path = "/certificate-pending";
        let certificate_pending_store =
            CertificatePendingStore::new(Box::new(DumbStoreAdapter::new()));
        let mut dependency_manager = setup_dependency_manager();

        dependency_manager
            .with_certificate_pending_store(Arc::new(RwLock::new(certificate_pending_store)));

        let response = request()
            .method(method)
            .path(&format!("/{}{}", SERVER_BASE_PATH, path))
            .reply(&router::routes(Arc::new(dependency_manager)))
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
        let certificate_pending_store =
            CertificatePendingStore::new(Box::new(DumbStoreAdapter::new()));
        let mut dependency_manager = setup_dependency_manager();
        dependency_manager
            .with_certificate_pending_store(Arc::new(RwLock::new(certificate_pending_store)));

        let method = Method::GET.as_str();
        let path = "/certificate-pending";

        let response = request()
            .method(method)
            .path(&format!("/{}{}", SERVER_BASE_PATH, path))
            .reply(&router::routes(Arc::new(dependency_manager)))
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
        let certificate_pending_store =
            CertificatePendingStore::new(Box::new(DumbStoreAdapter::new()));
        let mut dependency_manager = setup_dependency_manager();
        dependency_manager
            .with_certificate_pending_store(Arc::new(RwLock::new(certificate_pending_store)));

        let response = request()
            .method(method)
            .path(&format!("/{}{}", SERVER_BASE_PATH, path))
            .reply(&router::routes(Arc::new(dependency_manager)))
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
        let mut certificate_store = CertificateStore::new(Box::new(DumbStoreAdapter::<
            String,
            entities::Certificate,
        >::new()));
        certificate_store
            .save(fake_data::certificate("cert-hash-123".to_string()))
            .await
            .expect("certificate store save should have succeeded");
        let mut dependency_manager = setup_dependency_manager();
        dependency_manager.with_certificate_store(Arc::new(RwLock::new(certificate_store)));

        let method = Method::GET.as_str();
        let path = "/certificate/{certificate_hash}";

        let response = request()
            .method(method)
            .path(&format!("/{}{}", SERVER_BASE_PATH, path))
            .reply(&router::routes(Arc::new(dependency_manager)))
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
        let certificate_store = CertificateStore::new(Box::new(DumbStoreAdapter::<
            String,
            entities::Certificate,
        >::new()));
        let mut dependency_manager = setup_dependency_manager();
        dependency_manager.with_certificate_store(Arc::new(RwLock::new(certificate_store)));

        let method = Method::GET.as_str();
        let path = "/certificate/{certificate_hash}";

        let response = request()
            .method(method)
            .path(&format!("/{}{}", SERVER_BASE_PATH, path))
            .reply(&router::routes(Arc::new(dependency_manager)))
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
        let certificate_store = CertificateStore::new(Box::new(FailStoreAdapter::<
            String,
            entities::Certificate,
        >::new()));
        let mut dependency_manager = setup_dependency_manager();
        dependency_manager.with_certificate_store(Arc::new(RwLock::new(certificate_store)));

        let method = Method::GET.as_str();
        let path = "/certificate/{certificate_hash}";

        let response = request()
            .method(method)
            .path(&format!("/{}{}", SERVER_BASE_PATH, path))
            .reply(&router::routes(Arc::new(dependency_manager)))
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
