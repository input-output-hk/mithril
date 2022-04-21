use log::{debug, info};
use serde_json::Value::Null;
use std::convert::Infallible;
use std::net::IpAddr;
use std::sync::Arc;
use warp::Future;
use warp::{http::Method, http::StatusCode, Filter};

use crate::dependency::{DependencyManager, SnapshotStorerWrapper};
use crate::entities;
use crate::fake_data;

const SERVER_BASE_PATH: &str = "aggregator";

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
        info!("Start Server");
        let routes = router::routes(self.dependency_manager.clone());
        let (_, server) =
            warp::serve(routes).bind_with_graceful_shutdown((self.ip, self.port), shutdown_signal);
        tokio::spawn(server).await.unwrap();
    }
}

mod router {
    use super::*;

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
                .or(certificate_certificate_hash())
                .or(snapshots(dependency_manager.clone()))
                .or(snapshot_digest(dependency_manager))
                .or(register_signer())
                .or(register_signatures())
                .with(cors),
        )
    }

    /// GET /certificate-pending
    pub fn certificate_pending(
        dependency_manager: Arc<DependencyManager>,
    ) -> impl Filter<Extract = impl warp::Reply, Error = warp::Rejection> + Clone {
        warp::path!("certificate-pending")
            .and(warp::get())
            .and(with_dependency_manager(dependency_manager))
            .and_then(handlers::certificate_pending)
    }

    /// GET /certificate/{certificate_hash}
    pub fn certificate_certificate_hash(
    ) -> impl Filter<Extract = impl warp::Reply, Error = warp::Rejection> + Clone {
        warp::path!("certificate" / String)
            .and(warp::get())
            .and_then(handlers::certificate_certificate_hash)
    }

    /// GET /snapshots
    pub fn snapshots(
        dependency_manager: Arc<DependencyManager>,
    ) -> impl Filter<Extract = impl warp::Reply, Error = warp::Rejection> + Clone {
        warp::path!("snapshots")
            .and(warp::get())
            .and(with_snapshot_storer(dependency_manager))
            .and_then(handlers::snapshots)
    }

    /// GET /snapshot/digest
    pub fn snapshot_digest(
        dependency_manager: Arc<DependencyManager>,
    ) -> impl Filter<Extract = impl warp::Reply, Error = warp::Rejection> + Clone {
        warp::path!("snapshot" / String)
            .and(warp::get())
            .and(with_snapshot_storer(dependency_manager))
            .and_then(handlers::snapshot_digest)
    }

    /// POST /register-signer
    pub fn register_signer(
    ) -> impl Filter<Extract = impl warp::Reply, Error = warp::Rejection> + Clone {
        warp::path!("register-signer")
            .and(warp::post())
            .and(warp::body::json())
            .and_then(handlers::register_signer)
    }

    /// POST /register-signatures
    pub fn register_signatures(
    ) -> impl Filter<Extract = impl warp::Reply, Error = warp::Rejection> + Clone {
        warp::path!("register-signatures")
            .and(warp::post())
            .and(warp::body::json())
            .and_then(handlers::register_signatures)
    }

    /// With dependency manager middleware
    fn with_dependency_manager(
        dependency_manager: Arc<DependencyManager>,
    ) -> impl Filter<Extract = (Arc<DependencyManager>,), Error = Infallible> + Clone {
        warp::any().map(move || dependency_manager.clone())
    }

    /// With snapshot storer middleware
    fn with_snapshot_storer(
        dependency_manager: Arc<DependencyManager>,
    ) -> impl Filter<Extract = (SnapshotStorerWrapper,), Error = Infallible> + Clone {
        warp::any().map(move || dependency_manager.snapshot_storer.as_ref().unwrap().clone())
    }
}

mod handlers {
    use super::*;

    /// Certificate Pending
    pub async fn certificate_pending(
        _dependency_manager: Arc<DependencyManager>,
    ) -> Result<impl warp::Reply, Infallible> {
        debug!("certificate_pending");

        // Certificate pending
        let certificate_pending = fake_data::certificate_pending();

        Ok(warp::reply::json(&certificate_pending))
    }

    /// Certificate by certificate hash
    pub async fn certificate_certificate_hash(
        certificate_hash: String,
    ) -> Result<impl warp::Reply, Infallible> {
        debug!("certificate_certificate_hash/{}", certificate_hash);

        // Certificate
        let certificate = fake_data::certificate(certificate_hash);

        Ok(warp::reply::json(&certificate))
    }

    /// Snapshots
    pub async fn snapshots(
        snapshot_storer: SnapshotStorerWrapper,
    ) -> Result<impl warp::Reply, Infallible> {
        debug!("snapshots");

        // Snapshots
        let snapshot_store = snapshot_storer.read().await;
        match snapshot_store.list_snapshots().await {
            Ok(snapshots) => Ok(warp::reply::with_status(
                warp::reply::json(&snapshots),
                StatusCode::OK,
            )),
            Err(err) => Ok(warp::reply::with_status(
                warp::reply::json(&entities::Error::new("MITHRIL-E0001".to_string(), err)),
                StatusCode::INTERNAL_SERVER_ERROR,
            )),
        }
    }

    /// Snapshot by digest
    pub async fn snapshot_digest(
        digest: String,
        snapshot_storer: SnapshotStorerWrapper,
    ) -> Result<impl warp::Reply, Infallible> {
        debug!("snapshot_digest/{}", digest);

        // Snapshot
        let snapshot_store = snapshot_storer.read().await;
        match snapshot_store.get_snapshot_details(digest).await {
            Ok(snapshot) => match snapshot {
                Some(snapshot) => Ok(warp::reply::with_status(
                    warp::reply::json(&snapshot),
                    StatusCode::OK,
                )),
                None => Ok(warp::reply::with_status(
                    warp::reply::json(&Null),
                    StatusCode::NOT_FOUND,
                )),
            },
            Err(err) => Ok(warp::reply::with_status(
                warp::reply::json(&entities::Error::new("MITHRIL-E0002".to_string(), err)),
                StatusCode::INTERNAL_SERVER_ERROR,
            )),
        }
    }

    /// Register Signer
    pub async fn register_signer(signer: entities::Signer) -> Result<impl warp::Reply, Infallible> {
        debug!("register_signer/{:?}", signer);

        Ok(StatusCode::CREATED)
    }

    /// Register Signatures
    pub async fn register_signatures(
        signatures: Vec<entities::SingleSignature>,
    ) -> Result<impl warp::Reply, Infallible> {
        debug!("register_signatures/{:?}", signatures);

        Ok(StatusCode::CREATED)
    }
}

#[cfg(test)]
mod tests {
    const API_SPEC_FILE: &str = "../openapi.yaml";

    use serde_json::Value::Null;
    use tokio::sync::RwLock;
    use warp::test::request;

    use super::*;
    use crate::apispec::APISpec;
    use crate::entities::*;
    use crate::fake_data;
    use crate::snapshot_store::MockSnapshotStorer;

    fn setup_dependency_manager() -> DependencyManager {
        let config = Config {
            network: "testnet".to_string(),
            url_snapshot_manifest: "https://storage.googleapis.com/cardano-testnet/snapshots.json"
                .to_string(),
        };
        DependencyManager::new(config)
    }

    #[tokio::test]
    async fn test_certificate_pending_get_ok() {
        let dependency_manager = setup_dependency_manager();
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
    async fn test_certificate_certificate_hash_get_ok() {
        let dependency_manager = setup_dependency_manager();
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
    async fn test_snapshots_get_ok() {
        let fake_snapshots = fake_data::snapshots(5);
        let mut mock_snapshot_storer = MockSnapshotStorer::new();
        mock_snapshot_storer
            .expect_list_snapshots()
            .return_const(Ok(fake_snapshots))
            .once();
        let mut dependency_manager = setup_dependency_manager();
        dependency_manager.with_snapshot_storer(Arc::new(RwLock::new(mock_snapshot_storer)));

        let method = Method::GET.as_str();
        let path = "/snapshots";

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
    async fn test_snapshots_get_ko() {
        let mut mock_snapshot_storer = MockSnapshotStorer::new();
        mock_snapshot_storer
            .expect_list_snapshots()
            .return_const(Err("an error occurred".to_string()))
            .once();
        let mut dependency_manager = setup_dependency_manager();
        dependency_manager.with_snapshot_storer(Arc::new(RwLock::new(mock_snapshot_storer)));

        let method = Method::GET.as_str();
        let path = "/snapshots";

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
    async fn test_snapshot_digest_get_ok() {
        let fake_snapshot = fake_data::snapshots(1).first().unwrap().to_owned();
        let mut mock_snapshot_storer = MockSnapshotStorer::new();
        mock_snapshot_storer
            .expect_get_snapshot_details()
            .return_const(Ok(Some(fake_snapshot)))
            .once();
        let mut dependency_manager = setup_dependency_manager();
        dependency_manager.with_snapshot_storer(Arc::new(RwLock::new(mock_snapshot_storer)));

        let method = Method::GET.as_str();
        let path = "/snapshot/{digest}";

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
    async fn test_snapshot_digest_get_ok_nosnapshot() {
        let mut mock_snapshot_storer = MockSnapshotStorer::new();
        mock_snapshot_storer
            .expect_get_snapshot_details()
            .return_const(Ok(None))
            .once();
        let mut dependency_manager = setup_dependency_manager();
        dependency_manager.with_snapshot_storer(Arc::new(RwLock::new(mock_snapshot_storer)));

        let method = Method::GET.as_str();
        let path = "/snapshot/{digest}";

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
    async fn test_snapshot_digest_get_ko() {
        let mut mock_snapshot_storer = MockSnapshotStorer::new();
        mock_snapshot_storer
            .expect_get_snapshot_details()
            .return_const(Err("an error occurred".to_string()))
            .once();
        let mut dependency_manager = setup_dependency_manager();
        dependency_manager.with_snapshot_storer(Arc::new(RwLock::new(mock_snapshot_storer)));

        let method = Method::GET.as_str();
        let path = "/snapshot/{digest}";

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
    async fn test_register_signer_post_ok() {
        let dependency_manager = setup_dependency_manager();
        let signer = &fake_data::signers(1)[0];

        let method = Method::POST.as_str();
        let path = "/register-signer";

        let response = request()
            .method(method)
            .path(&format!("/{}{}", SERVER_BASE_PATH, path))
            .json(signer)
            .reply(&router::routes(Arc::new(dependency_manager)))
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
    async fn test_register_signatures_post_ok() {
        let dependency_manager = setup_dependency_manager();
        let signatures = &fake_data::single_signatures(1);

        let method = Method::POST.as_str();
        let path = "/register-signatures";

        let response = request()
            .method(method)
            .path(&format!("/{}{}", SERVER_BASE_PATH, path))
            .json(signatures)
            .reply(&router::routes(Arc::new(dependency_manager)))
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
