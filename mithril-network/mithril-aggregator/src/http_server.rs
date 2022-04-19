use log::{debug, info};
use serde_json::Value::Null;
use std::convert::Infallible;
use std::net::IpAddr;
use warp::Future;
use warp::{http::Method, http::StatusCode, Filter};

use crate::entities;
use crate::fake_data;
use crate::snapshot_store::*;

const SERVER_BASE_PATH: &str = "aggregator";

/// Server
pub struct Server {
    ip: IpAddr,
    port: u16,
}

impl Server {
    /// Server factory
    pub fn new(ip: String, port: u16) -> Self {
        Self {
            ip: ip.parse::<IpAddr>().unwrap(),
            port,
        }
    }

    /// Start
    pub async fn start(&self, shutdown_signal: impl Future<Output = ()> + Send + 'static) {
        info!("Start Server");
        let routes = router::routes();
        let (_, server) =
            warp::serve(routes).bind_with_graceful_shutdown((self.ip, self.port), shutdown_signal);
        tokio::spawn(server).await.unwrap();
    }
}

mod router {
    use super::*;

    /// Routes
    pub fn routes() -> impl Filter<Extract = impl warp::Reply, Error = warp::Rejection> + Clone {
        let cors = warp::cors()
            .allow_any_origin()
            .allow_headers(vec!["content-type"])
            .allow_methods(vec![Method::GET, Method::POST, Method::OPTIONS]);

        warp::any().and(warp::path(SERVER_BASE_PATH)).and(
            certificate_pending()
                .or(certificate_certificate_hash())
                .or(snapshots())
                .or(snapshot_digest())
                .or(register_signer())
                .or(register_signatures())
                .with(cors),
        )
    }

    /// GET /certificate-pending
    pub fn certificate_pending(
    ) -> impl Filter<Extract = impl warp::Reply, Error = warp::Rejection> + Clone {
        warp::path!("certificate-pending")
            .and(warp::get())
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
    pub fn snapshots() -> impl Filter<Extract = impl warp::Reply, Error = warp::Rejection> + Clone {
        warp::path!("snapshots")
            .and(warp::get())
            .and_then(handlers::snapshots)
    }

    /// GET /snapshot/digest
    pub fn snapshot_digest(
    ) -> impl Filter<Extract = impl warp::Reply, Error = warp::Rejection> + Clone {
        warp::path!("snapshot" / String)
            .and(warp::get())
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
}

mod handlers {
    use super::*;

    /// Certificate Pending
    pub async fn certificate_pending() -> Result<impl warp::Reply, Infallible> {
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
    pub async fn snapshots() -> Result<impl warp::Reply, Infallible> {
        debug!("snapshots");

        // Snapshots
        //         let snapshots = fake_data::snapshots(1);
        //         Ok(warp::reply::with_status(
        //             warp::reply::json(&snapshots),
        //             StatusCode::OK,
        //         ))
        let network = "testnet";
        let url = format!(
            "https://storage.googleapis.com/cardano-{}/snapshots.json",
            network
        );
        let snapshot_store = SnapshotStoreHTTPClient::new(url);
        match snapshot_store.list_snapshots().await {
            Ok(snapshots) => Ok(warp::reply::with_status(
                warp::reply::json(&snapshots),
                StatusCode::OK,
            )),
            Err(err) => Ok(warp::reply::with_status(
                warp::reply::json(&err),
                StatusCode::INTERNAL_SERVER_ERROR,
            )),
        }
    }

    /// Snapshot by digest
    pub async fn snapshot_digest(digest: String) -> Result<impl warp::Reply, Infallible> {
        debug!("snapshot_digest/{}", digest);

        // Snapshot
        // let snapshots = fake_data::snapshots(10);
        // let snapshot = snapshots.last();

        // Ok(warp::reply::json(&snapshot))
        let network = "testnet";
        let url = format!(
            "https://storage.googleapis.com/cardano-{}/snapshots.json",
            network
        );
        let snapshot_store = SnapshotStoreHTTPClient::new(url);
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
                warp::reply::json(&err),
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
    use warp::test::request;

    use super::*;
    use crate::apispec::APISpec;
    use crate::fake_data;

    #[tokio::test]
    async fn test_certificate_pending_get_ok() {
        let method = Method::GET.as_str();
        let path = "/certificate-pending";

        let response = request()
            .method(&method)
            .path(&format!("/{}{}", SERVER_BASE_PATH, path))
            .reply(&router::routes())
            .await;

        APISpec::from_file(API_SPEC_FILE)
            .method(&method)
            .path(&path)
            .validate_request(&Null)
            .unwrap()
            .validate_response(&response)
            .expect("OpenAPI error");
    }

    #[tokio::test]
    async fn test_certificate_certificate_hash_get_ok() {
        let method = Method::GET.as_str();
        let path = "/certificate/{certificate_hash}";

        let response = request()
            .method(&method)
            .path(&format!("/{}{}", SERVER_BASE_PATH, path))
            .reply(&router::routes())
            .await;

        APISpec::from_file(API_SPEC_FILE)
            .method(&method)
            .path(&path)
            .validate_request(&Null)
            .unwrap()
            .validate_response(&response)
            .expect("OpenAPI error");
    }

    #[tokio::test]
    async fn test_snapshots_get_ok() {
        let method = Method::GET.as_str();
        let path = "/snapshots";

        let response = request()
            .method(&method)
            .path(&format!("/{}{}", SERVER_BASE_PATH, path))
            .reply(&router::routes())
            .await;

        APISpec::from_file(API_SPEC_FILE)
            .method(&method)
            .path(&path)
            .validate_request(&Null)
            .unwrap()
            .validate_response(&response)
            .expect("OpenAPI error");
    }

    #[tokio::test]
    async fn test_snapshot_digest_get_ok() {
        let method = Method::GET.as_str();
        let path = "/snapshot/{digest}";

        let response = request()
            .method(&method)
            .path(&format!("/{}{}", SERVER_BASE_PATH, path))
            .reply(&router::routes())
            .await;

        APISpec::from_file(API_SPEC_FILE)
            .method(&method)
            .path(&path)
            .validate_request(&Null)
            .unwrap()
            .validate_response(&response)
            .expect("OpenAPI error");
    }

    #[tokio::test]
    async fn test_register_signer_post_ok() {
        let signer = &fake_data::signers(1)[0];

        let method = Method::POST.as_str();
        let path = "/register-signer";

        let response = request()
            .method(&method)
            .path(&format!("/{}{}", SERVER_BASE_PATH, path))
            .json(signer)
            .reply(&router::routes())
            .await;

        APISpec::from_file(API_SPEC_FILE)
            .method(&method)
            .path(&path)
            .validate_request(&signer)
            .unwrap()
            .validate_response(&response)
            .expect("OpenAPI error");
    }

    #[tokio::test]
    async fn test_register_signatures_post_ok() {
        let signatures = &fake_data::single_signatures(1);

        let method = Method::POST.as_str();
        let path = "/register-signatures";

        let response = request()
            .method(&method)
            .path(&format!("/{}{}", SERVER_BASE_PATH, path))
            .json(signatures)
            .reply(&router::routes())
            .await;

        APISpec::from_file(API_SPEC_FILE)
            .method(&method)
            .path(&path)
            .validate_request(&signatures)
            .unwrap()
            .validate_response(&response)
            .expect("OpenAPI error");
    }
}
