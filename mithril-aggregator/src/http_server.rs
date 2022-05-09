use chrono::prelude::*;
use log::{debug, info};
use serde_json::Value::Null;
use std::convert::Infallible;
use std::net::IpAddr;
use std::sync::Arc;
use warp::Future;
use warp::{http::Method, http::StatusCode, Filter};

use crate::dependency::{DependencyManager, MultiSignerWrapper, SnapshotStorerWrapper};
use crate::entities;
use crate::fake_data;
use crate::multi_signer;

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
                .or(certificate_certificate_hash(dependency_manager.clone()))
                .or(snapshots(dependency_manager.clone()))
                .or(snapshot_digest(dependency_manager.clone()))
                .or(register_signer(dependency_manager.clone()))
                .or(register_signatures(dependency_manager))
                .with(cors),
        )
    }

    /// GET /certificate-pending
    pub fn certificate_pending(
        _dependency_manager: Arc<DependencyManager>,
    ) -> impl Filter<Extract = impl warp::Reply, Error = warp::Rejection> + Clone {
        warp::path!("certificate-pending")
            .and(warp::get())
            .and_then(handlers::certificate_pending)
    }

    /// GET /certificate/{certificate_hash}
    pub fn certificate_certificate_hash(
        dependency_manager: Arc<DependencyManager>,
    ) -> impl Filter<Extract = impl warp::Reply, Error = warp::Rejection> + Clone {
        warp::path!("certificate" / String)
            .and(warp::get())
            .and(with_multi_signer(dependency_manager))
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
        _dependency_manager: Arc<DependencyManager>,
    ) -> impl Filter<Extract = impl warp::Reply, Error = warp::Rejection> + Clone {
        warp::path!("register-signer")
            .and(warp::post())
            .and(warp::body::json())
            .and_then(handlers::register_signer)
    }

    /// POST /register-signatures
    pub fn register_signatures(
        dependency_manager: Arc<DependencyManager>,
    ) -> impl Filter<Extract = impl warp::Reply, Error = warp::Rejection> + Clone {
        warp::path!("register-signatures")
            .and(warp::post())
            .and(warp::body::json())
            .and(with_multi_signer(dependency_manager))
            .and_then(handlers::register_signatures)
    }

    /// With snapshot storer middleware
    fn with_snapshot_storer(
        dependency_manager: Arc<DependencyManager>,
    ) -> impl Filter<Extract = (SnapshotStorerWrapper,), Error = Infallible> + Clone {
        warp::any().map(move || dependency_manager.snapshot_storer.as_ref().unwrap().clone())
    }

    /// With multi signer middleware
    fn with_multi_signer(
        dependency_manager: Arc<DependencyManager>,
    ) -> impl Filter<Extract = (MultiSignerWrapper,), Error = Infallible> + Clone {
        warp::any().map(move || dependency_manager.multi_signer.as_ref().unwrap().clone())
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
        multi_signer: MultiSignerWrapper,
    ) -> Result<impl warp::Reply, Infallible> {
        debug!("certificate_certificate_hash/{}", certificate_hash);

        // Certificate
        // TODO: This is temporary implementation that will be replaced with real certificate production
        let multi_signer = multi_signer.read().await;
        let message = certificate_hash.clone();
        match multi_signer.get_multi_signature(message.clone()) {
            Ok(Some(multi_signature)) => {
                let beacon = fake_data::beacon();
                let protocol_parameters = fake_data::protocol_parameters();
                let digest = message.clone();
                let certificate_hash = message;
                let previous_hash = "".to_string();
                let block = beacon.block;
                let timestamp: DateTime<Utc> = Utc::now();
                let started_at = format!("{:?}", timestamp);
                let completed_at = started_at.clone();
                let multi_signature =
                    multi_signer::key_encode_hex_multisig(&multi_signature).unwrap();
                let signers = multi_signer
                    .get_stake_distribution()
                    .iter()
                    .map(|(party_id, stake)| {
                        let verification_key = match multi_signer.get_signer(*party_id) {
                            Some(verification_key) => {
                                multi_signer::key_encode_hex(verification_key).unwrap()
                            }
                            None => "".to_string(),
                        };
                        entities::SignerWithStake::new(
                            *party_id as u64,
                            verification_key,
                            *stake as u64,
                        )
                    })
                    .collect::<Vec<entities::SignerWithStake>>();

                let certificate = entities::Certificate::new(
                    certificate_hash,
                    previous_hash,
                    block,
                    protocol_parameters,
                    digest,
                    started_at,
                    completed_at,
                    signers,
                    multi_signature,
                );
                Ok(warp::reply::with_status(
                    warp::reply::json(&certificate),
                    StatusCode::OK,
                ))
            }
            Ok(None) => Ok(warp::reply::with_status(
                warp::reply::json(&Null),
                StatusCode::NOT_FOUND,
            )),
            Err(err) => Ok(warp::reply::with_status(
                warp::reply::json(&entities::Error::new("MITHRIL-E0005".to_string(), err)),
                StatusCode::INTERNAL_SERVER_ERROR,
            )),
        }
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
        multi_signer: MultiSignerWrapper,
    ) -> Result<impl warp::Reply, Infallible> {
        debug!("register_signatures/{:?}", signatures);

        let mut multi_signer = multi_signer.write().await;
        let message = fake_data::digest();
        multi_signer.update_current_message(message).unwrap();
        for signature in &signatures {
            match &multi_signer::key_decode_hex_sig(&signature.signature) {
                Ok(single_signature) => {
                    if let Err(err) = multi_signer.register_single_signature(
                        signature.party_id as multi_signer::ProtocolPartyId,
                        single_signature,
                        signature.index as multi_signer::ProtocolLotteryIndex,
                    ) {
                        return Ok(warp::reply::with_status(
                            warp::reply::json(&entities::Error::new(
                                "MITHRIL-E0003".to_string(),
                                err,
                            )),
                            StatusCode::INTERNAL_SERVER_ERROR,
                        ));
                    }
                }
                Err(err) => {
                    // TODO: Depending on the error we should return a 409 or a 500
                    return Ok(warp::reply::with_status(
                        warp::reply::json(&entities::Error::new(
                            "MITHRIL-E0004".to_string(),
                            err.to_string(),
                        )),
                        StatusCode::INTERNAL_SERVER_ERROR,
                    ));
                }
            }
        }
        Ok(warp::reply::with_status(
            warp::reply::json(&Null),
            StatusCode::CREATED,
        ))
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
    use crate::multi_signer::{key_decode_hex_multisig, MockMultiSigner};
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
        let multi_signature_hex = "000000000000000500000000000001d80000000000000000000000000000033a00000000000000448fa1ff304e39ccdc19dda4fcf80e387eba87e4bc91b534c86163d52e00c7c159bb581d87adf45624533643a406895e48066980805d30b00b04f68a30b4855a8ec018c16f8e1f4c6f6eea995ad0c01f7c5f66319e6334dca55efb44457910e0c2a832e9c10f8841487b9481b026c6d12f1cccb09039fb2a1c424c59619e3f36c6c2b087dd0eb9c5e1ca62f34ae9e18f9793b1aa7542a5423e21d8e84b4472c31664412cc604a666e9fdf03baf3c758e728c7a11576ebb01110ac39a0df95636e2abc3f2fdb319bb977a82cf7a547d8e7beb8dcea908a0bc4433dce0d3abcfd8c1cdbfed6fe581774290f8fc5cf73a475f00000000000000000000000000000003fd9e9edeebaab0c721c8421ad3b37ee237f0b61ef6d74c9e107204db3e73fd02c0db98af702d457044c3112ccdcc4eea6af6d11fc8ab6c1ba53ae54a3abf7dda6c6234b17181c24787ae4acd59af01d0f6a17ae71eb21af16234840e2500b5e0672e65dec73dd90c5cd8be73ec8eb6fd5119bd04fa3a2df350496653186d6fb4b4d0f933d8872c9007813c3ff46824e52b0b7b567370b8d09fb9490aa91b60da6398b542c93c9a0ed35ee59ed0a6e64c1776b2d05775e613e71dce3ae1165f670000000000000000000000000000033a00000000000000038fa1ff304e39ccdc19dda4fcf80e387eba87e4bc91b534c86163d52e00c7c159bb581d87adf45624533643a406895e48066980805d30b00b04f68a30b4855a8ec018c16f8e1f4c6f6eea995ad0c01f7c5f66319e6334dca55efb44457910e0c2a832e9c10f8841487b9481b026c6d12f1cccb09039fb2a1c424c59619e3f36c6c2b087dd0eb9c5e1ca62f34ae9e18f9793b1aa7542a5423e21d8e84b4472c31664412cc604a666e9fdf03baf3c758e728c7a11576ebb01110ac39a0df95636e2abc3f2fdb319bb977a82cf7a547d8e7beb8dcea908a0bc4433dce0d3abcfd8c1cdbfed6fe581774290f8fc5cf73a475f00000000000000000000000000000003fd9e9edeebaab0c721c8421ad3b37ee237f0b61ef6d74c9e107204db3e73fd02c0db98af702d457044c3112ccdcc4eea6af6d11fc8ab6c1ba53ae54a3abf7dda6c6234b17181c24787ae4acd59af01d0f6a17ae71eb21af16234840e2500b5e0672e65dec73dd90c5cd8be73ec8eb6fd5119bd04fa3a2df350496653186d6fb4b4d0f933d8872c9007813c3ff46824e52b0b7b567370b8d09fb9490aa91b60da6398b542c93c9a0ed35ee59ed0a6e64c1776b2d05775e613e71dce3ae1165f670000000000000000000000000000033a000000000000003f8fa1ff304e39ccdc19dda4fcf80e387eba87e4bc91b534c86163d52e00c7c159bb581d87adf45624533643a406895e48066980805d30b00b04f68a30b4855a8ec018c16f8e1f4c6f6eea995ad0c01f7c5f66319e6334dca55efb44457910e0c2a832e9c10f8841487b9481b026c6d12f1cccb09039fb2a1c424c59619e3f36c6c2b087dd0eb9c5e1ca62f34ae9e18f9793b1aa7542a5423e21d8e84b4472c31664412cc604a666e9fdf03baf3c758e728c7a11576ebb01110ac39a0df95636e2abc3f2fdb319bb977a82cf7a547d8e7beb8dcea908a0bc4433dce0d3abcfd8c1cdbfed6fe581774290f8fc5cf73a475f00000000000000000000000000000003fd9e9edeebaab0c721c8421ad3b37ee237f0b61ef6d74c9e107204db3e73fd02c0db98af702d457044c3112ccdcc4eea6af6d11fc8ab6c1ba53ae54a3abf7dda6c6234b17181c24787ae4acd59af01d0f6a17ae71eb21af16234840e2500b5e0672e65dec73dd90c5cd8be73ec8eb6fd5119bd04fa3a2df350496653186d6fb4b4d0f933d8872c9007813c3ff46824e52b0b7b567370b8d09fb9490aa91b60da6398b542c93c9a0ed35ee59ed0a6e64c1776b2d05775e613e71dce3ae1165f670000000000000000000000000000033a00000000000000428fa1ff304e39ccdc19dda4fcf80e387eba87e4bc91b534c86163d52e00c7c159bb581d87adf45624533643a406895e48066980805d30b00b04f68a30b4855a8ec018c16f8e1f4c6f6eea995ad0c01f7c5f66319e6334dca55efb44457910e0c2a832e9c10f8841487b9481b026c6d12f1cccb09039fb2a1c424c59619e3f36c6c2b087dd0eb9c5e1ca62f34ae9e18f9793b1aa7542a5423e21d8e84b4472c31664412cc604a666e9fdf03baf3c758e728c7a11576ebb01110ac39a0df95636e2abc3f2fdb319bb977a82cf7a547d8e7beb8dcea908a0bc4433dce0d3abcfd8c1cdbfed6fe581774290f8fc5cf73a475f00000000000000000000000000000003fd9e9edeebaab0c721c8421ad3b37ee237f0b61ef6d74c9e107204db3e73fd02c0db98af702d457044c3112ccdcc4eea6af6d11fc8ab6c1ba53ae54a3abf7dda6c6234b17181c24787ae4acd59af01d0f6a17ae71eb21af16234840e2500b5e0672e65dec73dd90c5cd8be73ec8eb6fd5119bd04fa3a2df350496653186d6fb4b4d0f933d8872c9007813c3ff46824e52b0b7b567370b8d09fb9490aa91b60da6398b542c93c9a0ed35ee59ed0a6e64c1776b2d05775e613e71dce3ae1165f670000000000000000000000000000033a00000000000000558fa1ff304e39ccdc19dda4fcf80e387eba87e4bc91b534c86163d52e00c7c159bb581d87adf45624533643a406895e48066980805d30b00b04f68a30b4855a8ec018c16f8e1f4c6f6eea995ad0c01f7c5f66319e6334dca55efb44457910e0c2a832e9c10f8841487b9481b026c6d12f1cccb09039fb2a1c424c59619e3f36c6c2b087dd0eb9c5e1ca62f34ae9e18f9793b1aa7542a5423e21d8e84b4472c31664412cc604a666e9fdf03baf3c758e728c7a11576ebb01110ac39a0df95636e2abc3f2fdb319bb977a82cf7a547d8e7beb8dcea908a0bc4433dce0d3abcfd8c1cdbfed6fe581774290f8fc5cf73a475f00000000000000000000000000000003fd9e9edeebaab0c721c8421ad3b37ee237f0b61ef6d74c9e107204db3e73fd02c0db98af702d457044c3112ccdcc4eea6af6d11fc8ab6c1ba53ae54a3abf7dda6c6234b17181c24787ae4acd59af01d0f6a17ae71eb21af16234840e2500b5e0672e65dec73dd90c5cd8be73ec8eb6fd5119bd04fa3a2df350496653186d6fb4b4d0f933d8872c9007813c3ff46824e52b0b7b567370b8d09fb9490aa91b60da6398b542c93c9a0ed35ee59ed0a6e64c1776b2d05775e613e71dce3ae1165f67".to_string();
        let mut mock_multi_signer = MockMultiSigner::new();
        mock_multi_signer
            .expect_get_multi_signature()
            .return_once(move |_| Ok(Some(key_decode_hex_multisig(&multi_signature_hex).unwrap())));
        mock_multi_signer
            .expect_get_stake_distribution()
            .return_once(|| Vec::new());
        let mut dependency_manager = setup_dependency_manager();
        dependency_manager.with_multi_signer(Arc::new(RwLock::new(mock_multi_signer)));

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
        let mut mock_multi_signer = MockMultiSigner::new();
        mock_multi_signer
            .expect_get_multi_signature()
            .return_once(|_| Ok(None));
        let mut dependency_manager = setup_dependency_manager();
        dependency_manager.with_multi_signer(Arc::new(RwLock::new(mock_multi_signer)));

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
        let mut mock_multi_signer = MockMultiSigner::new();
        mock_multi_signer
            .expect_get_multi_signature()
            .return_once(|_| Err("an error occurred".to_string()));
        let mut dependency_manager = setup_dependency_manager();
        dependency_manager.with_multi_signer(Arc::new(RwLock::new(mock_multi_signer)));

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
        let mut mock_multi_signer = MockMultiSigner::new();
        mock_multi_signer
            .expect_update_current_message()
            .return_const(Ok(()))
            .once();
        mock_multi_signer
            .expect_register_single_signature()
            .return_const(Ok(()))
            .once();
        let mut dependency_manager = setup_dependency_manager();
        dependency_manager.with_multi_signer(Arc::new(RwLock::new(mock_multi_signer)));

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

    #[tokio::test]
    async fn test_register_signatures_post_ko() {
        let mut mock_multi_signer = MockMultiSigner::new();
        mock_multi_signer
            .expect_update_current_message()
            .return_const(Ok(()))
            .once();
        mock_multi_signer
            .expect_register_single_signature()
            .return_const(Err("an error occurred".to_string()))
            .once();
        let mut dependency_manager = setup_dependency_manager();
        dependency_manager.with_multi_signer(Arc::new(RwLock::new(mock_multi_signer)));

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
