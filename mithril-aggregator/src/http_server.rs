use mithril_common::crypto_helper::{key_decode_hex, ProtocolLotteryIndex, ProtocolPartyId};
use mithril_common::entities;
use mithril_common::fake_data;
use serde_json::Value::Null;
use slog_scope::{debug, info};
use std::convert::Infallible;
use std::net::IpAddr;
use std::sync::Arc;
use warp::Future;
use warp::{http::Method, http::StatusCode, Filter};

use super::dependency::{
    BeaconStoreWrapper, CertificateStoreWrapper, DependencyManager, MultiSignerWrapper,
    SnapshotStoreWrapper,
};
use super::multi_signer;
use super::Config;

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
                .or(serve_snapshots_dir(dependency_manager.clone()))
                .or(snapshot_download(dependency_manager.clone()))
                .or(snapshot_digest(dependency_manager.clone()))
                .or(register_signer(dependency_manager.clone()))
                .or(register_signatures(dependency_manager))
                .with(cors),
        )
    }

    /// GET /certificate-pending
    pub fn certificate_pending(
        dependency_manager: Arc<DependencyManager>,
    ) -> impl Filter<Extract = impl warp::Reply, Error = warp::Rejection> + Clone {
        warp::path!("certificate-pending")
            .and(warp::get())
            .and(with_beacon_store(dependency_manager.clone()))
            .and(with_multi_signer(dependency_manager))
            .and_then(handlers::certificate_pending)
    }

    /// GET /certificate/{certificate_hash}
    pub fn certificate_certificate_hash(
        dependency_manager: Arc<DependencyManager>,
    ) -> impl Filter<Extract = impl warp::Reply, Error = warp::Rejection> + Clone {
        warp::path!("certificate" / String)
            .and(warp::get())
            .and(with_certificate_store(dependency_manager))
            .and_then(handlers::certificate_certificate_hash)
    }

    /// GET /snapshots
    pub fn snapshots(
        dependency_manager: Arc<DependencyManager>,
    ) -> impl Filter<Extract = impl warp::Reply, Error = warp::Rejection> + Clone {
        warp::path!("snapshots")
            .and(warp::get())
            .and(with_snapshot_store(dependency_manager))
            .and_then(handlers::snapshots)
    }

    /// GET /snapshots/{digest}/download
    pub fn snapshot_download(
        dependency_manager: Arc<DependencyManager>,
    ) -> impl Filter<Extract = impl warp::Reply, Error = warp::Rejection> + Clone {
        warp::path!("snapshot" / String / "download")
            .and(warp::get())
            .and(with_config(dependency_manager.clone()))
            .and(with_snapshot_store(dependency_manager))
            .and_then(handlers::snapshot_download)
    }

    pub fn serve_snapshots_dir(
        dependency_manager: Arc<DependencyManager>,
    ) -> impl Filter<Extract = impl warp::Reply, Error = warp::Rejection> + Clone {
        let config = dependency_manager.config.clone();

        warp::path("snapshot_download")
            .and(warp::fs::dir(config.snapshot_directory))
            .and(with_snapshot_store(dependency_manager))
            .and_then(handlers::ensure_downloaded_file_is_a_snapshot)
    }

    /// GET /snapshot/digest
    pub fn snapshot_digest(
        dependency_manager: Arc<DependencyManager>,
    ) -> impl Filter<Extract = impl warp::Reply, Error = warp::Rejection> + Clone {
        warp::path!("snapshot" / String)
            .and(warp::get())
            .and(with_snapshot_store(dependency_manager))
            .and_then(handlers::snapshot_digest)
    }

    /// POST /register-signer
    pub fn register_signer(
        dependency_manager: Arc<DependencyManager>,
    ) -> impl Filter<Extract = impl warp::Reply, Error = warp::Rejection> + Clone {
        warp::path!("register-signer")
            .and(warp::post())
            .and(warp::body::json())
            .and(with_multi_signer(dependency_manager))
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

    /// With beacon store middleware
    fn with_beacon_store(
        dependency_manager: Arc<DependencyManager>,
    ) -> impl Filter<Extract = (BeaconStoreWrapper,), Error = Infallible> + Clone {
        warp::any().map(move || dependency_manager.beacon_store.as_ref().unwrap().clone())
    }

    /// With snapshot store middleware
    fn with_snapshot_store(
        dependency_manager: Arc<DependencyManager>,
    ) -> impl Filter<Extract = (SnapshotStoreWrapper,), Error = Infallible> + Clone {
        warp::any().map(move || dependency_manager.snapshot_store.as_ref().unwrap().clone())
    }

    /// With certificate store middleware
    fn with_certificate_store(
        dependency_manager: Arc<DependencyManager>,
    ) -> impl Filter<Extract = (CertificateStoreWrapper,), Error = Infallible> + Clone {
        warp::any().map(move || {
            dependency_manager
                .certificate_store
                .as_ref()
                .unwrap()
                .clone()
        })
    }

    /// With multi signer middleware
    fn with_multi_signer(
        dependency_manager: Arc<DependencyManager>,
    ) -> impl Filter<Extract = (MultiSignerWrapper,), Error = Infallible> + Clone {
        warp::any().map(move || dependency_manager.multi_signer.as_ref().unwrap().clone())
    }

    /// With config middleware
    fn with_config(
        dependency_manager: Arc<DependencyManager>,
    ) -> impl Filter<Extract = (Config,), Error = Infallible> + Clone {
        warp::any().map(move || dependency_manager.config.clone())
    }
}

mod handlers {
    use super::*;
    use std::str::FromStr;
    use warp::http::Uri;

    /// Certificate Pending
    pub async fn certificate_pending(
        beacon_store: BeaconStoreWrapper,
        multi_signer: MultiSignerWrapper,
    ) -> Result<impl warp::Reply, Infallible> {
        debug!("certificate_pending");

        let beacon_store = beacon_store.read().await;
        match beacon_store.get_current_beacon().await {
            Ok(Some(beacon)) => {
                let multi_signer = multi_signer.read().await;
                match multi_signer.get_multi_signature().await {
                    Ok(None) => {
                        let mut certificate_pending = fake_data::certificate_pending();
                        certificate_pending.beacon = beacon.clone();

                        let protocol_parameters = multi_signer.get_protocol_parameters().await;
                        if protocol_parameters.is_none() {
                            return Ok(warp::reply::with_status(
                                warp::reply::json(&entities::Error::new(
                                    "MITHRIL-E0004".to_string(),
                                    "no protocol parameters available".to_string(),
                                )),
                                StatusCode::INTERNAL_SERVER_ERROR,
                            ));
                        }
                        let protocol_parameters = protocol_parameters.unwrap().into();

                        let previous_hash = certificate_pending.previous_hash;

                        let signers = multi_signer.get_signers().await;
                        if let Err(err) = signers {
                            return Ok(warp::reply::with_status(
                                warp::reply::json(&entities::Error::new(
                                    "MITHRIL-E0007".to_string(),
                                    err.to_string(),
                                )),
                                StatusCode::INTERNAL_SERVER_ERROR,
                            ));
                        }
                        let signers = signers.unwrap();

                        let certificate_pending = entities::CertificatePending::new(
                            beacon,
                            protocol_parameters,
                            previous_hash,
                            signers,
                        );

                        Ok(warp::reply::with_status(
                            warp::reply::json(&certificate_pending),
                            StatusCode::OK,
                        ))
                    }
                    Ok(_) => Ok(warp::reply::with_status(
                        warp::reply::json(&Null),
                        StatusCode::NO_CONTENT,
                    )),
                    Err(err) => Ok(warp::reply::with_status(
                        warp::reply::json(&entities::Error::new(
                            "MITHRIL-E0008".to_string(),
                            err.to_string(),
                        )),
                        StatusCode::INTERNAL_SERVER_ERROR,
                    )),
                }
            }
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

    /// Snapshots
    pub async fn snapshots(
        snapshot_store: SnapshotStoreWrapper,
    ) -> Result<impl warp::Reply, Infallible> {
        debug!("snapshots");

        // Snapshots
        let snapshot_store = snapshot_store.read().await;
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

    /// Download a file if and only if it's a snapshot archive
    pub async fn ensure_downloaded_file_is_a_snapshot(
        reply: warp::fs::File,
        snapshot_store: SnapshotStoreWrapper,
    ) -> Result<impl warp::Reply, Infallible> {
        let filepath = reply.path().to_path_buf();
        debug!(
            "ensure_downloaded_file_is_a_snapshot / file: `{}`",
            filepath.display()
        );

        match crate::tools::extract_digest_from_path(&filepath) {
            Ok(digest) => {
                let snapshot_store = snapshot_store.read().await;
                match snapshot_store.get_snapshot_details(digest).await {
                    Ok(Some(_)) => Ok(Box::new(warp::reply::with_header(
                        reply,
                        "Content-Disposition",
                        format!(
                            "attachment; filename=\"{}\"",
                            filepath.file_name().unwrap().to_str().unwrap()
                        ),
                    )) as Box<dyn warp::Reply>),
                    _ => Ok(Box::new(warp::reply::with_status(
                        warp::reply::reply(),
                        StatusCode::NOT_FOUND,
                    ))),
                }
            }
            Err(_) => Ok(Box::new(warp::reply::with_status(
                warp::reply::reply(),
                StatusCode::NOT_FOUND,
            ))),
        }
    }

    /// Snapshot download
    pub async fn snapshot_download(
        digest: String,
        config: Config,
        snapshot_store: SnapshotStoreWrapper,
    ) -> Result<impl warp::Reply, Infallible> {
        debug!("snapshot_download/{}", digest);

        // Snapshot
        let snapshot_store = snapshot_store.read().await;
        match snapshot_store.get_snapshot_details(digest).await {
            Ok(Some(snapshot)) => {
                let filename = format!("{}.{}.tar.gz", config.network, snapshot.digest);
                let snapshot_uri = format!(
                    "{}{}/snapshot_download/{}",
                    config.server_url, SERVER_BASE_PATH, filename
                );
                let snapshot_uri = Uri::from_str(&snapshot_uri).unwrap();

                Ok(Box::new(warp::redirect::found(snapshot_uri)) as Box<dyn warp::Reply>)
            }
            Ok(None) => Ok(Box::new(warp::reply::with_status(
                warp::reply::reply(),
                StatusCode::NOT_FOUND,
            ))),
            Err(err) => Ok(Box::new(warp::reply::with_status(
                warp::reply::json(&entities::Error::new("MITHRIL-E0002".to_string(), err)),
                StatusCode::INTERNAL_SERVER_ERROR,
            ))),
        }
    }

    /// Snapshot by digest
    pub async fn snapshot_digest(
        digest: String,
        snapshot_store: SnapshotStoreWrapper,
    ) -> Result<impl warp::Reply, Infallible> {
        debug!("snapshot_digest/{}", digest);

        // Snapshot
        let snapshot_store = snapshot_store.read().await;
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
                    Ok(()) => Ok(warp::reply::with_status(
                        warp::reply::json(&Null),
                        StatusCode::CREATED,
                    )),
                    Err(multi_signer::ProtocolError::ExistingSigner()) => Ok(
                        warp::reply::with_status(warp::reply::json(&Null), StatusCode::CONFLICT),
                    ),
                    Err(err) => Ok(warp::reply::with_status(
                        warp::reply::json(&entities::Error::new(
                            "MITHRIL-E0006".to_string(),
                            err.to_string(),
                        )),
                        StatusCode::INTERNAL_SERVER_ERROR,
                    )),
                }
            }
            Err(_) => Ok(warp::reply::with_status(
                warp::reply::json(&Null),
                StatusCode::BAD_REQUEST,
            )),
        }
    }

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
                            signature.party_id as ProtocolPartyId,
                            &single_signature,
                            signature.index as ProtocolLotteryIndex,
                        )
                        .await
                    {
                        Err(multi_signer::ProtocolError::ExistingSingleSignature(_)) => {
                            return Ok(warp::reply::with_status(
                                warp::reply::json(&Null),
                                StatusCode::CONFLICT,
                            ));
                        }
                        Err(err) => {
                            return Ok(warp::reply::with_status(
                                warp::reply::json(&entities::Error::new(
                                    "MITHRIL-E0003".to_string(),
                                    err.to_string(),
                                )),
                                StatusCode::INTERNAL_SERVER_ERROR,
                            ));
                        }
                        _ => {}
                    }
                }
                Err(_) => {
                    return Ok(warp::reply::with_status(
                        warp::reply::json(&Null),
                        StatusCode::BAD_REQUEST,
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

    use mithril_common::apispec::APISpec;
    use mithril_common::fake_data;
    use serde_json::Value::Null;
    use tokio::sync::RwLock;
    use warp::test::request;

    use super::super::beacon_store::{BeaconStoreError, MockBeaconStore};
    use super::super::entities::*;
    use super::super::multi_signer::MockMultiSigner;
    use super::super::multi_signer::ProtocolError;
    use super::super::snapshot_stores::MockSnapshotStore;
    use super::super::store::adapter::DumbStoreAdapter;
    use super::super::store::adapter::FailStoreAdapter;
    use super::super::store::CertificateStore;
    use super::*;

    fn setup_dependency_manager() -> DependencyManager {
        let config = Config {
            network: "testnet".to_string(),
            url_snapshot_manifest: "https://storage.googleapis.com/cardano-testnet/snapshots.json"
                .to_string(),
            snapshot_store_type: SnapshotStoreType::Local,
            snapshot_uploader_type: SnapshotUploaderType::Local,
            server_url: "http://0.0.0.0:8080".to_string(),
            db_directory: Default::default(),
            snapshot_directory: Default::default(),
            pending_certificate_store_directory: std::env::temp_dir()
                .join("mithril_test_pending_cert_db"),
            certificate_store_directory: std::env::temp_dir().join("mithril_test_cert_db"),
            verification_key_store_directory: std::env::temp_dir()
                .join("mithril_test_verification_key_db"),
            stake_store_directory: std::env::temp_dir().join("mithril_test_stake_db"),
        };
        DependencyManager::new(config)
    }

    #[tokio::test]
    async fn test_certificate_pending_get_ok() {
        let fake_protocol_parameters = fake_data::protocol_parameters();
        let fake_signers = fake_data::signers(5);
        let method = Method::GET.as_str();
        let path = "/certificate-pending";
        let mut beacon_store = MockBeaconStore::new();
        beacon_store
            .expect_get_current_beacon()
            .return_once(|| Ok(Some(fake_data::beacon())));
        let mut mock_multi_signer = MockMultiSigner::new();
        mock_multi_signer
            .expect_get_protocol_parameters()
            .return_once(|| Some(fake_protocol_parameters.into()));
        mock_multi_signer
            .expect_get_signers()
            .return_once(|| Ok(fake_signers));
        mock_multi_signer
            .expect_get_multi_signature()
            .return_once(|| Ok(None));
        let mut dependency_manager = setup_dependency_manager();

        dependency_manager
            .with_beacon_store(Arc::new(RwLock::new(beacon_store)))
            .with_multi_signer(Arc::new(RwLock::new(mock_multi_signer)));

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
        let fake_protocol_parameters = fake_data::protocol_parameters();
        let mut beacon_store = MockBeaconStore::new();
        beacon_store
            .expect_get_current_beacon()
            .return_once(|| Ok(None));
        let mut mock_multi_signer = MockMultiSigner::new();
        mock_multi_signer
            .expect_get_protocol_parameters()
            .return_once(|| Some(fake_protocol_parameters.into()));
        let mut dependency_manager = setup_dependency_manager();
        dependency_manager
            .with_multi_signer(Arc::new(RwLock::new(mock_multi_signer)))
            .with_beacon_store(Arc::new(RwLock::new(beacon_store)));

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
    async fn test_certificate_pending_get_ko_current_beacon_500() {
        let fake_protocol_parameters = fake_data::protocol_parameters();
        let method = Method::GET.as_str();
        let path = "/certificate-pending";
        let mut beacon_store = MockBeaconStore::new();
        beacon_store
            .expect_get_current_beacon()
            .return_once(|| Err(BeaconStoreError::GenericError()));
        let mut mock_multi_signer = MockMultiSigner::new();
        mock_multi_signer
            .expect_get_protocol_parameters()
            .return_once(|| Some(fake_protocol_parameters.into()));
        let mut dependency_manager = setup_dependency_manager();
        dependency_manager
            .with_multi_signer(Arc::new(RwLock::new(mock_multi_signer)))
            .with_beacon_store(Arc::new(RwLock::new(beacon_store)));

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
    async fn test_certificate_pending_get_ko_signers_500() {
        let fake_protocol_parameters = fake_data::protocol_parameters();
        let mut beacon_store = MockBeaconStore::new();
        beacon_store
            .expect_get_current_beacon()
            .return_once(|| Ok(Some(fake_data::beacon())));
        let mut mock_multi_signer = MockMultiSigner::new();
        mock_multi_signer
            .expect_get_protocol_parameters()
            .return_once(|| Some(fake_protocol_parameters.into()));
        mock_multi_signer
            .expect_get_signers()
            .return_once(|| Err(ProtocolError::Codec("an error occurred".to_string())));
        mock_multi_signer
            .expect_get_multi_signature()
            .return_once(|| Ok(None));
        let mut dependency_manager = setup_dependency_manager();
        dependency_manager
            .with_multi_signer(Arc::new(RwLock::new(mock_multi_signer)))
            .with_beacon_store(Arc::new(RwLock::new(beacon_store)));

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
    async fn test_certificate_pending_get_ko_protocol_parameters_500() {
        let mut beacon_store = MockBeaconStore::new();
        beacon_store
            .expect_get_current_beacon()
            .return_once(|| Ok(Some(fake_data::beacon())));
        let mut mock_multi_signer = MockMultiSigner::new();
        mock_multi_signer
            .expect_get_protocol_parameters()
            .return_once(|| None);
        mock_multi_signer
            .expect_get_multi_signature()
            .return_once(|| Ok(None));
        let mut dependency_manager = setup_dependency_manager();
        dependency_manager
            .with_multi_signer(Arc::new(RwLock::new(mock_multi_signer)))
            .with_beacon_store(Arc::new(RwLock::new(beacon_store)));

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

    #[tokio::test]
    async fn test_snapshots_get_ok() {
        let fake_snapshots = fake_data::snapshots(5);
        let mut mock_snapshot_store = MockSnapshotStore::new();
        mock_snapshot_store
            .expect_list_snapshots()
            .return_const(Ok(fake_snapshots))
            .once();
        let mut dependency_manager = setup_dependency_manager();
        dependency_manager.with_snapshot_store(Arc::new(RwLock::new(mock_snapshot_store)));

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
        let mut mock_snapshot_store = MockSnapshotStore::new();
        mock_snapshot_store
            .expect_list_snapshots()
            .return_const(Err("an error occurred".to_string()))
            .once();
        let mut dependency_manager = setup_dependency_manager();
        dependency_manager.with_snapshot_store(Arc::new(RwLock::new(mock_snapshot_store)));

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
    async fn test_snapshot_download_get_ok() {
        let fake_snapshot = fake_data::snapshots(1).first().unwrap().to_owned();
        let mut mock_snapshot_store = MockSnapshotStore::new();
        mock_snapshot_store
            .expect_get_snapshot_details()
            .return_const(Ok(Some(fake_snapshot)))
            .once();
        let mut dependency_manager = setup_dependency_manager();
        dependency_manager.with_snapshot_store(Arc::new(RwLock::new(mock_snapshot_store)));

        let method = Method::GET.as_str();
        let path = "/snapshot/{digest}/download";

        let response = request()
            .method(method)
            .path(&format!("/{}{}", SERVER_BASE_PATH, path))
            .reply(&router::routes(Arc::new(dependency_manager)))
            .await;

        APISpec::from_file(API_SPEC_FILE)
            .method(method)
            .path(path)
            .content_type("application/gzip")
            .validate_request(&Null)
            .unwrap()
            .validate_response(&response)
            .expect("OpenAPI error");
    }

    #[tokio::test]
    async fn test_snapshot_download_get_ok_nosnapshot() {
        let mut mock_snapshot_store = MockSnapshotStore::new();
        mock_snapshot_store
            .expect_get_snapshot_details()
            .return_const(Ok(None))
            .once();
        let mut dependency_manager = setup_dependency_manager();
        dependency_manager.with_snapshot_store(Arc::new(RwLock::new(mock_snapshot_store)));

        let method = Method::GET.as_str();
        let path = "/snapshot/{digest}/download";

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
    async fn test_snapshot_download_get_ko() {
        let mut mock_snapshot_store = MockSnapshotStore::new();
        mock_snapshot_store
            .expect_get_snapshot_details()
            .return_const(Err("an error occurred".to_string()))
            .once();
        let mut dependency_manager = setup_dependency_manager();
        dependency_manager.with_snapshot_store(Arc::new(RwLock::new(mock_snapshot_store)));

        let method = Method::GET.as_str();
        let path = "/snapshot/{digest}/download";

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
        let mut mock_snapshot_store = MockSnapshotStore::new();
        mock_snapshot_store
            .expect_get_snapshot_details()
            .return_const(Ok(Some(fake_snapshot)))
            .once();
        let mut dependency_manager = setup_dependency_manager();
        dependency_manager.with_snapshot_store(Arc::new(RwLock::new(mock_snapshot_store)));

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
        let mut mock_snapshot_store = MockSnapshotStore::new();
        mock_snapshot_store
            .expect_get_snapshot_details()
            .return_const(Ok(None))
            .once();
        let mut dependency_manager = setup_dependency_manager();
        dependency_manager.with_snapshot_store(Arc::new(RwLock::new(mock_snapshot_store)));

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
        let mut mock_snapshot_store = MockSnapshotStore::new();
        mock_snapshot_store
            .expect_get_snapshot_details()
            .return_const(Err("an error occurred".to_string()))
            .once();
        let mut dependency_manager = setup_dependency_manager();
        dependency_manager.with_snapshot_store(Arc::new(RwLock::new(mock_snapshot_store)));

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
        let mut mock_multi_signer = MockMultiSigner::new();
        mock_multi_signer
            .expect_register_signer()
            .return_once(|_, _| Ok(()));
        let mut dependency_manager = setup_dependency_manager();
        dependency_manager.with_multi_signer(Arc::new(RwLock::new(mock_multi_signer)));

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
    async fn test_register_signer_post_ko_400() {
        let mock_multi_signer = MockMultiSigner::new();
        let mut dependency_manager = setup_dependency_manager();
        dependency_manager.with_multi_signer(Arc::new(RwLock::new(mock_multi_signer)));

        let mut signer = fake_data::signers(1)[0].clone();
        signer.verification_key = "invalid-key".to_string();

        let method = Method::POST.as_str();
        let path = "/register-signer";

        let response = request()
            .method(method)
            .path(&format!("/{}{}", SERVER_BASE_PATH, path))
            .json(&signer)
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
    async fn test_register_signer_post_ko_409() {
        let mut mock_multi_signer = MockMultiSigner::new();
        mock_multi_signer
            .expect_register_signer()
            .return_once(|_, _| Err(ProtocolError::ExistingSigner()));
        let mut dependency_manager = setup_dependency_manager();
        dependency_manager.with_multi_signer(Arc::new(RwLock::new(mock_multi_signer)));

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
    async fn test_register_signer_post_ko_500() {
        let mut mock_multi_signer = MockMultiSigner::new();
        mock_multi_signer
            .expect_register_signer()
            .return_once(|_, _| Err(ProtocolError::Core("an error occurred".to_string())));
        let mut dependency_manager = setup_dependency_manager();
        dependency_manager.with_multi_signer(Arc::new(RwLock::new(mock_multi_signer)));

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
