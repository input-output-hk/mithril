use chrono::prelude::*;
use hex::ToHex;
use mithril_common::crypto_helper::{key_decode_hex, key_encode_hex, ProtocolPartyId};
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
    BeaconStoreWrapper, DependencyManager, MultiSignerWrapper, SnapshotStoreWrapper,
};
use super::multi_signer;

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
            .and(with_beacon_store(dependency_manager.clone()))
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

    /// With snapshot storer middleware
    fn with_snapshot_storer(
        dependency_manager: Arc<DependencyManager>,
    ) -> impl Filter<Extract = (SnapshotStoreWrapper,), Error = Infallible> + Clone {
        warp::any().map(move || dependency_manager.snapshot_storer.as_ref().unwrap().clone())
    }

    /// With multi signer middleware
    fn with_multi_signer(
        dependency_manager: Arc<DependencyManager>,
    ) -> impl Filter<Extract = (MultiSignerWrapper,), Error = Infallible> + Clone {
        warp::any().map(move || dependency_manager.multi_signer.as_ref().unwrap().clone())
    }

    /// With beacon store middleware
    fn with_beacon_store(
        dependency_manager: Arc<DependencyManager>,
    ) -> impl Filter<Extract = (BeaconStoreWrapper,), Error = Infallible> + Clone {
        warp::any().map(move || dependency_manager.beacon_store.as_ref().unwrap().clone())
    }
}

mod handlers {
    use super::*;

    /// Certificate Pending
    pub async fn certificate_pending(
        beacon_store: BeaconStoreWrapper,
        multi_signer: MultiSignerWrapper,
    ) -> Result<impl warp::Reply, Infallible> {
        debug!("certificate_pending");

        let beacon_store = beacon_store.read().await;
        match beacon_store.get_current_beacon().await {
            Ok(Some(beacon)) => {
                let message = fake_data::digest(&beacon);
                let multi_signer = multi_signer.read().await;
                match multi_signer.get_multi_signature(message.encode_hex::<String>()) {
                    Ok(None) => {
                        let mut certificate_pending = fake_data::certificate_pending();
                        certificate_pending.beacon = beacon.clone();

                        let protocol_parameters = multi_signer.get_protocol_parameters();
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

                        let signers = multi_signer.get_signers();
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
        beacon_store: BeaconStoreWrapper,
        multi_signer: MultiSignerWrapper,
    ) -> Result<impl warp::Reply, Infallible> {
        debug!("certificate_certificate_hash/{}", certificate_hash);

        // Certificate
        // TODO: This is temporary implementation that will be replaced with real certificate production
        let multi_signer = multi_signer.read().await;
        let message = certificate_hash.clone();
        match multi_signer.get_multi_signature(message.clone()) {
            Ok(Some(multi_signature)) => {
                let beacon_store = beacon_store.read().await;
                let beacon = beacon_store.get_current_beacon().await.unwrap().unwrap();
                let protocol_parameters = fake_data::protocol_parameters();
                let digest = message.clone();
                let certificate_hash = message;
                let previous_hash = "".to_string();
                let timestamp: DateTime<Utc> = Utc::now();
                let started_at = format!("{:?}", timestamp);
                let completed_at = started_at.clone();
                let multi_signature = key_encode_hex(&multi_signature).unwrap();
                let signers = multi_signer
                    .get_stake_distribution()
                    .iter()
                    .map(|(party_id, stake)| {
                        let verification_key = match multi_signer.get_signer(*party_id).unwrap() {
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
                    beacon,
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
        snapshot_storer: SnapshotStoreWrapper,
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
        snapshot_storer: SnapshotStoreWrapper,
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
                    match multi_signer.register_single_signature(
                        signature.party_id as multi_signer::ProtocolPartyId,
                        &single_signature,
                        signature.index as multi_signer::ProtocolLotteryIndex,
                    ) {
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
    use super::super::multi_signer::ProtocolError;

    use super::super::entities::*;
    use super::super::multi_signer::MockMultiSigner;
    use super::super::snapshot_store::MockSnapshotStore;
    use super::*;

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
        let fake_protocol_parameters = fake_data::protocol_parameters();
        let fake_signers_with_stakes = fake_data::signers_with_stakes(5);
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
            .return_once(|| Ok(fake_signers_with_stakes));
        mock_multi_signer
            .expect_get_multi_signature()
            .return_once(|_| Ok(None));
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
            .return_once(|_| Ok(None));
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
            .return_once(|_| Ok(None));
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
        let multi_signature_hex = "7b227369676e617475726573223a5b7b227369676d61223a5b3134362c3235332c3139302c3130302c3231362c3133322c3231362c3132322c3231352c3232342c3232302c3135302c32322c3234392c36392c3131312c3134322c3230352c3230332c3233302c34342c34312c3133352c38352c3136332c34372c3231332c37302c3133332c33382c3230352c3132342c3233302c3139302c3233312c36392c3138322c3138352c32342c39382c3137392c3133302c33312c3131352c3233332c3232372c3132312c3131395d2c22706b223a5b3134332c3136312c3235352c34382c37382c35372c3230342c3232302c32352c3232312c3136342c3235322c3234382c31342c35362c3132362c3138362c3133352c3232382c3138382c3134352c3138312c35322c3230302c39372c39392c3231332c34362c302c3139392c3139332c38392c3138372c38382c32392c3133352c3137332c3234342c38362c33362c38332c35342c36372c3136342c362c3133372c39342c37322c362c3130352c3132382c3132382c39332c34382c3137362c31312c342c3234362c3133382c34382c3138302c3133332c39302c3134322c3139322c32342c3139332c3131312c3134322c33312c37362c3131312c3131302c3233342c3135332c39302c3230382c3139322c33312c3132342c39352c3130322c34392c3135382c39392c35322c3232302c3136352c39342c3235312c36382c36392c3132312c31362c3232342c3139345d2c227374616b65223a3832362c22696e646578223a382c2270617468223a7b2276616c756573223a5b5b34372c3136332c3234362c3133342c3232332c3133352c3130352c3134392c32322c3132362c3132342c34362c39332c3131362c3139362c3139392c3138322c3232382c3134332c3132382c3130342c3235342c31342c36382c33322c3133312c36382c3231322c3132382c3234372c3134342c37362c35342c3135302c36322c36382c31372c39352c3232372c3233352c34322c35382c3230302c3130352c37362c34302c3138382c3138302c3234352c3136302c3234332c33392c3131312c34362c3132312c37322c3132352c3133302c32352c352c3132322c38302c3131302c37355d2c5b36342c38302c33332c3138382c33342c3234372c3230392c3137332c33332c32392c3135392c3235322c35392c3136392c3231392c3134372c3231392c3134332c3137392c32362c3139312c37332c34352c3234352c32382c3138332c3234372c3133342c312c3137342c3131382c3133352c3134352c3231322c3136332c342c36362c36382c35362c38382c3138392c37342c37312c382c32322c34312c3232352c3231352c3231342c3231342c3134392c3134372c36302c3137372c33352c3231372c3137352c3130382c3134372c3132392c3137352c3132372c35332c3135365d2c5b31362c38372c3134332c31352c3130382c3136342c3233312c3235332c32312c3133322c3134372c36382c3132392c39342c3231362c33362c3235332c37312c3230382c36322c3134382c3134332c3231382c37382c3231392c3130322c3136332c3232382c3130382c35382c342c3135392c3133342c3138312c34372c32342c34372c32392c38332c3133342c35342c39362c39372c3232312c31362c34312c3135372c3139312c36372c34352c3130302c3137372c3231362c3234322c35382c3137372c3134322c35342c3137382c32392c3131372c3139332c3130382c3137365d5d2c22696e646578223a342c22686173686572223a6e756c6c7d7d2c7b227369676d61223a5b3134362c3235332c3139302c3130302c3231362c3133322c3231362c3132322c3231352c3232342c3232302c3135302c32322c3234392c36392c3131312c3134322c3230352c3230332c3233302c34342c34312c3133352c38352c3136332c34372c3231332c37302c3133332c33382c3230352c3132342c3233302c3139302c3233312c36392c3138322c3138352c32342c39382c3137392c3133302c33312c3131352c3233332c3232372c3132312c3131395d2c22706b223a5b3134332c3136312c3235352c34382c37382c35372c3230342c3232302c32352c3232312c3136342c3235322c3234382c31342c35362c3132362c3138362c3133352c3232382c3138382c3134352c3138312c35322c3230302c39372c39392c3231332c34362c302c3139392c3139332c38392c3138372c38382c32392c3133352c3137332c3234342c38362c33362c38332c35342c36372c3136342c362c3133372c39342c37322c362c3130352c3132382c3132382c39332c34382c3137362c31312c342c3234362c3133382c34382c3138302c3133332c39302c3134322c3139322c32342c3139332c3131312c3134322c33312c37362c3131312c3131302c3233342c3135332c39302c3230382c3139322c33312c3132342c39352c3130322c34392c3135382c39392c35322c3232302c3136352c39342c3235312c36382c36392c3132312c31362c3232342c3139345d2c227374616b65223a3832362c22696e646578223a31342c2270617468223a7b2276616c756573223a5b5b34372c3136332c3234362c3133342c3232332c3133352c3130352c3134392c32322c3132362c3132342c34362c39332c3131362c3139362c3139392c3138322c3232382c3134332c3132382c3130342c3235342c31342c36382c33322c3133312c36382c3231322c3132382c3234372c3134342c37362c35342c3135302c36322c36382c31372c39352c3232372c3233352c34322c35382c3230302c3130352c37362c34302c3138382c3138302c3234352c3136302c3234332c33392c3131312c34362c3132312c37322c3132352c3133302c32352c352c3132322c38302c3131302c37355d2c5b36342c38302c33332c3138382c33342c3234372c3230392c3137332c33332c32392c3135392c3235322c35392c3136392c3231392c3134372c3231392c3134332c3137392c32362c3139312c37332c34352c3234352c32382c3138332c3234372c3133342c312c3137342c3131382c3133352c3134352c3231322c3136332c342c36362c36382c35362c38382c3138392c37342c37312c382c32322c34312c3232352c3231352c3231342c3231342c3134392c3134372c36302c3137372c33352c3231372c3137352c3130382c3134372c3132392c3137352c3132372c35332c3135365d2c5b31362c38372c3134332c31352c3130382c3136342c3233312c3235332c32312c3133322c3134372c36382c3132392c39342c3231362c33362c3235332c37312c3230382c36322c3134382c3134332c3231382c37382c3231392c3130322c3136332c3232382c3130382c35382c342c3135392c3133342c3138312c34372c32342c34372c32392c38332c3133342c35342c39362c39372c3232312c31362c34312c3135372c3139312c36372c34352c3130302c3137372c3231362c3234322c35382c3137372c3134322c35342c3137382c32392c3131372c3139332c3130382c3137365d5d2c22696e646578223a342c22686173686572223a6e756c6c7d7d2c7b227369676d61223a5b3134362c3235332c3139302c3130302c3231362c3133322c3231362c3132322c3231352c3232342c3232302c3135302c32322c3234392c36392c3131312c3134322c3230352c3230332c3233302c34342c34312c3133352c38352c3136332c34372c3231332c37302c3133332c33382c3230352c3132342c3233302c3139302c3233312c36392c3138322c3138352c32342c39382c3137392c3133302c33312c3131352c3233332c3232372c3132312c3131395d2c22706b223a5b3134332c3136312c3235352c34382c37382c35372c3230342c3232302c32352c3232312c3136342c3235322c3234382c31342c35362c3132362c3138362c3133352c3232382c3138382c3134352c3138312c35322c3230302c39372c39392c3231332c34362c302c3139392c3139332c38392c3138372c38382c32392c3133352c3137332c3234342c38362c33362c38332c35342c36372c3136342c362c3133372c39342c37322c362c3130352c3132382c3132382c39332c34382c3137362c31312c342c3234362c3133382c34382c3138302c3133332c39302c3134322c3139322c32342c3139332c3131312c3134322c33312c37362c3131312c3131302c3233342c3135332c39302c3230382c3139322c33312c3132342c39352c3130322c34392c3135382c39392c35322c3232302c3136352c39342c3235312c36382c36392c3132312c31362c3232342c3139345d2c227374616b65223a3832362c22696e646578223a31392c2270617468223a7b2276616c756573223a5b5b34372c3136332c3234362c3133342c3232332c3133352c3130352c3134392c32322c3132362c3132342c34362c39332c3131362c3139362c3139392c3138322c3232382c3134332c3132382c3130342c3235342c31342c36382c33322c3133312c36382c3231322c3132382c3234372c3134342c37362c35342c3135302c36322c36382c31372c39352c3232372c3233352c34322c35382c3230302c3130352c37362c34302c3138382c3138302c3234352c3136302c3234332c33392c3131312c34362c3132312c37322c3132352c3133302c32352c352c3132322c38302c3131302c37355d2c5b36342c38302c33332c3138382c33342c3234372c3230392c3137332c33332c32392c3135392c3235322c35392c3136392c3231392c3134372c3231392c3134332c3137392c32362c3139312c37332c34352c3234352c32382c3138332c3234372c3133342c312c3137342c3131382c3133352c3134352c3231322c3136332c342c36362c36382c35362c38382c3138392c37342c37312c382c32322c34312c3232352c3231352c3231342c3231342c3134392c3134372c36302c3137372c33352c3231372c3137352c3130382c3134372c3132392c3137352c3132372c35332c3135365d2c5b31362c38372c3134332c31352c3130382c3136342c3233312c3235332c32312c3133322c3134372c36382c3132392c39342c3231362c33362c3235332c37312c3230382c36322c3134382c3134332c3231382c37382c3231392c3130322c3136332c3232382c3130382c35382c342c3135392c3133342c3138312c34372c32342c34372c32392c38332c3133342c35342c39362c39372c3232312c31362c34312c3135372c3139312c36372c34352c3130302c3137372c3231362c3234322c35382c3137372c3134322c35342c3137382c32392c3131372c3139332c3130382c3137365d5d2c22696e646578223a342c22686173686572223a6e756c6c7d7d2c7b227369676d61223a5b3134362c3235332c3139302c3130302c3231362c3133322c3231362c3132322c3231352c3232342c3232302c3135302c32322c3234392c36392c3131312c3134322c3230352c3230332c3233302c34342c34312c3133352c38352c3136332c34372c3231332c37302c3133332c33382c3230352c3132342c3233302c3139302c3233312c36392c3138322c3138352c32342c39382c3137392c3133302c33312c3131352c3233332c3232372c3132312c3131395d2c22706b223a5b3134332c3136312c3235352c34382c37382c35372c3230342c3232302c32352c3232312c3136342c3235322c3234382c31342c35362c3132362c3138362c3133352c3232382c3138382c3134352c3138312c35322c3230302c39372c39392c3231332c34362c302c3139392c3139332c38392c3138372c38382c32392c3133352c3137332c3234342c38362c33362c38332c35342c36372c3136342c362c3133372c39342c37322c362c3130352c3132382c3132382c39332c34382c3137362c31312c342c3234362c3133382c34382c3138302c3133332c39302c3134322c3139322c32342c3139332c3131312c3134322c33312c37362c3131312c3131302c3233342c3135332c39302c3230382c3139322c33312c3132342c39352c3130322c34392c3135382c39392c35322c3232302c3136352c39342c3235312c36382c36392c3132312c31362c3232342c3139345d2c227374616b65223a3832362c22696e646578223a31332c2270617468223a7b2276616c756573223a5b5b34372c3136332c3234362c3133342c3232332c3133352c3130352c3134392c32322c3132362c3132342c34362c39332c3131362c3139362c3139392c3138322c3232382c3134332c3132382c3130342c3235342c31342c36382c33322c3133312c36382c3231322c3132382c3234372c3134342c37362c35342c3135302c36322c36382c31372c39352c3232372c3233352c34322c35382c3230302c3130352c37362c34302c3138382c3138302c3234352c3136302c3234332c33392c3131312c34362c3132312c37322c3132352c3133302c32352c352c3132322c38302c3131302c37355d2c5b36342c38302c33332c3138382c33342c3234372c3230392c3137332c33332c32392c3135392c3235322c35392c3136392c3231392c3134372c3231392c3134332c3137392c32362c3139312c37332c34352c3234352c32382c3138332c3234372c3133342c312c3137342c3131382c3133352c3134352c3231322c3136332c342c36362c36382c35362c38382c3138392c37342c37312c382c32322c34312c3232352c3231352c3231342c3231342c3134392c3134372c36302c3137372c33352c3231372c3137352c3130382c3134372c3132392c3137352c3132372c35332c3135365d2c5b31362c38372c3134332c31352c3130382c3136342c3233312c3235332c32312c3133322c3134372c36382c3132392c39342c3231362c33362c3235332c37312c3230382c36322c3134382c3134332c3231382c37382c3231392c3130322c3136332c3232382c3130382c35382c342c3135392c3133342c3138312c34372c32342c34372c32392c38332c3133342c35342c39362c39372c3232312c31362c34312c3135372c3139312c36372c34352c3130302c3137372c3231362c3234322c35382c3137372c3134322c35342c3137382c32392c3131372c3139332c3130382c3137365d5d2c22696e646578223a342c22686173686572223a6e756c6c7d7d2c7b227369676d61223a5b3134362c3235332c3139302c3130302c3231362c3133322c3231362c3132322c3231352c3232342c3232302c3135302c32322c3234392c36392c3131312c3134322c3230352c3230332c3233302c34342c34312c3133352c38352c3136332c34372c3231332c37302c3133332c33382c3230352c3132342c3233302c3139302c3233312c36392c3138322c3138352c32342c39382c3137392c3133302c33312c3131352c3233332c3232372c3132312c3131395d2c22706b223a5b3134332c3136312c3235352c34382c37382c35372c3230342c3232302c32352c3232312c3136342c3235322c3234382c31342c35362c3132362c3138362c3133352c3232382c3138382c3134352c3138312c35322c3230302c39372c39392c3231332c34362c302c3139392c3139332c38392c3138372c38382c32392c3133352c3137332c3234342c38362c33362c38332c35342c36372c3136342c362c3133372c39342c37322c362c3130352c3132382c3132382c39332c34382c3137362c31312c342c3234362c3133382c34382c3138302c3133332c39302c3134322c3139322c32342c3139332c3131312c3134322c33312c37362c3131312c3131302c3233342c3135332c39302c3230382c3139322c33312c3132342c39352c3130322c34392c3135382c39392c35322c3232302c3136352c39342c3235312c36382c36392c3132312c31362c3232342c3139345d2c227374616b65223a3832362c22696e646578223a31322c2270617468223a7b2276616c756573223a5b5b34372c3136332c3234362c3133342c3232332c3133352c3130352c3134392c32322c3132362c3132342c34362c39332c3131362c3139362c3139392c3138322c3232382c3134332c3132382c3130342c3235342c31342c36382c33322c3133312c36382c3231322c3132382c3234372c3134342c37362c35342c3135302c36322c36382c31372c39352c3232372c3233352c34322c35382c3230302c3130352c37362c34302c3138382c3138302c3234352c3136302c3234332c33392c3131312c34362c3132312c37322c3132352c3133302c32352c352c3132322c38302c3131302c37355d2c5b36342c38302c33332c3138382c33342c3234372c3230392c3137332c33332c32392c3135392c3235322c35392c3136392c3231392c3134372c3231392c3134332c3137392c32362c3139312c37332c34352c3234352c32382c3138332c3234372c3133342c312c3137342c3131382c3133352c3134352c3231322c3136332c342c36362c36382c35362c38382c3138392c37342c37312c382c32322c34312c3232352c3231352c3231342c3231342c3134392c3134372c36302c3137372c33352c3231372c3137352c3130382c3134372c3132392c3137352c3132372c35332c3135365d2c5b31362c38372c3134332c31352c3130382c3136342c3233312c3235332c32312c3133322c3134372c36382c3132392c39342c3231362c33362c3235332c37312c3230382c36322c3134382c3134332c3231382c37382c3231392c3130322c3136332c3232382c3130382c35382c342c3135392c3133342c3138312c34372c32342c34372c32392c38332c3133342c35342c39362c39372c3232312c31362c34312c3135372c3139312c36372c34352c3130302c3137372c3231362c3234322c35382c3137372c3134322c35342c3137382c32392c3131372c3139332c3130382c3137365d5d2c22696e646578223a342c22686173686572223a6e756c6c7d7d2c7b227369676d61223a5b3134362c3235332c3139302c3130302c3231362c3133322c3231362c3132322c3231352c3232342c3232302c3135302c32322c3234392c36392c3131312c3134322c3230352c3230332c3233302c34342c34312c3133352c38352c3136332c34372c3231332c37302c3133332c33382c3230352c3132342c3233302c3139302c3233312c36392c3138322c3138352c32342c39382c3137392c3133302c33312c3131352c3233332c3232372c3132312c3131395d2c22706b223a5b3134332c3136312c3235352c34382c37382c35372c3230342c3232302c32352c3232312c3136342c3235322c3234382c31342c35362c3132362c3138362c3133352c3232382c3138382c3134352c3138312c35322c3230302c39372c39392c3231332c34362c302c3139392c3139332c38392c3138372c38382c32392c3133352c3137332c3234342c38362c33362c38332c35342c36372c3136342c362c3133372c39342c37322c362c3130352c3132382c3132382c39332c34382c3137362c31312c342c3234362c3133382c34382c3138302c3133332c39302c3134322c3139322c32342c3139332c3131312c3134322c33312c37362c3131312c3131302c3233342c3135332c39302c3230382c3139322c33312c3132342c39352c3130322c34392c3135382c39392c35322c3232302c3136352c39342c3235312c36382c36392c3132312c31362c3232342c3139345d2c227374616b65223a3832362c22696e646578223a31362c2270617468223a7b2276616c756573223a5b5b34372c3136332c3234362c3133342c3232332c3133352c3130352c3134392c32322c3132362c3132342c34362c39332c3131362c3139362c3139392c3138322c3232382c3134332c3132382c3130342c3235342c31342c36382c33322c3133312c36382c3231322c3132382c3234372c3134342c37362c35342c3135302c36322c36382c31372c39352c3232372c3233352c34322c35382c3230302c3130352c37362c34302c3138382c3138302c3234352c3136302c3234332c33392c3131312c34362c3132312c37322c3132352c3133302c32352c352c3132322c38302c3131302c37355d2c5b36342c38302c33332c3138382c33342c3234372c3230392c3137332c33332c32392c3135392c3235322c35392c3136392c3231392c3134372c3231392c3134332c3137392c32362c3139312c37332c34352c3234352c32382c3138332c3234372c3133342c312c3137342c3131382c3133352c3134352c3231322c3136332c342c36362c36382c35362c38382c3138392c37342c37312c382c32322c34312c3232352c3231352c3231342c3231342c3134392c3134372c36302c3137372c33352c3231372c3137352c3130382c3134372c3132392c3137352c3132372c35332c3135365d2c5b31362c38372c3134332c31352c3130382c3136342c3233312c3235332c32312c3133322c3134372c36382c3132392c39342c3231362c33362c3235332c37312c3230382c36322c3134382c3134332c3231382c37382c3231392c3130322c3136332c3232382c3130382c35382c342c3135392c3133342c3138312c34372c32342c34372c32392c38332c3133342c35342c39362c39372c3232312c31362c34312c3135372c3139312c36372c34352c3130302c3137372c3231362c3234322c35382c3137372c3134322c35342c3137382c32392c3131372c3139332c3130382c3137365d5d2c22696e646578223a342c22686173686572223a6e756c6c7d7d2c7b227369676d61223a5b3134362c3235332c3139302c3130302c3231362c3133322c3231362c3132322c3231352c3232342c3232302c3135302c32322c3234392c36392c3131312c3134322c3230352c3230332c3233302c34342c34312c3133352c38352c3136332c34372c3231332c37302c3133332c33382c3230352c3132342c3233302c3139302c3233312c36392c3138322c3138352c32342c39382c3137392c3133302c33312c3131352c3233332c3232372c3132312c3131395d2c22706b223a5b3134332c3136312c3235352c34382c37382c35372c3230342c3232302c32352c3232312c3136342c3235322c3234382c31342c35362c3132362c3138362c3133352c3232382c3138382c3134352c3138312c35322c3230302c39372c39392c3231332c34362c302c3139392c3139332c38392c3138372c38382c32392c3133352c3137332c3234342c38362c33362c38332c35342c36372c3136342c362c3133372c39342c37322c362c3130352c3132382c3132382c39332c34382c3137362c31312c342c3234362c3133382c34382c3138302c3133332c39302c3134322c3139322c32342c3139332c3131312c3134322c33312c37362c3131312c3131302c3233342c3135332c39302c3230382c3139322c33312c3132342c39352c3130322c34392c3135382c39392c35322c3232302c3136352c39342c3235312c36382c36392c3132312c31362c3232342c3139345d2c227374616b65223a3832362c22696e646578223a32302c2270617468223a7b2276616c756573223a5b5b34372c3136332c3234362c3133342c3232332c3133352c3130352c3134392c32322c3132362c3132342c34362c39332c3131362c3139362c3139392c3138322c3232382c3134332c3132382c3130342c3235342c31342c36382c33322c3133312c36382c3231322c3132382c3234372c3134342c37362c35342c3135302c36322c36382c31372c39352c3232372c3233352c34322c35382c3230302c3130352c37362c34302c3138382c3138302c3234352c3136302c3234332c33392c3131312c34362c3132312c37322c3132352c3133302c32352c352c3132322c38302c3131302c37355d2c5b36342c38302c33332c3138382c33342c3234372c3230392c3137332c33332c32392c3135392c3235322c35392c3136392c3231392c3134372c3231392c3134332c3137392c32362c3139312c37332c34352c3234352c32382c3138332c3234372c3133342c312c3137342c3131382c3133352c3134352c3231322c3136332c342c36362c36382c35362c38382c3138392c37342c37312c382c32322c34312c3232352c3231352c3231342c3231342c3134392c3134372c36302c3137372c33352c3231372c3137352c3130382c3134372c3132392c3137352c3132372c35332c3135365d2c5b31362c38372c3134332c31352c3130382c3136342c3233312c3235332c32312c3133322c3134372c36382c3132392c39342c3231362c33362c3235332c37312c3230382c36322c3134382c3134332c3231382c37382c3231392c3130322c3136332c3232382c3130382c35382c342c3135392c3133342c3138312c34372c32342c34372c32392c38332c3133342c35342c39362c39372c3232312c31362c34312c3135372c3139312c36372c34352c3130302c3137372c3231362c3234322c35382c3137372c3134322c35342c3137382c32392c3131372c3139332c3130382c3137365d5d2c22696e646578223a342c22686173686572223a6e756c6c7d7d2c7b227369676d61223a5b3134362c3235332c3139302c3130302c3231362c3133322c3231362c3132322c3231352c3232342c3232302c3135302c32322c3234392c36392c3131312c3134322c3230352c3230332c3233302c34342c34312c3133352c38352c3136332c34372c3231332c37302c3133332c33382c3230352c3132342c3233302c3139302c3233312c36392c3138322c3138352c32342c39382c3137392c3133302c33312c3131352c3233332c3232372c3132312c3131395d2c22706b223a5b3134332c3136312c3235352c34382c37382c35372c3230342c3232302c32352c3232312c3136342c3235322c3234382c31342c35362c3132362c3138362c3133352c3232382c3138382c3134352c3138312c35322c3230302c39372c39392c3231332c34362c302c3139392c3139332c38392c3138372c38382c32392c3133352c3137332c3234342c38362c33362c38332c35342c36372c3136342c362c3133372c39342c37322c362c3130352c3132382c3132382c39332c34382c3137362c31312c342c3234362c3133382c34382c3138302c3133332c39302c3134322c3139322c32342c3139332c3131312c3134322c33312c37362c3131312c3131302c3233342c3135332c39302c3230382c3139322c33312c3132342c39352c3130322c34392c3135382c39392c35322c3232302c3136352c39342c3235312c36382c36392c3132312c31362c3232342c3139345d2c227374616b65223a3832362c22696e646578223a342c2270617468223a7b2276616c756573223a5b5b34372c3136332c3234362c3133342c3232332c3133352c3130352c3134392c32322c3132362c3132342c34362c39332c3131362c3139362c3139392c3138322c3232382c3134332c3132382c3130342c3235342c31342c36382c33322c3133312c36382c3231322c3132382c3234372c3134342c37362c35342c3135302c36322c36382c31372c39352c3232372c3233352c34322c35382c3230302c3130352c37362c34302c3138382c3138302c3234352c3136302c3234332c33392c3131312c34362c3132312c37322c3132352c3133302c32352c352c3132322c38302c3131302c37355d2c5b36342c38302c33332c3138382c33342c3234372c3230392c3137332c33332c32392c3135392c3235322c35392c3136392c3231392c3134372c3231392c3134332c3137392c32362c3139312c37332c34352c3234352c32382c3138332c3234372c3133342c312c3137342c3131382c3133352c3134352c3231322c3136332c342c36362c36382c35362c38382c3138392c37342c37312c382c32322c34312c3232352c3231352c3231342c3231342c3134392c3134372c36302c3137372c33352c3231372c3137352c3130382c3134372c3132392c3137352c3132372c35332c3135365d2c5b31362c38372c3134332c31352c3130382c3136342c3233312c3235332c32312c3133322c3134372c36382c3132392c39342c3231362c33362c3235332c37312c3230382c36322c3134382c3134332c3231382c37382c3231392c3130322c3136332c3232382c3130382c35382c342c3135392c3133342c3138312c34372c32342c34372c32392c38332c3133342c35342c39362c39372c3232312c31362c34312c3135372c3139312c36372c34352c3130302c3137372c3231362c3234322c35382c3137372c3134322c35342c3137382c32392c3131372c3139332c3130382c3137365d5d2c22696e646578223a342c22686173686572223a6e756c6c7d7d2c7b227369676d61223a5b3134362c3235332c3139302c3130302c3231362c3133322c3231362c3132322c3231352c3232342c3232302c3135302c32322c3234392c36392c3131312c3134322c3230352c3230332c3233302c34342c34312c3133352c38352c3136332c34372c3231332c37302c3133332c33382c3230352c3132342c3233302c3139302c3233312c36392c3138322c3138352c32342c39382c3137392c3133302c33312c3131352c3233332c3232372c3132312c3131395d2c22706b223a5b3134332c3136312c3235352c34382c37382c35372c3230342c3232302c32352c3232312c3136342c3235322c3234382c31342c35362c3132362c3138362c3133352c3232382c3138382c3134352c3138312c35322c3230302c39372c39392c3231332c34362c302c3139392c3139332c38392c3138372c38382c32392c3133352c3137332c3234342c38362c33362c38332c35342c36372c3136342c362c3133372c39342c37322c362c3130352c3132382c3132382c39332c34382c3137362c31312c342c3234362c3133382c34382c3138302c3133332c39302c3134322c3139322c32342c3139332c3131312c3134322c33312c37362c3131312c3131302c3233342c3135332c39302c3230382c3139322c33312c3132342c39352c3130322c34392c3135382c39392c35322c3232302c3136352c39342c3235312c36382c36392c3132312c31362c3232342c3139345d2c227374616b65223a3832362c22696e646578223a32342c2270617468223a7b2276616c756573223a5b5b34372c3136332c3234362c3133342c3232332c3133352c3130352c3134392c32322c3132362c3132342c34362c39332c3131362c3139362c3139392c3138322c3232382c3134332c3132382c3130342c3235342c31342c36382c33322c3133312c36382c3231322c3132382c3234372c3134342c37362c35342c3135302c36322c36382c31372c39352c3232372c3233352c34322c35382c3230302c3130352c37362c34302c3138382c3138302c3234352c3136302c3234332c33392c3131312c34362c3132312c37322c3132352c3133302c32352c352c3132322c38302c3131302c37355d2c5b36342c38302c33332c3138382c33342c3234372c3230392c3137332c33332c32392c3135392c3235322c35392c3136392c3231392c3134372c3231392c3134332c3137392c32362c3139312c37332c34352c3234352c32382c3138332c3234372c3133342c312c3137342c3131382c3133352c3134352c3231322c3136332c342c36362c36382c35362c38382c3138392c37342c37312c382c32322c34312c3232352c3231352c3231342c3231342c3134392c3134372c36302c3137372c33352c3231372c3137352c3130382c3134372c3132392c3137352c3132372c35332c3135365d2c5b31362c38372c3134332c31352c3130382c3136342c3233312c3235332c32312c3133322c3134372c36382c3132392c39342c3231362c33362c3235332c37312c3230382c36322c3134382c3134332c3231382c37382c3231392c3130322c3136332c3232382c3130382c35382c342c3135392c3133342c3138312c34372c32342c34372c32392c38332c3133342c35342c39362c39372c3232312c31362c34312c3135372c3139312c36372c34352c3130302c3137372c3231362c3234322c35382c3137372c3134322c35342c3137382c32392c3131372c3139332c3130382c3137365d5d2c22696e646578223a342c22686173686572223a6e756c6c7d7d2c7b227369676d61223a5b3134362c3235332c3139302c3130302c3231362c3133322c3231362c3132322c3231352c3232342c3232302c3135302c32322c3234392c36392c3131312c3134322c3230352c3230332c3233302c34342c34312c3133352c38352c3136332c34372c3231332c37302c3133332c33382c3230352c3132342c3233302c3139302c3233312c36392c3138322c3138352c32342c39382c3137392c3133302c33312c3131352c3233332c3232372c3132312c3131395d2c22706b223a5b3134332c3136312c3235352c34382c37382c35372c3230342c3232302c32352c3232312c3136342c3235322c3234382c31342c35362c3132362c3138362c3133352c3232382c3138382c3134352c3138312c35322c3230302c39372c39392c3231332c34362c302c3139392c3139332c38392c3138372c38382c32392c3133352c3137332c3234342c38362c33362c38332c35342c36372c3136342c362c3133372c39342c37322c362c3130352c3132382c3132382c39332c34382c3137362c31312c342c3234362c3133382c34382c3138302c3133332c39302c3134322c3139322c32342c3139332c3131312c3134322c33312c37362c3131312c3131302c3233342c3135332c39302c3230382c3139322c33312c3132342c39352c3130322c34392c3135382c39392c35322c3232302c3136352c39342c3235312c36382c36392c3132312c31362c3232342c3139345d2c227374616b65223a3832362c22696e646578223a32382c2270617468223a7b2276616c756573223a5b5b34372c3136332c3234362c3133342c3232332c3133352c3130352c3134392c32322c3132362c3132342c34362c39332c3131362c3139362c3139392c3138322c3232382c3134332c3132382c3130342c3235342c31342c36382c33322c3133312c36382c3231322c3132382c3234372c3134342c37362c35342c3135302c36322c36382c31372c39352c3232372c3233352c34322c35382c3230302c3130352c37362c34302c3138382c3138302c3234352c3136302c3234332c33392c3131312c34362c3132312c37322c3132352c3133302c32352c352c3132322c38302c3131302c37355d2c5b36342c38302c33332c3138382c33342c3234372c3230392c3137332c33332c32392c3135392c3235322c35392c3136392c3231392c3134372c3231392c3134332c3137392c32362c3139312c37332c34352c3234352c32382c3138332c3234372c3133342c312c3137342c3131382c3133352c3134352c3231322c3136332c342c36362c36382c35362c38382c3138392c37342c37312c382c32322c34312c3232352c3231352c3231342c3231342c3134392c3134372c36302c3137372c33352c3231372c3137352c3130382c3134372c3132392c3137352c3132372c35332c3135365d2c5b31362c38372c3134332c31352c3130382c3136342c3233312c3235332c32312c3133322c3134372c36382c3132392c39342c3231362c33362c3235332c37312c3230382c36322c3134382c3134332c3231382c37382c3231392c3130322c3136332c3232382c3130382c35382c342c3135392c3133342c3138312c34372c32342c34372c32392c38332c3133342c35342c39362c39372c3232312c31362c34312c3135372c3139312c36372c34352c3130302c3137372c3231362c3234322c35382c3137372c3134322c35342c3137382c32392c3131372c3139332c3130382c3137365d5d2c22696e646578223a342c22686173686572223a6e756c6c7d7d5d7d".to_string();
        let mut beacon_store = MockBeaconStore::new();
        beacon_store
            .expect_get_current_beacon()
            .return_once(|| Ok(Some(fake_data::beacon())));
        let mut mock_multi_signer = MockMultiSigner::new();
        mock_multi_signer
            .expect_get_multi_signature()
            .return_once(move |_| Ok(Some(key_decode_hex(&multi_signature_hex).unwrap())));
        mock_multi_signer
            .expect_get_stake_distribution()
            .return_once(|| Vec::new());
        let mut dependency_manager = setup_dependency_manager();
        dependency_manager
            .with_multi_signer(Arc::new(RwLock::new(mock_multi_signer)))
            .with_beacon_store(Arc::new(RwLock::new(beacon_store)));

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
        let mut beacon_store = MockBeaconStore::new();
        beacon_store
            .expect_get_current_beacon()
            .return_once(|| Ok(Some(fake_data::beacon())));
        let mut mock_multi_signer = MockMultiSigner::new();
        mock_multi_signer
            .expect_get_multi_signature()
            .return_once(|_| Ok(None));
        let mut dependency_manager = setup_dependency_manager();
        dependency_manager
            .with_multi_signer(Arc::new(RwLock::new(mock_multi_signer)))
            .with_beacon_store(Arc::new(RwLock::new(beacon_store)));

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
        let mut beacon_store = MockBeaconStore::new();
        beacon_store
            .expect_get_current_beacon()
            .return_once(|| Ok(None));
        let mut mock_multi_signer = MockMultiSigner::new();
        mock_multi_signer
            .expect_get_multi_signature()
            .return_once(|_| Err(ProtocolError::Codec("an error occurred".to_string())));
        let mut dependency_manager = setup_dependency_manager();
        dependency_manager
            .with_multi_signer(Arc::new(RwLock::new(mock_multi_signer)))
            .with_beacon_store(Arc::new(RwLock::new(beacon_store)));

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
        let mut mock_snapshot_storer = MockSnapshotStore::new();
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
        let mut mock_snapshot_storer = MockSnapshotStore::new();
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
        let mut mock_snapshot_storer = MockSnapshotStore::new();
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
        let mut mock_snapshot_storer = MockSnapshotStore::new();
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
        let mut mock_snapshot_storer = MockSnapshotStore::new();
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
