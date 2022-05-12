use chrono::prelude::*;
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

use super::dependency::{DependencyManager, MultiSignerWrapper, SnapshotStorerWrapper};
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
            .and(with_multi_signer(dependency_manager))
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
    pub async fn certificate_pending(
        multi_signer: MultiSignerWrapper,
    ) -> Result<impl warp::Reply, Infallible> {
        debug!("certificate_pending");

        let certificate_pending_fake = fake_data::certificate_pending();
        let multi_signer = multi_signer.read().await;

        let beacon = certificate_pending_fake.beacon;

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

        let previous_hash = certificate_pending_fake.previous_hash;

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

        let certificate_pending =
            entities::CertificatePending::new(beacon, protocol_parameters, previous_hash, signers);

        Ok(warp::reply::with_status(
            warp::reply::json(&certificate_pending),
            StatusCode::OK,
        ))
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
        let message = fake_data::digest();
        multi_signer.update_current_message(message).unwrap();
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
    use mithril_common::crypto_helper::key_decode_hex;
    use mithril_common::fake_data;
    use serde_json::Value::Null;
    use tokio::sync::RwLock;
    use warp::test::request;

    use crate::multi_signer::ProtocolError;

    use super::super::entities::*;
    use super::super::multi_signer::MockMultiSigner;
    use super::super::snapshot_store::MockSnapshotStorer;
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
        let mut mock_multi_signer = MockMultiSigner::new();
        mock_multi_signer
            .expect_get_protocol_parameters()
            .return_once(|| Some(fake_protocol_parameters.into()));
        mock_multi_signer
            .expect_get_signers()
            .return_once(|| Ok(fake_signers_with_stakes));
        let mut dependency_manager = setup_dependency_manager();
        dependency_manager.with_multi_signer(Arc::new(RwLock::new(mock_multi_signer)));

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
        let mut mock_multi_signer = MockMultiSigner::new();
        mock_multi_signer
            .expect_get_protocol_parameters()
            .return_once(|| None);
        let mut dependency_manager = setup_dependency_manager();
        dependency_manager.with_multi_signer(Arc::new(RwLock::new(mock_multi_signer)));

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
    async fn test_certificate_pending_get_ko_signers_500() {
        let fake_protocol_parameters = fake_data::protocol_parameters();
        let mut mock_multi_signer = MockMultiSigner::new();
        mock_multi_signer
            .expect_get_protocol_parameters()
            .return_once(|| Some(fake_protocol_parameters.into()));
        mock_multi_signer
            .expect_get_signers()
            .return_once(|| Err(ProtocolError::Codec("an error occurred".to_string())));
        let mut dependency_manager = setup_dependency_manager();
        dependency_manager.with_multi_signer(Arc::new(RwLock::new(mock_multi_signer)));

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
        let multi_signature_hex = "7b227369676e617475726573223a5b7b227369676d61223a5b3137312c3139352c3234322c3235332c3137392c32352c3138372c3135312c3132322c3133302c3230372c3132322c38342c3132352c3134322c3132332c3233352c3134312c3230362c3136392c382c3136302c3138382c36382c35312c3232302c3232342c3231312c3137312c3230372c3231362c3139332c3230352c3139312c3233372c3131312c3232392c3132392c3131392c36362c3134342c3234382c3235322c39322c3234372c35382c37312c39355d2c22706b223a7b22766b223a5b3134332c3136312c3235352c34382c37382c35372c3230342c3232302c32352c3232312c3136342c3235322c3234382c31342c35362c3132362c3138362c3133352c3232382c3138382c3134352c3138312c35322c3230302c39372c39392c3231332c34362c302c3139392c3139332c38392c3138372c38382c32392c3133352c3137332c3234342c38362c33362c38332c35342c36372c3136342c362c3133372c39342c37322c362c3130352c3132382c3132382c39332c34382c3137362c31312c342c3234362c3133382c34382c3138302c3133332c39302c3134322c3139322c32342c3139332c3131312c3134322c33312c37362c3131312c3131302c3233342c3135332c39302c3230382c3139322c33312c3132342c39352c3130322c34392c3135382c39392c35322c3232302c3136352c39342c3235312c36382c36392c3132312c31362c3232342c3139345d2c22706f70223a5b3136382c35302c3233332c3139332c31352c3133362c36352c37322c3132332c3134382c3132392c3137362c33382c3139382c3230392c34372c32382c3230342c3137362c3134342c35372c3235312c34322c32382c36362c37362c38392c39372c3135382c36332c35342c3139382c3139342c3137362c3133352c3232312c31342c3138352c3139372c3232352c3230322c39382c3234332c37342c3233332c3232352c3134332c3135312c3134372c3137372c3137302c3131372c36362c3136352c36362c36322c33332c3231362c3233322c37352c36382c3131342c3139352c32322c3130302c36352c34342c3139382c342c3136362c3130322c3233332c3235332c3234302c35392c3137352c36302c3131372c3134322c3131342c3134302c3132322c31372c38372c3131302c3138372c312c31372c31302c3139352c3135342c31332c3234392c38362c35342c3232365d7d2c227061727479223a302c227374616b65223a3832362c22696e646578223a33302c2270617468223a7b2276616c756573223a5b5b3235332c3135382c3135382c3232322c3233352c3137302c3137362c3139392c33332c3230302c36362c32362c3231312c3137392c3132362c3232362c35352c3234302c3138322c33302c3234362c3231352c37362c3135382c31362c3131342c342c3231392c36322c3131352c3235332c322c3139322c3231392c3135322c3137352c3131322c34352c36392c3131322c36382c3139352c31372c34342c3230352c3230342c37382c3233342c3130362c3234362c3230392c33312c3230302c3137312c3130382c32372c3136352c35382c3232392c37342c35382c3139312c3132352c3231385d2c5b3130382c39382c35322c3137372c3131332c3132392c3139342c37312c3133352c3137342c37342c3230352c38392c3137352c312c3230382c3234362c3136312c3132322c3233312c33302c3137382c32362c3234312c39382c35322c3133322c31342c33372c302c3138312c3232342c3130332c34362c3130312c3232322c3139392c36312c3231372c31322c39322c3231362c3139302c3131352c3233362c3134322c3138322c3235332c38312c32352c3138392c342c3235302c35382c34352c3234332c38302c37332c3130322c38332c32342c3130392c3131312c3138305d2c5b3138302c3230382c3234392c35312c3231362c3133352c34342c3134342c372c3132392c36302c36332c3234342c3130342c33362c3232392c34332c31312c3132332c38362c3131352c3131322c3138342c3230382c3135392c3138352c37332c31302c3136392c32372c39362c3231382c39392c3135322c3138312c36362c3230312c36302c3135342c31342c3231312c39342c3232392c3135382c3230382c3136362c3233302c37362c32332c3131382c3137382c3230382c38372c3131372c3233302c31392c3233312c32392c3230362c35382c3232352c32322c39352c3130335d5d2c22696e646578223a302c22686173686572223a6e756c6c7d7d2c7b227369676d61223a5b3137312c3139352c3234322c3235332c3137392c32352c3138372c3135312c3132322c3133302c3230372c3132322c38342c3132352c3134322c3132332c3233352c3134312c3230362c3136392c382c3136302c3138382c36382c35312c3232302c3232342c3231312c3137312c3230372c3231362c3139332c3230352c3139312c3233372c3131312c3232392c3132392c3131392c36362c3134342c3234382c3235322c39322c3234372c35382c37312c39355d2c22706b223a7b22766b223a5b3134332c3136312c3235352c34382c37382c35372c3230342c3232302c32352c3232312c3136342c3235322c3234382c31342c35362c3132362c3138362c3133352c3232382c3138382c3134352c3138312c35322c3230302c39372c39392c3231332c34362c302c3139392c3139332c38392c3138372c38382c32392c3133352c3137332c3234342c38362c33362c38332c35342c36372c3136342c362c3133372c39342c37322c362c3130352c3132382c3132382c39332c34382c3137362c31312c342c3234362c3133382c34382c3138302c3133332c39302c3134322c3139322c32342c3139332c3131312c3134322c33312c37362c3131312c3131302c3233342c3135332c39302c3230382c3139322c33312c3132342c39352c3130322c34392c3135382c39392c35322c3232302c3136352c39342c3235312c36382c36392c3132312c31362c3232342c3139345d2c22706f70223a5b3136382c35302c3233332c3139332c31352c3133362c36352c37322c3132332c3134382c3132392c3137362c33382c3139382c3230392c34372c32382c3230342c3137362c3134342c35372c3235312c34322c32382c36362c37362c38392c39372c3135382c36332c35342c3139382c3139342c3137362c3133352c3232312c31342c3138352c3139372c3232352c3230322c39382c3234332c37342c3233332c3232352c3134332c3135312c3134372c3137372c3137302c3131372c36362c3136352c36362c36322c33332c3231362c3233322c37352c36382c3131342c3139352c32322c3130302c36352c34342c3139382c342c3136362c3130322c3233332c3235332c3234302c35392c3137352c36302c3131372c3134322c3131342c3134302c3132322c31372c38372c3131302c3138372c312c31372c31302c3139352c3135342c31332c3234392c38362c35342c3232365d7d2c227061727479223a302c227374616b65223a3832362c22696e646578223a312c2270617468223a7b2276616c756573223a5b5b3235332c3135382c3135382c3232322c3233352c3137302c3137362c3139392c33332c3230302c36362c32362c3231312c3137392c3132362c3232362c35352c3234302c3138322c33302c3234362c3231352c37362c3135382c31362c3131342c342c3231392c36322c3131352c3235332c322c3139322c3231392c3135322c3137352c3131322c34352c36392c3131322c36382c3139352c31372c34342c3230352c3230342c37382c3233342c3130362c3234362c3230392c33312c3230302c3137312c3130382c32372c3136352c35382c3232392c37342c35382c3139312c3132352c3231385d2c5b3130382c39382c35322c3137372c3131332c3132392c3139342c37312c3133352c3137342c37342c3230352c38392c3137352c312c3230382c3234362c3136312c3132322c3233312c33302c3137382c32362c3234312c39382c35322c3133322c31342c33372c302c3138312c3232342c3130332c34362c3130312c3232322c3139392c36312c3231372c31322c39322c3231362c3139302c3131352c3233362c3134322c3138322c3235332c38312c32352c3138392c342c3235302c35382c34352c3234332c38302c37332c3130322c38332c32342c3130392c3131312c3138305d2c5b3138302c3230382c3234392c35312c3231362c3133352c34342c3134342c372c3132392c36302c36332c3234342c3130342c33362c3232392c34332c31312c3132332c38362c3131352c3131322c3138342c3230382c3135392c3138352c37332c31302c3136392c32372c39362c3231382c39392c3135322c3138312c36362c3230312c36302c3135342c31342c3231312c39342c3232392c3135382c3230382c3136362c3233302c37362c32332c3131382c3137382c3230382c38372c3131372c3233302c31392c3233312c32392c3230362c35382c3232352c32322c39352c3130335d5d2c22696e646578223a302c22686173686572223a6e756c6c7d7d2c7b227369676d61223a5b3137312c3139352c3234322c3235332c3137392c32352c3138372c3135312c3132322c3133302c3230372c3132322c38342c3132352c3134322c3132332c3233352c3134312c3230362c3136392c382c3136302c3138382c36382c35312c3232302c3232342c3231312c3137312c3230372c3231362c3139332c3230352c3139312c3233372c3131312c3232392c3132392c3131392c36362c3134342c3234382c3235322c39322c3234372c35382c37312c39355d2c22706b223a7b22766b223a5b3134332c3136312c3235352c34382c37382c35372c3230342c3232302c32352c3232312c3136342c3235322c3234382c31342c35362c3132362c3138362c3133352c3232382c3138382c3134352c3138312c35322c3230302c39372c39392c3231332c34362c302c3139392c3139332c38392c3138372c38382c32392c3133352c3137332c3234342c38362c33362c38332c35342c36372c3136342c362c3133372c39342c37322c362c3130352c3132382c3132382c39332c34382c3137362c31312c342c3234362c3133382c34382c3138302c3133332c39302c3134322c3139322c32342c3139332c3131312c3134322c33312c37362c3131312c3131302c3233342c3135332c39302c3230382c3139322c33312c3132342c39352c3130322c34392c3135382c39392c35322c3232302c3136352c39342c3235312c36382c36392c3132312c31362c3232342c3139345d2c22706f70223a5b3136382c35302c3233332c3139332c31352c3133362c36352c37322c3132332c3134382c3132392c3137362c33382c3139382c3230392c34372c32382c3230342c3137362c3134342c35372c3235312c34322c32382c36362c37362c38392c39372c3135382c36332c35342c3139382c3139342c3137362c3133352c3232312c31342c3138352c3139372c3232352c3230322c39382c3234332c37342c3233332c3232352c3134332c3135312c3134372c3137372c3137302c3131372c36362c3136352c36362c36322c33332c3231362c3233322c37352c36382c3131342c3139352c32322c3130302c36352c34342c3139382c342c3136362c3130322c3233332c3235332c3234302c35392c3137352c36302c3131372c3134322c3131342c3134302c3132322c31372c38372c3131302c3138372c312c31372c31302c3139352c3135342c31332c3234392c38362c35342c3232365d7d2c227061727479223a302c227374616b65223a3832362c22696e646578223a33322c2270617468223a7b2276616c756573223a5b5b3235332c3135382c3135382c3232322c3233352c3137302c3137362c3139392c33332c3230302c36362c32362c3231312c3137392c3132362c3232362c35352c3234302c3138322c33302c3234362c3231352c37362c3135382c31362c3131342c342c3231392c36322c3131352c3235332c322c3139322c3231392c3135322c3137352c3131322c34352c36392c3131322c36382c3139352c31372c34342c3230352c3230342c37382c3233342c3130362c3234362c3230392c33312c3230302c3137312c3130382c32372c3136352c35382c3232392c37342c35382c3139312c3132352c3231385d2c5b3130382c39382c35322c3137372c3131332c3132392c3139342c37312c3133352c3137342c37342c3230352c38392c3137352c312c3230382c3234362c3136312c3132322c3233312c33302c3137382c32362c3234312c39382c35322c3133322c31342c33372c302c3138312c3232342c3130332c34362c3130312c3232322c3139392c36312c3231372c31322c39322c3231362c3139302c3131352c3233362c3134322c3138322c3235332c38312c32352c3138392c342c3235302c35382c34352c3234332c38302c37332c3130322c38332c32342c3130392c3131312c3138305d2c5b3138302c3230382c3234392c35312c3231362c3133352c34342c3134342c372c3132392c36302c36332c3234342c3130342c33362c3232392c34332c31312c3132332c38362c3131352c3131322c3138342c3230382c3135392c3138352c37332c31302c3136392c32372c39362c3231382c39392c3135322c3138312c36362c3230312c36302c3135342c31342c3231312c39342c3232392c3135382c3230382c3136362c3233302c37362c32332c3131382c3137382c3230382c38372c3131372c3233302c31392c3233312c32392c3230362c35382c3232352c32322c39352c3130335d5d2c22696e646578223a302c22686173686572223a6e756c6c7d7d2c7b227369676d61223a5b3137312c3139352c3234322c3235332c3137392c32352c3138372c3135312c3132322c3133302c3230372c3132322c38342c3132352c3134322c3132332c3233352c3134312c3230362c3136392c382c3136302c3138382c36382c35312c3232302c3232342c3231312c3137312c3230372c3231362c3139332c3230352c3139312c3233372c3131312c3232392c3132392c3131392c36362c3134342c3234382c3235322c39322c3234372c35382c37312c39355d2c22706b223a7b22766b223a5b3134332c3136312c3235352c34382c37382c35372c3230342c3232302c32352c3232312c3136342c3235322c3234382c31342c35362c3132362c3138362c3133352c3232382c3138382c3134352c3138312c35322c3230302c39372c39392c3231332c34362c302c3139392c3139332c38392c3138372c38382c32392c3133352c3137332c3234342c38362c33362c38332c35342c36372c3136342c362c3133372c39342c37322c362c3130352c3132382c3132382c39332c34382c3137362c31312c342c3234362c3133382c34382c3138302c3133332c39302c3134322c3139322c32342c3139332c3131312c3134322c33312c37362c3131312c3131302c3233342c3135332c39302c3230382c3139322c33312c3132342c39352c3130322c34392c3135382c39392c35322c3232302c3136352c39342c3235312c36382c36392c3132312c31362c3232342c3139345d2c22706f70223a5b3136382c35302c3233332c3139332c31352c3133362c36352c37322c3132332c3134382c3132392c3137362c33382c3139382c3230392c34372c32382c3230342c3137362c3134342c35372c3235312c34322c32382c36362c37362c38392c39372c3135382c36332c35342c3139382c3139342c3137362c3133352c3232312c31342c3138352c3139372c3232352c3230322c39382c3234332c37342c3233332c3232352c3134332c3135312c3134372c3137372c3137302c3131372c36362c3136352c36362c36322c33332c3231362c3233322c37352c36382c3131342c3139352c32322c3130302c36352c34342c3139382c342c3136362c3130322c3233332c3235332c3234302c35392c3137352c36302c3131372c3134322c3131342c3134302c3132322c31372c38372c3131302c3138372c312c31372c31302c3139352c3135342c31332c3234392c38362c35342c3232365d7d2c227061727479223a302c227374616b65223a3832362c22696e646578223a332c2270617468223a7b2276616c756573223a5b5b3235332c3135382c3135382c3232322c3233352c3137302c3137362c3139392c33332c3230302c36362c32362c3231312c3137392c3132362c3232362c35352c3234302c3138322c33302c3234362c3231352c37362c3135382c31362c3131342c342c3231392c36322c3131352c3235332c322c3139322c3231392c3135322c3137352c3131322c34352c36392c3131322c36382c3139352c31372c34342c3230352c3230342c37382c3233342c3130362c3234362c3230392c33312c3230302c3137312c3130382c32372c3136352c35382c3232392c37342c35382c3139312c3132352c3231385d2c5b3130382c39382c35322c3137372c3131332c3132392c3139342c37312c3133352c3137342c37342c3230352c38392c3137352c312c3230382c3234362c3136312c3132322c3233312c33302c3137382c32362c3234312c39382c35322c3133322c31342c33372c302c3138312c3232342c3130332c34362c3130312c3232322c3139392c36312c3231372c31322c39322c3231362c3139302c3131352c3233362c3134322c3138322c3235332c38312c32352c3138392c342c3235302c35382c34352c3234332c38302c37332c3130322c38332c32342c3130392c3131312c3138305d2c5b3138302c3230382c3234392c35312c3231362c3133352c34342c3134342c372c3132392c36302c36332c3234342c3130342c33362c3232392c34332c31312c3132332c38362c3131352c3131322c3138342c3230382c3135392c3138352c37332c31302c3136392c32372c39362c3231382c39392c3135322c3138312c36362c3230312c36302c3135342c31342c3231312c39342c3232392c3135382c3230382c3136362c3233302c37362c32332c3131382c3137382c3230382c38372c3131372c3233302c31392c3233312c32392c3230362c35382c3232352c32322c39352c3130335d5d2c22696e646578223a302c22686173686572223a6e756c6c7d7d2c7b227369676d61223a5b3137312c3139352c3234322c3235332c3137392c32352c3138372c3135312c3132322c3133302c3230372c3132322c38342c3132352c3134322c3132332c3233352c3134312c3230362c3136392c382c3136302c3138382c36382c35312c3232302c3232342c3231312c3137312c3230372c3231362c3139332c3230352c3139312c3233372c3131312c3232392c3132392c3131392c36362c3134342c3234382c3235322c39322c3234372c35382c37312c39355d2c22706b223a7b22766b223a5b3134332c3136312c3235352c34382c37382c35372c3230342c3232302c32352c3232312c3136342c3235322c3234382c31342c35362c3132362c3138362c3133352c3232382c3138382c3134352c3138312c35322c3230302c39372c39392c3231332c34362c302c3139392c3139332c38392c3138372c38382c32392c3133352c3137332c3234342c38362c33362c38332c35342c36372c3136342c362c3133372c39342c37322c362c3130352c3132382c3132382c39332c34382c3137362c31312c342c3234362c3133382c34382c3138302c3133332c39302c3134322c3139322c32342c3139332c3131312c3134322c33312c37362c3131312c3131302c3233342c3135332c39302c3230382c3139322c33312c3132342c39352c3130322c34392c3135382c39392c35322c3232302c3136352c39342c3235312c36382c36392c3132312c31362c3232342c3139345d2c22706f70223a5b3136382c35302c3233332c3139332c31352c3133362c36352c37322c3132332c3134382c3132392c3137362c33382c3139382c3230392c34372c32382c3230342c3137362c3134342c35372c3235312c34322c32382c36362c37362c38392c39372c3135382c36332c35342c3139382c3139342c3137362c3133352c3232312c31342c3138352c3139372c3232352c3230322c39382c3234332c37342c3233332c3232352c3134332c3135312c3134372c3137372c3137302c3131372c36362c3136352c36362c36322c33332c3231362c3233322c37352c36382c3131342c3139352c32322c3130302c36352c34342c3139382c342c3136362c3130322c3233332c3235332c3234302c35392c3137352c36302c3131372c3134322c3131342c3134302c3132322c31372c38372c3131302c3138372c312c31372c31302c3139352c3135342c31332c3234392c38362c35342c3232365d7d2c227061727479223a302c227374616b65223a3832362c22696e646578223a352c2270617468223a7b2276616c756573223a5b5b3235332c3135382c3135382c3232322c3233352c3137302c3137362c3139392c33332c3230302c36362c32362c3231312c3137392c3132362c3232362c35352c3234302c3138322c33302c3234362c3231352c37362c3135382c31362c3131342c342c3231392c36322c3131352c3235332c322c3139322c3231392c3135322c3137352c3131322c34352c36392c3131322c36382c3139352c31372c34342c3230352c3230342c37382c3233342c3130362c3234362c3230392c33312c3230302c3137312c3130382c32372c3136352c35382c3232392c37342c35382c3139312c3132352c3231385d2c5b3130382c39382c35322c3137372c3131332c3132392c3139342c37312c3133352c3137342c37342c3230352c38392c3137352c312c3230382c3234362c3136312c3132322c3233312c33302c3137382c32362c3234312c39382c35322c3133322c31342c33372c302c3138312c3232342c3130332c34362c3130312c3232322c3139392c36312c3231372c31322c39322c3231362c3139302c3131352c3233362c3134322c3138322c3235332c38312c32352c3138392c342c3235302c35382c34352c3234332c38302c37332c3130322c38332c32342c3130392c3131312c3138305d2c5b3138302c3230382c3234392c35312c3231362c3133352c34342c3134342c372c3132392c36302c36332c3234342c3130342c33362c3232392c34332c31312c3132332c38362c3131352c3131322c3138342c3230382c3135392c3138352c37332c31302c3136392c32372c39362c3231382c39392c3135322c3138312c36362c3230312c36302c3135342c31342c3231312c39342c3232392c3135382c3230382c3136362c3233302c37362c32332c3131382c3137382c3230382c38372c3131372c3233302c31392c3233312c32392c3230362c35382c3232352c32322c39352c3130335d5d2c22696e646578223a302c22686173686572223a6e756c6c7d7d2c7b227369676d61223a5b3137312c3139352c3234322c3235332c3137392c32352c3138372c3135312c3132322c3133302c3230372c3132322c38342c3132352c3134322c3132332c3233352c3134312c3230362c3136392c382c3136302c3138382c36382c35312c3232302c3232342c3231312c3137312c3230372c3231362c3139332c3230352c3139312c3233372c3131312c3232392c3132392c3131392c36362c3134342c3234382c3235322c39322c3234372c35382c37312c39355d2c22706b223a7b22766b223a5b3134332c3136312c3235352c34382c37382c35372c3230342c3232302c32352c3232312c3136342c3235322c3234382c31342c35362c3132362c3138362c3133352c3232382c3138382c3134352c3138312c35322c3230302c39372c39392c3231332c34362c302c3139392c3139332c38392c3138372c38382c32392c3133352c3137332c3234342c38362c33362c38332c35342c36372c3136342c362c3133372c39342c37322c362c3130352c3132382c3132382c39332c34382c3137362c31312c342c3234362c3133382c34382c3138302c3133332c39302c3134322c3139322c32342c3139332c3131312c3134322c33312c37362c3131312c3131302c3233342c3135332c39302c3230382c3139322c33312c3132342c39352c3130322c34392c3135382c39392c35322c3232302c3136352c39342c3235312c36382c36392c3132312c31362c3232342c3139345d2c22706f70223a5b3136382c35302c3233332c3139332c31352c3133362c36352c37322c3132332c3134382c3132392c3137362c33382c3139382c3230392c34372c32382c3230342c3137362c3134342c35372c3235312c34322c32382c36362c37362c38392c39372c3135382c36332c35342c3139382c3139342c3137362c3133352c3232312c31342c3138352c3139372c3232352c3230322c39382c3234332c37342c3233332c3232352c3134332c3135312c3134372c3137372c3137302c3131372c36362c3136352c36362c36322c33332c3231362c3233322c37352c36382c3131342c3139352c32322c3130302c36352c34342c3139382c342c3136362c3130322c3233332c3235332c3234302c35392c3137352c36302c3131372c3134322c3131342c3134302c3132322c31372c38372c3131302c3138372c312c31372c31302c3139352c3135342c31332c3234392c38362c35342c3232365d7d2c227061727479223a302c227374616b65223a3832362c22696e646578223a35342c2270617468223a7b2276616c756573223a5b5b3235332c3135382c3135382c3232322c3233352c3137302c3137362c3139392c33332c3230302c36362c32362c3231312c3137392c3132362c3232362c35352c3234302c3138322c33302c3234362c3231352c37362c3135382c31362c3131342c342c3231392c36322c3131352c3235332c322c3139322c3231392c3135322c3137352c3131322c34352c36392c3131322c36382c3139352c31372c34342c3230352c3230342c37382c3233342c3130362c3234362c3230392c33312c3230302c3137312c3130382c32372c3136352c35382c3232392c37342c35382c3139312c3132352c3231385d2c5b3130382c39382c35322c3137372c3131332c3132392c3139342c37312c3133352c3137342c37342c3230352c38392c3137352c312c3230382c3234362c3136312c3132322c3233312c33302c3137382c32362c3234312c39382c35322c3133322c31342c33372c302c3138312c3232342c3130332c34362c3130312c3232322c3139392c36312c3231372c31322c39322c3231362c3139302c3131352c3233362c3134322c3138322c3235332c38312c32352c3138392c342c3235302c35382c34352c3234332c38302c37332c3130322c38332c32342c3130392c3131312c3138305d2c5b3138302c3230382c3234392c35312c3231362c3133352c34342c3134342c372c3132392c36302c36332c3234342c3130342c33362c3232392c34332c31312c3132332c38362c3131352c3131322c3138342c3230382c3135392c3138352c37332c31302c3136392c32372c39362c3231382c39392c3135322c3138312c36362c3230312c36302c3135342c31342c3231312c39342c3232392c3135382c3230382c3136362c3233302c37362c32332c3131382c3137382c3230382c38372c3131372c3233302c31392c3233312c32392c3230362c35382c3232352c32322c39352c3130335d5d2c22696e646578223a302c22686173686572223a6e756c6c7d7d2c7b227369676d61223a5b3137312c3139352c3234322c3235332c3137392c32352c3138372c3135312c3132322c3133302c3230372c3132322c38342c3132352c3134322c3132332c3233352c3134312c3230362c3136392c382c3136302c3138382c36382c35312c3232302c3232342c3231312c3137312c3230372c3231362c3139332c3230352c3139312c3233372c3131312c3232392c3132392c3131392c36362c3134342c3234382c3235322c39322c3234372c35382c37312c39355d2c22706b223a7b22766b223a5b3134332c3136312c3235352c34382c37382c35372c3230342c3232302c32352c3232312c3136342c3235322c3234382c31342c35362c3132362c3138362c3133352c3232382c3138382c3134352c3138312c35322c3230302c39372c39392c3231332c34362c302c3139392c3139332c38392c3138372c38382c32392c3133352c3137332c3234342c38362c33362c38332c35342c36372c3136342c362c3133372c39342c37322c362c3130352c3132382c3132382c39332c34382c3137362c31312c342c3234362c3133382c34382c3138302c3133332c39302c3134322c3139322c32342c3139332c3131312c3134322c33312c37362c3131312c3131302c3233342c3135332c39302c3230382c3139322c33312c3132342c39352c3130322c34392c3135382c39392c35322c3232302c3136352c39342c3235312c36382c36392c3132312c31362c3232342c3139345d2c22706f70223a5b3136382c35302c3233332c3139332c31352c3133362c36352c37322c3132332c3134382c3132392c3137362c33382c3139382c3230392c34372c32382c3230342c3137362c3134342c35372c3235312c34322c32382c36362c37362c38392c39372c3135382c36332c35342c3139382c3139342c3137362c3133352c3232312c31342c3138352c3139372c3232352c3230322c39382c3234332c37342c3233332c3232352c3134332c3135312c3134372c3137372c3137302c3131372c36362c3136352c36362c36322c33332c3231362c3233322c37352c36382c3131342c3139352c32322c3130302c36352c34342c3139382c342c3136362c3130322c3233332c3235332c3234302c35392c3137352c36302c3131372c3134322c3131342c3134302c3132322c31372c38372c3131302c3138372c312c31372c31302c3139352c3135342c31332c3234392c38362c35342c3232365d7d2c227061727479223a302c227374616b65223a3832362c22696e646578223a33352c2270617468223a7b2276616c756573223a5b5b3235332c3135382c3135382c3232322c3233352c3137302c3137362c3139392c33332c3230302c36362c32362c3231312c3137392c3132362c3232362c35352c3234302c3138322c33302c3234362c3231352c37362c3135382c31362c3131342c342c3231392c36322c3131352c3235332c322c3139322c3231392c3135322c3137352c3131322c34352c36392c3131322c36382c3139352c31372c34342c3230352c3230342c37382c3233342c3130362c3234362c3230392c33312c3230302c3137312c3130382c32372c3136352c35382c3232392c37342c35382c3139312c3132352c3231385d2c5b3130382c39382c35322c3137372c3131332c3132392c3139342c37312c3133352c3137342c37342c3230352c38392c3137352c312c3230382c3234362c3136312c3132322c3233312c33302c3137382c32362c3234312c39382c35322c3133322c31342c33372c302c3138312c3232342c3130332c34362c3130312c3232322c3139392c36312c3231372c31322c39322c3231362c3139302c3131352c3233362c3134322c3138322c3235332c38312c32352c3138392c342c3235302c35382c34352c3234332c38302c37332c3130322c38332c32342c3130392c3131312c3138305d2c5b3138302c3230382c3234392c35312c3231362c3133352c34342c3134342c372c3132392c36302c36332c3234342c3130342c33362c3232392c34332c31312c3132332c38362c3131352c3131322c3138342c3230382c3135392c3138352c37332c31302c3136392c32372c39362c3231382c39392c3135322c3138312c36362c3230312c36302c3135342c31342c3231312c39342c3232392c3135382c3230382c3136362c3233302c37362c32332c3131382c3137382c3230382c38372c3131372c3233302c31392c3233312c32392c3230362c35382c3232352c32322c39352c3130335d5d2c22696e646578223a302c22686173686572223a6e756c6c7d7d2c7b227369676d61223a5b3137312c3139352c3234322c3235332c3137392c32352c3138372c3135312c3132322c3133302c3230372c3132322c38342c3132352c3134322c3132332c3233352c3134312c3230362c3136392c382c3136302c3138382c36382c35312c3232302c3232342c3231312c3137312c3230372c3231362c3139332c3230352c3139312c3233372c3131312c3232392c3132392c3131392c36362c3134342c3234382c3235322c39322c3234372c35382c37312c39355d2c22706b223a7b22766b223a5b3134332c3136312c3235352c34382c37382c35372c3230342c3232302c32352c3232312c3136342c3235322c3234382c31342c35362c3132362c3138362c3133352c3232382c3138382c3134352c3138312c35322c3230302c39372c39392c3231332c34362c302c3139392c3139332c38392c3138372c38382c32392c3133352c3137332c3234342c38362c33362c38332c35342c36372c3136342c362c3133372c39342c37322c362c3130352c3132382c3132382c39332c34382c3137362c31312c342c3234362c3133382c34382c3138302c3133332c39302c3134322c3139322c32342c3139332c3131312c3134322c33312c37362c3131312c3131302c3233342c3135332c39302c3230382c3139322c33312c3132342c39352c3130322c34392c3135382c39392c35322c3232302c3136352c39342c3235312c36382c36392c3132312c31362c3232342c3139345d2c22706f70223a5b3136382c35302c3233332c3139332c31352c3133362c36352c37322c3132332c3134382c3132392c3137362c33382c3139382c3230392c34372c32382c3230342c3137362c3134342c35372c3235312c34322c32382c36362c37362c38392c39372c3135382c36332c35342c3139382c3139342c3137362c3133352c3232312c31342c3138352c3139372c3232352c3230322c39382c3234332c37342c3233332c3232352c3134332c3135312c3134372c3137372c3137302c3131372c36362c3136352c36362c36322c33332c3231362c3233322c37352c36382c3131342c3139352c32322c3130302c36352c34342c3139382c342c3136362c3130322c3233332c3235332c3234302c35392c3137352c36302c3131372c3134322c3131342c3134302c3132322c31372c38372c3131302c3138372c312c31372c31302c3139352c3135342c31332c3234392c38362c35342c3232365d7d2c227061727479223a302c227374616b65223a3832362c22696e646578223a35372c2270617468223a7b2276616c756573223a5b5b3235332c3135382c3135382c3232322c3233352c3137302c3137362c3139392c33332c3230302c36362c32362c3231312c3137392c3132362c3232362c35352c3234302c3138322c33302c3234362c3231352c37362c3135382c31362c3131342c342c3231392c36322c3131352c3235332c322c3139322c3231392c3135322c3137352c3131322c34352c36392c3131322c36382c3139352c31372c34342c3230352c3230342c37382c3233342c3130362c3234362c3230392c33312c3230302c3137312c3130382c32372c3136352c35382c3232392c37342c35382c3139312c3132352c3231385d2c5b3130382c39382c35322c3137372c3131332c3132392c3139342c37312c3133352c3137342c37342c3230352c38392c3137352c312c3230382c3234362c3136312c3132322c3233312c33302c3137382c32362c3234312c39382c35322c3133322c31342c33372c302c3138312c3232342c3130332c34362c3130312c3232322c3139392c36312c3231372c31322c39322c3231362c3139302c3131352c3233362c3134322c3138322c3235332c38312c32352c3138392c342c3235302c35382c34352c3234332c38302c37332c3130322c38332c32342c3130392c3131312c3138305d2c5b3138302c3230382c3234392c35312c3231362c3133352c34342c3134342c372c3132392c36302c36332c3234342c3130342c33362c3232392c34332c31312c3132332c38362c3131352c3131322c3138342c3230382c3135392c3138352c37332c31302c3136392c32372c39362c3231382c39392c3135322c3138312c36362c3230312c36302c3135342c31342c3231312c39342c3232392c3135382c3230382c3136362c3233302c37362c32332c3131382c3137382c3230382c38372c3131372c3233302c31392c3233312c32392c3230362c35382c3232352c32322c39352c3130335d5d2c22696e646578223a302c22686173686572223a6e756c6c7d7d2c7b227369676d61223a5b3137312c3139352c3234322c3235332c3137392c32352c3138372c3135312c3132322c3133302c3230372c3132322c38342c3132352c3134322c3132332c3233352c3134312c3230362c3136392c382c3136302c3138382c36382c35312c3232302c3232342c3231312c3137312c3230372c3231362c3139332c3230352c3139312c3233372c3131312c3232392c3132392c3131392c36362c3134342c3234382c3235322c39322c3234372c35382c37312c39355d2c22706b223a7b22766b223a5b3134332c3136312c3235352c34382c37382c35372c3230342c3232302c32352c3232312c3136342c3235322c3234382c31342c35362c3132362c3138362c3133352c3232382c3138382c3134352c3138312c35322c3230302c39372c39392c3231332c34362c302c3139392c3139332c38392c3138372c38382c32392c3133352c3137332c3234342c38362c33362c38332c35342c36372c3136342c362c3133372c39342c37322c362c3130352c3132382c3132382c39332c34382c3137362c31312c342c3234362c3133382c34382c3138302c3133332c39302c3134322c3139322c32342c3139332c3131312c3134322c33312c37362c3131312c3131302c3233342c3135332c39302c3230382c3139322c33312c3132342c39352c3130322c34392c3135382c39392c35322c3232302c3136352c39342c3235312c36382c36392c3132312c31362c3232342c3139345d2c22706f70223a5b3136382c35302c3233332c3139332c31352c3133362c36352c37322c3132332c3134382c3132392c3137362c33382c3139382c3230392c34372c32382c3230342c3137362c3134342c35372c3235312c34322c32382c36362c37362c38392c39372c3135382c36332c35342c3139382c3139342c3137362c3133352c3232312c31342c3138352c3139372c3232352c3230322c39382c3234332c37342c3233332c3232352c3134332c3135312c3134372c3137372c3137302c3131372c36362c3136352c36362c36322c33332c3231362c3233322c37352c36382c3131342c3139352c32322c3130302c36352c34342c3139382c342c3136362c3130322c3233332c3235332c3234302c35392c3137352c36302c3131372c3134322c3131342c3134302c3132322c31372c38372c3131302c3138372c312c31372c31302c3139352c3135342c31332c3234392c38362c35342c3232365d7d2c227061727479223a302c227374616b65223a3832362c22696e646578223a382c2270617468223a7b2276616c756573223a5b5b3235332c3135382c3135382c3232322c3233352c3137302c3137362c3139392c33332c3230302c36362c32362c3231312c3137392c3132362c3232362c35352c3234302c3138322c33302c3234362c3231352c37362c3135382c31362c3131342c342c3231392c36322c3131352c3235332c322c3139322c3231392c3135322c3137352c3131322c34352c36392c3131322c36382c3139352c31372c34342c3230352c3230342c37382c3233342c3130362c3234362c3230392c33312c3230302c3137312c3130382c32372c3136352c35382c3232392c37342c35382c3139312c3132352c3231385d2c5b3130382c39382c35322c3137372c3131332c3132392c3139342c37312c3133352c3137342c37342c3230352c38392c3137352c312c3230382c3234362c3136312c3132322c3233312c33302c3137382c32362c3234312c39382c35322c3133322c31342c33372c302c3138312c3232342c3130332c34362c3130312c3232322c3139392c36312c3231372c31322c39322c3231362c3139302c3131352c3233362c3134322c3138322c3235332c38312c32352c3138392c342c3235302c35382c34352c3234332c38302c37332c3130322c38332c32342c3130392c3131312c3138305d2c5b3138302c3230382c3234392c35312c3231362c3133352c34342c3134342c372c3132392c36302c36332c3234342c3130342c33362c3232392c34332c31312c3132332c38362c3131352c3131322c3138342c3230382c3135392c3138352c37332c31302c3136392c32372c39362c3231382c39392c3135322c3138312c36362c3230312c36302c3135342c31342c3231312c39342c3232392c3135382c3230382c3136362c3233302c37362c32332c3131382c3137382c3230382c38372c3131372c3233302c31392c3233312c32392c3230362c35382c3232352c32322c39352c3130335d5d2c22696e646578223a302c22686173686572223a6e756c6c7d7d2c7b227369676d61223a5b3137312c3139352c3234322c3235332c3137392c32352c3138372c3135312c3132322c3133302c3230372c3132322c38342c3132352c3134322c3132332c3233352c3134312c3230362c3136392c382c3136302c3138382c36382c35312c3232302c3232342c3231312c3137312c3230372c3231362c3139332c3230352c3139312c3233372c3131312c3232392c3132392c3131392c36362c3134342c3234382c3235322c39322c3234372c35382c37312c39355d2c22706b223a7b22766b223a5b3134332c3136312c3235352c34382c37382c35372c3230342c3232302c32352c3232312c3136342c3235322c3234382c31342c35362c3132362c3138362c3133352c3232382c3138382c3134352c3138312c35322c3230302c39372c39392c3231332c34362c302c3139392c3139332c38392c3138372c38382c32392c3133352c3137332c3234342c38362c33362c38332c35342c36372c3136342c362c3133372c39342c37322c362c3130352c3132382c3132382c39332c34382c3137362c31312c342c3234362c3133382c34382c3138302c3133332c39302c3134322c3139322c32342c3139332c3131312c3134322c33312c37362c3131312c3131302c3233342c3135332c39302c3230382c3139322c33312c3132342c39352c3130322c34392c3135382c39392c35322c3232302c3136352c39342c3235312c36382c36392c3132312c31362c3232342c3139345d2c22706f70223a5b3136382c35302c3233332c3139332c31352c3133362c36352c37322c3132332c3134382c3132392c3137362c33382c3139382c3230392c34372c32382c3230342c3137362c3134342c35372c3235312c34322c32382c36362c37362c38392c39372c3135382c36332c35342c3139382c3139342c3137362c3133352c3232312c31342c3138352c3139372c3232352c3230322c39382c3234332c37342c3233332c3232352c3134332c3135312c3134372c3137372c3137302c3131372c36362c3136352c36362c36322c33332c3231362c3233322c37352c36382c3131342c3139352c32322c3130302c36352c34342c3139382c342c3136362c3130322c3233332c3235332c3234302c35392c3137352c36302c3131372c3134322c3131342c3134302c3132322c31372c38372c3131302c3138372c312c31372c31302c3139352c3135342c31332c3234392c38362c35342c3232365d7d2c227061727479223a302c227374616b65223a3832362c22696e646578223a31312c2270617468223a7b2276616c756573223a5b5b3235332c3135382c3135382c3232322c3233352c3137302c3137362c3139392c33332c3230302c36362c32362c3231312c3137392c3132362c3232362c35352c3234302c3138322c33302c3234362c3231352c37362c3135382c31362c3131342c342c3231392c36322c3131352c3235332c322c3139322c3231392c3135322c3137352c3131322c34352c36392c3131322c36382c3139352c31372c34342c3230352c3230342c37382c3233342c3130362c3234362c3230392c33312c3230302c3137312c3130382c32372c3136352c35382c3232392c37342c35382c3139312c3132352c3231385d2c5b3130382c39382c35322c3137372c3131332c3132392c3139342c37312c3133352c3137342c37342c3230352c38392c3137352c312c3230382c3234362c3136312c3132322c3233312c33302c3137382c32362c3234312c39382c35322c3133322c31342c33372c302c3138312c3232342c3130332c34362c3130312c3232322c3139392c36312c3231372c31322c39322c3231362c3139302c3131352c3233362c3134322c3138322c3235332c38312c32352c3138392c342c3235302c35382c34352c3234332c38302c37332c3130322c38332c32342c3130392c3131312c3138305d2c5b3138302c3230382c3234392c35312c3231362c3133352c34342c3134342c372c3132392c36302c36332c3234342c3130342c33362c3232392c34332c31312c3132332c38362c3131352c3131322c3138342c3230382c3135392c3138352c37332c31302c3136392c32372c39362c3231382c39392c3135322c3138312c36362c3230312c36302c3135342c31342c3231312c39342c3232392c3135382c3230382c3136362c3233302c37362c32332c3131382c3137382c3230382c38372c3131372c3233302c31392c3233312c32392c3230362c35382c3232352c32322c39352c3130335d5d2c22696e646578223a302c22686173686572223a6e756c6c7d7d5d7d".to_string();
        let mut mock_multi_signer = MockMultiSigner::new();
        mock_multi_signer
            .expect_get_multi_signature()
            .return_once(move |_| Ok(Some(key_decode_hex(&multi_signature_hex).unwrap())));
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
            .return_once(|_| Err(ProtocolError::Codec("an error occurred".to_string())));
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
