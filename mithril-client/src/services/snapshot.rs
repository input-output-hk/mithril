use std::{
    fs::{remove_file, File},
    path::{Path, PathBuf},
    sync::Arc,
};

use async_trait::async_trait;
use flate2::read::GzDecoder;
use human_bytes::human_bytes;
use mithril_common::{
    certificate_chain::CertificateVerifier,
    crypto_helper::{key_decode_hex, ProtocolGenesisVerifier},
    digesters::ImmutableDigester,
    entities::{ProtocolMessagePartKey, Snapshot},
    StdError, StdResult,
};
use slog_scope::{debug, info, warn};
use tar::Archive;
use thiserror::Error;

use crate::aggregator_client::{AggregatorHTTPClientError, CertificateClient, SnapshotClient};

/// [SnapshotService] related errors.
#[derive(Error, Debug)]
pub enum SnapshotServiceError {
    /// The given identifier does not link to an existing snapshot.
    #[error("Snapshot '{0}' not found")]
    SnapshotNotFound(String),

    /// Error raised when the certificate verification failed for the downloaded archive.
    #[error("Certificate verification failed (snapshot digest = '{digest}'). The archive has been downloaded in '{path}'.")]
    CouldNotVerifySnapshot {
        /// snapshot digest
        digest: String,

        /// The path of the downloaded archive
        path: PathBuf,
    },

    /// The given certificate could not be found, contains the certificate hash
    #[error("Could not find certificate '{0}'.")]
    CouldNotFindCertificate(String),

    /// The configuration has invalid or missing parameters
    #[error("Missing or invalid parameters: {context}. Error: {error}")]
    InvalidParameters {
        /// Error context
        context: String,

        /// Eventual nested error
        error: StdError,
    },

    /// The directory where the files from snapshot are expanded already exists.
    /// An error is raised because it lets the user a chance to preserve a
    /// previous work.
    #[error("Unpack directory '{0}' already exists, please move or delete it.")]
    UnpackDirectoryAlreadyExists(PathBuf),
}

/// ## SnapshotService
///
/// This trait is the interface for the Snapshot service used in the main commands.
#[async_trait]
pub trait SnapshotService: Sync + Send {
    /// Return the list of the snapshots stored by the Aggregator.
    async fn list(&self) -> StdResult<Vec<Snapshot>>;

    /// Show details of the snapshot identified by the given digest.
    async fn show(&self, digest: &str) -> StdResult<Snapshot>;

    /// Download and verify the snapshot identified by the given digest.
    /// The returned path is the location where the archive has been unpacked.
    async fn download(
        &self,
        snapshot: &Snapshot,
        pathdir: &Path,
        genesis_verification_key: &str,
    ) -> StdResult<PathBuf>;
}

/// Service used by the Command to perform business oriented tasks.
pub struct MithrilClientSnapshotService {
    /// Snapshot HTTP client
    snapshot_client: Arc<SnapshotClient>,

    /// Certificate HTTP client
    certificate_client: Arc<CertificateClient>,

    /// Certificate verifier
    certificate_verifier: Arc<dyn CertificateVerifier>,

    /// Imutable digester
    immutable_digester: Arc<dyn ImmutableDigester>,
}

impl MithrilClientSnapshotService {
    /// Create a new instance of the service.
    pub fn new(
        snapshot_client: Arc<SnapshotClient>,
        certificate_client: Arc<CertificateClient>,
        certificate_verifier: Arc<dyn CertificateVerifier>,
        immutable_digester: Arc<dyn ImmutableDigester>,
    ) -> Self {
        Self {
            snapshot_client,
            certificate_client,
            certificate_verifier,
            immutable_digester,
        }
    }

    async fn unpack_snapshot(&self, filepath: &Path, unpack_dir: &Path) -> StdResult<()> {
        let snapshot_file_tar_gz = File::open(filepath)?;
        let snapshot_file_tar = GzDecoder::new(snapshot_file_tar_gz);
        let mut snapshot_archive = Archive::new(snapshot_file_tar);
        snapshot_archive.unpack(unpack_dir)?;

        Ok(())
    }
}

#[async_trait]
impl SnapshotService for MithrilClientSnapshotService {
    async fn list(&self) -> StdResult<Vec<Snapshot>> {
        debug!("Snapshot service: list.");

        self.snapshot_client.list().await
    }

    async fn show(&self, digest: &str) -> StdResult<Snapshot> {
        debug!("Snapshot service: show.");
        let snapshot =
            self.snapshot_client
                .show(digest)
                .await
                .map_err(|e| match &e.downcast_ref::<AggregatorHTTPClientError>() {
                    Some(error)
                        if matches!(error, &&AggregatorHTTPClientError::RemoteServerLogical(_)) =>
                    {
                        Box::new(SnapshotServiceError::SnapshotNotFound(digest.to_owned()))
                            as StdError
                    }
                    _ => e,
                })?;

        Ok(snapshot)
    }

    async fn download(
        &self,
        snapshot: &Snapshot,
        pathdir: &Path,
        genesis_verification_key: &str,
    ) -> StdResult<PathBuf> {
        debug!("Snapshot service: download.");
        // 0 - Check prerequisistes are met
        let unpack_dir = pathdir.join("db");

        if unpack_dir.exists() {
            return Err(SnapshotServiceError::UnpackDirectoryAlreadyExists(unpack_dir).into());
        }
        {
            let free_space = fs2::available_space(pathdir)? as f64;

            if free_space < 2.5 * snapshot.size as f64 {
                warn!("There might not be enough space on the disk ({} free) to unpack a {} size snapshot.", human_bytes(free_space), human_bytes(snapshot.size as f64));
            } else {
                info!("It seems there is enough disk space ({} free) to download and unpack the {} large snapshot.", human_bytes(free_space), human_bytes(snapshot.size as f64));
            }

            let _file =
                File::create(&unpack_dir).map_err(|e| SnapshotServiceError::InvalidParameters {
                    context: format!(
                        "Can not write in the given directory '{}'.",
                        pathdir.display()
                    ),
                    error: e.into(),
                })?;
            remove_file(&unpack_dir)?;
        }

        // 1 - Instanciate a genesis key verifier
        let genesis_verification_key = key_decode_hex(&genesis_verification_key.to_string())
            .map_err(|e| SnapshotServiceError::InvalidParameters {
                context: format!("Invalid genesis verification key '{genesis_verification_key}'"),
                error: e.into(),
            })?;
        let genesis_verifier =
            ProtocolGenesisVerifier::from_verification_key(genesis_verification_key);

        // 2 - Get certificate information
        let certificate = self
            .certificate_client
            .get(&snapshot.certificate_hash)
            .await?
            .ok_or_else(|| {
                SnapshotServiceError::CouldNotFindCertificate(snapshot.certificate_hash.clone())
            })?;

        // 3 - Check the certificate chain
        self.certificate_verifier
            .verify_certificate_chain(
                certificate.clone(),
                self.certificate_client.clone(),
                &genesis_verifier,
            )
            .await?;

        // 4 - Launch download and unpack the file on disk
        let filepath = self
            .snapshot_client
            .download(snapshot, pathdir)
            .await
            .map_err(|e| format!("Could not download file in '{}': {e}", pathdir.display()))?;

        self.unpack_snapshot(&filepath, &unpack_dir)
            .await
            .map_err(|e| {
                format!(
                    "Could not unpack file '{}' in '{}': {e}",
                    filepath.display(),
                    unpack_dir.display()
                )
            })?;
        let unpacked_snapshot_digest = self
            .immutable_digester
            .compute_digest(&unpack_dir, &certificate.beacon)
            .await
            .map_err(|e| {
                format!(
                    "Could not compute digest in '{}': {e}",
                    unpack_dir.display()
                )
            })?;

        // 5 - Compute protocol message and compare hash sums
        let mut protocol_message = certificate.protocol_message.clone();
        protocol_message.set_message_part(
            ProtocolMessagePartKey::SnapshotDigest,
            unpacked_snapshot_digest,
        );
        if protocol_message.compute_hash() != certificate.signed_message {
            debug!("Digest verification failed, removing unpacked files & directory.");

            if let Err(e) = std::fs::remove_dir_all(&unpack_dir) {
                warn!("Error while removing unpacked files & directory: {e}.");
            }

            return Err(SnapshotServiceError::CouldNotVerifySnapshot {
                digest: snapshot.digest.clone(),
                path: filepath.canonicalize().unwrap(),
            }
            .into());
        }

        Ok(unpack_dir)
    }
}

#[cfg(test)]
mod tests {
    use chrono::{DateTime, Utc};
    use config::{builder::DefaultState, ConfigBuilder};
    use flate2::{write::GzEncoder, Compression};
    use mithril_common::{
        crypto_helper::{key_encode_hex, tests_setup::setup_genesis},
        digesters::DumbImmutableDigester,
        messages::{
            CertificateMessage, SnapshotListItemMessage, SnapshotListMessage, SnapshotMessage,
        },
        test_utils::fake_data,
    };
    use std::{fs::create_dir_all, io::Write};

    use crate::{
        aggregator_client::{AggregatorClient, MockAggregatorHTTPClient},
        dependencies::DependenciesBuilder,
        FromSnapshotMessageAdapter,
    };

    use super::super::mock::*;

    use super::*;

    /// see [`archive_file_path`] to see where the dummy will be created
    fn build_dummy_snapshot(digest: &str, data_expected: &str, test_dir: &Path) {
        let archive_file_path = test_dir.join(format!("snapshot-{digest}"));
        let data_file_path = test_dir.join(Path::new("db/test_data.txt"));
        create_dir_all(data_file_path.parent().unwrap()).unwrap();
        let mut source_file = File::create(data_file_path.as_path()).unwrap();
        write!(source_file, "{data_expected}").unwrap();
        let archive_file = File::create(archive_file_path).unwrap();
        let archive_encoder = GzEncoder::new(&archive_file, Compression::default());
        let mut archive_builder = tar::Builder::new(archive_encoder);
        archive_builder
            .append_dir_all(".", data_file_path.parent().unwrap())
            .unwrap();
        archive_builder.into_inner().unwrap().finish().unwrap();
        let _ = std::fs::remove_dir_all(data_file_path.parent().unwrap());
    }

    fn get_snapshot_list_message() -> SnapshotListMessage {
        let item1 = SnapshotListItemMessage {
            digest: "digest-1".to_string(),
            beacon: fake_data::beacon(),
            certificate_hash: "certificate-hash-1".to_string(),
            size: 1024,
            created_at: DateTime::<Utc>::default(),
            locations: vec!["location-1.1".to_string(), "location-1.2".to_string()],
        };
        let item2 = SnapshotListItemMessage {
            digest: "digest-2".to_string(),
            beacon: fake_data::beacon(),
            certificate_hash: "certificate-hash-2".to_string(),
            size: 1024,
            created_at: DateTime::<Utc>::default(),
            locations: vec!["location-2.1".to_string(), "location-2.2".to_string()],
        };

        vec![item1, item2]
    }

    fn get_snapshot_message() -> SnapshotMessage {
        SnapshotMessage {
            digest: "digest-10".to_string(),
            beacon: fake_data::beacon(),
            certificate_hash: "snapshot-digest-123".to_string(),
            size: 1024,
            created_at: DateTime::<Utc>::default(),
            locations: vec!["location-10.1".to_string(), "location-10.2".to_string()],
        }
    }

    fn get_dep_builder(http_client: Arc<dyn AggregatorClient>) -> DependenciesBuilder {
        let config_builder: ConfigBuilder<DefaultState> = ConfigBuilder::default();
        let config = config_builder
            .set_default("download_dir", "")
            .unwrap()
            .build()
            .unwrap();

        let mut builder = DependenciesBuilder::new(Arc::new(config));
        builder.certificate_verifier = Some(Arc::new(MockCertificateVerifierImpl::new()));
        builder.immutable_digester = Some(Arc::new(DumbImmutableDigester::new("digest", true)));
        builder.aggregator_client = Some(http_client);

        builder
    }

    #[tokio::test]
    async fn test_list_snapshots() {
        let mut http_client = MockAggregatorHTTPClient::new();
        http_client
            .expect_get_content()
            .returning(|_| Ok(serde_json::to_string(&get_snapshot_list_message()).unwrap()));
        let snapshot_service = get_dep_builder(Arc::new(http_client))
            .get_snapshot_service()
            .await
            .unwrap();

        let list = snapshot_service.list().await.unwrap();

        assert_eq!(2, list.len());
    }

    #[tokio::test]
    async fn test_list_snapshots_err() {
        let mut http_client = MockAggregatorHTTPClient::new();
        http_client
            .expect_get_content()
            .returning(|_| {
                Err(AggregatorHTTPClientError::RemoteServerUnreachable(
                    "whatever".to_string(),
                ))
            })
            .times(1);
        let snapshot_service = get_dep_builder(Arc::new(http_client))
            .get_snapshot_service()
            .await
            .unwrap();

        snapshot_service.list().await.unwrap_err();
    }

    #[tokio::test]
    async fn test_show_snapshot() {
        let mut http_client = MockAggregatorHTTPClient::new();
        http_client
            .expect_get_content()
            .return_once(|_| Ok(serde_json::to_string(&get_snapshot_message()).unwrap()))
            .times(1);
        let snapshot_service = get_dep_builder(Arc::new(http_client))
            .get_snapshot_service()
            .await
            .unwrap();

        assert_eq!(
            "digest-10".to_string(),
            snapshot_service.show("digest").await.unwrap().digest
        );
    }

    #[tokio::test]
    async fn test_show_snapshot_not_found() {
        let mut http_client = MockAggregatorHTTPClient::new();
        http_client
            .expect_get_content()
            .return_once(move |_| {
                Err(AggregatorHTTPClientError::RemoteServerLogical(
                    "whatever".to_string(),
                ))
            })
            .times(1);
        let snapshot_service = get_dep_builder(Arc::new(http_client))
            .get_snapshot_service()
            .await
            .unwrap();

        snapshot_service.show("digest-10").await.unwrap_err();
    }

    #[tokio::test]
    async fn test_show_snapshot_err() {
        let mut http_client = MockAggregatorHTTPClient::new();
        http_client
            .expect_get_content()
            .return_once(move |_| {
                Err(AggregatorHTTPClientError::ApiVersionMismatch(
                    "whatever".to_string(),
                ))
            })
            .times(1);
        let snapshot_service = get_dep_builder(Arc::new(http_client))
            .get_snapshot_service()
            .await
            .unwrap();

        snapshot_service.show("digest-10").await.unwrap_err();
    }

    #[tokio::test]
    async fn test_download_snapshot_ok() {
        let test_path = std::env::temp_dir().join("test_download_snapshot_ok");
        let _ = std::fs::remove_dir_all(&test_path);
        let mut http_client = MockAggregatorHTTPClient::new();
        http_client
            .expect_download()
            .returning(move |_, _| Ok(()))
            .times(1);
        http_client.expect_get_content().returning(|_| {
            let mut message = CertificateMessage::dummy();
            message.signed_message = message.protocol_message.compute_hash();
            let message = serde_json::to_string(&message).unwrap();

            Ok(message)
        });
        let mut builder = get_dep_builder(Arc::new(http_client));
        let mut certificate_verifier = MockCertificateVerifierImpl::new();
        certificate_verifier
            .expect_verify_certificate_chain()
            .returning(|_, _, _| Ok(()))
            .times(1);
        builder.certificate_verifier = Some(Arc::new(certificate_verifier));
        builder.immutable_digester = Some(Arc::new(DumbImmutableDigester::new(
            "snapshot-digest-123",
            true,
        )));
        let snapshot = FromSnapshotMessageAdapter::adapt(get_snapshot_message());
        let snapshot_service = builder.get_snapshot_service().await.unwrap();

        let (_, verifier) = setup_genesis();
        let genesis_verification_key = verifier.to_verification_key();
        build_dummy_snapshot("digest-10", "1234567890".repeat(124).as_str(), &test_path);
        let filepath = snapshot_service
            .download(
                &snapshot,
                &test_path,
                &key_encode_hex(genesis_verification_key).unwrap(),
            )
            .await
            .expect("Snapshot download should succeed.");
        assert!(filepath.exists());
        let unpack_dir = filepath
            .parent()
            .expect("Test downloaded file must be in a directory.")
            .join("db");
        assert!(unpack_dir.is_dir());
    }

    #[tokio::test]
    async fn test_download_snapshot_invalid_digest() {
        let test_path = std::env::temp_dir().join("test_download_snapshot_invalid_digest");
        let _ = std::fs::remove_dir_all(&test_path);
        let mut http_client = MockAggregatorHTTPClient::new();
        http_client
            .expect_download()
            .returning(move |_, _| Ok(()))
            .times(1);
        http_client.expect_get_content().returning(|_| {
            let mut message = CertificateMessage::dummy();
            message.signed_message = message.protocol_message.compute_hash();
            let message = serde_json::to_string(&message).unwrap();

            Ok(message)
        });
        let http_client = Arc::new(http_client);
        let mut dep_builder = get_dep_builder(http_client);
        let mut certificate_verifier = MockCertificateVerifierImpl::new();
        certificate_verifier
            .expect_verify_certificate_chain()
            .returning(|_, _, _| Ok(()))
            .times(1);
        let immutable_digester = DumbImmutableDigester::new("snapshot-digest-KO", true);
        dep_builder.certificate_verifier = Some(Arc::new(certificate_verifier));
        dep_builder.immutable_digester = Some(Arc::new(immutable_digester));
        let snapshot_service = dep_builder.get_snapshot_service().await.unwrap();
        let mut snapshot = FromSnapshotMessageAdapter::adapt(get_snapshot_message());
        snapshot.digest = "digest-10".to_string();

        let (_, verifier) = setup_genesis();
        let genesis_verification_key = verifier.to_verification_key();
        build_dummy_snapshot("digest-10", "1234567890".repeat(124).as_str(), &test_path);
        let err = snapshot_service
            .download(
                &snapshot,
                &test_path,
                &key_encode_hex(genesis_verification_key).unwrap(),
            )
            .await
            .expect_err("Snapshot digest comparison should fail.");

        if let Some(e) = err.downcast_ref::<SnapshotServiceError>() {
            match e {
                SnapshotServiceError::CouldNotVerifySnapshot {
                    digest,
                    path: _path,
                } => {
                    assert_eq!("digest-10", digest.as_str());
                }
                _ => panic!("Wrong error type when snapshot could not be verified."),
            }
        } else {
            panic!(
                "Expected a SnapshotServiceError when snapshot can not be verified. Got {err:?}: '{err}'"
            );
        }
        let filepath = test_path.join("snapshot-digest-10");
        assert!(filepath.exists());
        let unpack_dir = filepath
            .parent()
            .expect("Test downloaded file must be in a directory.")
            .join("db");
        assert!(!unpack_dir.exists());
    }

    #[tokio::test]
    async fn test_download_snapshot_dir_already_exists() {
        let test_path = std::env::temp_dir().join("test_download_snapshot_dir_already_exists");
        let _ = std::fs::remove_dir_all(&test_path);
        create_dir_all(test_path.join("db")).unwrap();
        let http_client = MockAggregatorHTTPClient::new();
        let http_client = Arc::new(http_client);
        let mut dep_builder = get_dep_builder(http_client);
        let snapshot_service = dep_builder.get_snapshot_service().await.unwrap();

        let (_, verifier) = setup_genesis();
        let genesis_verification_key = verifier.to_verification_key();
        let snapshot = FromSnapshotMessageAdapter::adapt(get_snapshot_message());
        let err = snapshot_service
            .download(
                &snapshot,
                &test_path,
                &key_encode_hex(genesis_verification_key).unwrap(),
            )
            .await
            .expect_err("Snapshot download should fail.");

        if let Some(e) = err.downcast_ref::<SnapshotServiceError>() {
            match e {
                SnapshotServiceError::UnpackDirectoryAlreadyExists(path) => {
                    assert_eq!(&test_path.join("db"), path);
                }
                _ => panic!("Wrong error type when unpack dir already exists."),
            }
        } else {
            panic!("Expected a SnapshotServiceError when unpack dir already exists. {err}");
        }
    }
}
