use std::{path::PathBuf, sync::Arc};

use async_trait::async_trait;
use mithril_common::{certificate_chain::CertificateVerifier, entities::Snapshot, StdError};
use thiserror::Error;

use crate::aggregator_client::{AggregatorHTTPClientError, SnapshotClient};

type SnapshotServiceResult<T> = Result<T, SnapshotServiceError>;

/// [AggregatorHandler] related errors.
#[derive(Error, Debug)]
pub enum SnapshotServiceError {
    /// The given identifier does not link to an existing snapshot.
    #[error("Snapshot '{0}' not found")]
    SnapshotNotFound(String),

    /// Error raised when the certificate verification failed for the downloaded archive.
    #[error("Certificate verification failed for snapshot '{digest}'. The archive has been downloaded as '{path}'.")]
    CouldNotVerifySnapshot {
        /// The identifier of the snapshot
        digest: String,
        /// The path of the downloaded archive
        path: PathBuf,
    },

    /// A command cannot be fulfilled because of an underlying error
    #[error("A subsystem failed: {message} ({error})")]
    Subsystem {
        /// error context
        message: String,

        /// original error
        error: StdError,
    },
}

/// ## SnapshotService
///
/// This trait is the interface for the Snapshot service used in the main commands.
#[async_trait]
pub trait SnapshotService {
    /// Return the list of the snapshots stored by the Aggregator.
    async fn list(&self) -> SnapshotServiceResult<Vec<Snapshot>>;

    /// Show details of the snapshot identified by the given digest.
    async fn show(&self, digest: &str) -> SnapshotServiceResult<Snapshot>;

    /// Download and verify the snapshot identified by the given digest.
    async fn download(&self, digest: &str) -> SnapshotServiceResult<Snapshot>;
}

/// Configuration related to the [SnapshotService].
pub struct SnapshotConfig {}

/// Service used by the Command to perform business oriented tasks.
pub struct MithrilClientSnapshotService {
    /// Command configuration
    config: SnapshotConfig,

    /// Snapshot HTTP client
    snapshot_client: Arc<SnapshotClient>,

    /// Certificate verifier
    certificate_verifier: Arc<dyn CertificateVerifier>,
}

impl MithrilClientSnapshotService {
    /// Create a new instance of the service.
    pub fn new(
        config: SnapshotConfig,
        snapshot_client: Arc<SnapshotClient>,
        certificate_verifier: Arc<dyn CertificateVerifier>,
    ) -> Self {
        Self {
            config,
            snapshot_client,
            certificate_verifier,
        }
    }
}

#[async_trait]
impl SnapshotService for MithrilClientSnapshotService {
    async fn list(&self) -> SnapshotServiceResult<Vec<Snapshot>> {
        self.snapshot_client
            .list()
            .await
            .map_err(|error| SnapshotServiceError::Subsystem {
                message: "Could not list snapshots".to_string(),
                error: error.into(),
            })
    }

    async fn show(&self, digest: &str) -> SnapshotServiceResult<Snapshot> {
        self.snapshot_client.show(digest).await.map_err(|e| {
            if matches!(&e, AggregatorHTTPClientError::RemoteServerLogical(_)) {
                SnapshotServiceError::SnapshotNotFound(digest.to_owned())
            } else {
                SnapshotServiceError::Subsystem {
                    message: format!("Could not get snapshot details for digest '{digest}'"),
                    error: e.into(),
                }
            }
        })
    }

    async fn download(&self, digest: &str) -> SnapshotServiceResult<Snapshot> {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::messages::{SnapshotListItemMessage, SnapshotListMessage, SnapshotMessage};
    use mithril_common::test_utils::fake_data;

    use crate::aggregator_client::MockAggregatorHTTPClient;

    use super::super::mock::*;

    use super::*;

    fn get_snapshot_list_message() -> SnapshotListMessage {
        let item1 = SnapshotListItemMessage {
            digest: "digest-1".to_string(),
            beacon: fake_data::beacon(),
            certificate_hash: "certificate-hash-1".to_string(),
            size: 1024,
            created_at: "whatever".to_string(),
            locations: vec!["location-1.1".to_string(), "location-1.2".to_string()],
        };
        let item2 = SnapshotListItemMessage {
            digest: "digest-2".to_string(),
            beacon: fake_data::beacon(),
            certificate_hash: "certificate-hash-2".to_string(),
            size: 1024,
            created_at: "whatever".to_string(),
            locations: vec!["location-2.1".to_string(), "location-2.2".to_string()],
        };

        vec![item1, item2]
    }

    fn get_snapshot_message() -> SnapshotMessage {
        SnapshotMessage {
            digest: "digest-10".to_string(),
            beacon: fake_data::beacon(),
            certificate_hash: "certificate-hash-10".to_string(),
            size: 1024,
            created_at: "whatever".to_string(),
            locations: vec!["location-10.1".to_string(), "location-10.2".to_string()],
        }
    }

    #[tokio::test]
    async fn test_list_snapshots() {
        let mut http_client = MockAggregatorHTTPClient::new();
        http_client
            .expect_get_json()
            .returning(|| Ok(get_snapshot_list_message()));
        let snapshot_client = SnapshotClient::new(Arc::new(http_client), "whatever");
        let certificate_verifier = MockCertificateVerifierImpl::new();
        let config = SnapshotConfig {};
        let snapshot_service = MithrilClientSnapshotService::new(
            config,
            Arc::new(snapshot_client),
            Arc::new(certificate_verifier),
        );

        let list = snapshot_service.list().await.unwrap();

        assert_eq!(2, list.len());
    }

    #[tokio::test]
    async fn test_list_snapshots_err() {
        let mut http_client = MockAggregatorHTTPClient::new();
        http_client
            .expect_get_json()
            .returning(|| {
                Err(AggregatorHTTPClientError::RemoteServerUnreachable(
                    "whatever".to_string(),
                ))
            })
            .times(1);
        let snapshot_client = SnapshotClient::new(Arc::new(http_client), "whatever");
        let certificate_verifier = MockCertificateVerifierImpl::new();
        let config = SnapshotConfig {};
        let snapshot_service = MithrilClientSnapshotService::new(
            config,
            Arc::new(snapshot_client),
            Arc::new(certificate_verifier),
        );

        let error = snapshot_service.list().await.unwrap_err();

        assert!(matches!(
            error,
            SnapshotServiceError::Subsystem {
                message: _,
                error: _
            }
        ));
    }

    #[tokio::test]
    async fn test_show_snapshot() {
        let mut http_client = MockAggregatorHTTPClient::new();
        http_client
            .expect_get_json()
            .return_once(|_| Ok(get_snapshot_message()))
            .times(1);
        let snapshot_client = SnapshotClient::new(Arc::new(http_client), "whatever");
        let certificate_verifier = MockCertificateVerifierImpl::new();
        let config = SnapshotConfig {};
        let snapshot_service = MithrilClientSnapshotService::new(
            config,
            Arc::new(snapshot_client),
            Arc::new(certificate_verifier),
        );

        assert_eq!(
            "digest-10".to_string(),
            snapshot_service.show("digest").await.unwrap().digest
        );
    }

    #[tokio::test]
    async fn test_show_snapshot_not_found() {
        let mut http_client = MockAggregatorHTTPClient::new();
        http_client
            .expect_get_json()
            .return_once(move |_| {
                Err(AggregatorHTTPClientError::RemoteServerLogical(
                    "whatever".to_string(),
                ))
            })
            .times(1);
        let snapshot_client = SnapshotClient::new(Arc::new(http_client), "whatever");
        let certificate_verifier = MockCertificateVerifierImpl::new();
        let config = SnapshotConfig {};
        let snapshot_service = MithrilClientSnapshotService::new(
            config,
            Arc::new(snapshot_client),
            Arc::new(certificate_verifier),
        );
        let err = snapshot_service.show("digest-10").await.unwrap_err();

        assert!(
            matches!(err, SnapshotServiceError::SnapshotNotFound(e) if e == "digest-10".to_string())
        );
    }

    #[tokio::test]
    async fn test_show_snapshot_err() {
        let mut http_client = MockAggregatorHTTPClient::new();
        http_client
            .expect_get_json()
            .return_once(move |_| {
                Err(AggregatorHTTPClientError::ApiVersionMismatch(
                    "whatever".to_string(),
                ))
            })
            .times(1);
        let snapshot_client = SnapshotClient::new(Arc::new(http_client), "whatever");
        let certificate_verifier = MockCertificateVerifierImpl::new();
        let config = SnapshotConfig {};
        let snapshot_service = MithrilClientSnapshotService::new(
            config,
            Arc::new(snapshot_client),
            Arc::new(certificate_verifier),
        );
        let err = snapshot_service.show("digest-10").await.unwrap_err();

        assert!(matches!(
            err,
            SnapshotServiceError::Subsystem {
                message: _,
                error: _
            }
        ));
    }

    #[tokio::test]
    async fn test_download_snapshot() {
        let mut http_client = MockAggregatorHTTPClient::new();
        http_client
            .expect_download()
            .return_once(move |_| Ok(()))
            .times(1);
        let snapshot_client = SnapshotClient::new(Arc::new(http_client), "whatever");
        let certificate_verifier = MockCertificateVerifierImpl::new();
        let config = SnapshotConfig {};
        let snapshot_service = MithrilClientSnapshotService::new(
            config,
            Arc::new(snapshot_client),
            Arc::new(certificate_verifier),
        );

        snapshot_service.download("digest").unwrap();
    }
}
