use crate::beacon_store::BeaconStore;
use crate::{CertificatePendingStore, CertificateStore};
use std::sync::Arc;
use tokio::sync::RwLock;

use super::entities::*;
use super::multi_signer::MultiSigner;
use super::snapshot_stores::SnapshotStore;

/// BeaconStoreWrapper wraps a BeaconStore
pub type BeaconStoreWrapper = Arc<RwLock<dyn BeaconStore>>;

///  SnapshotStoreWrapper wraps a SnapshotStore
pub type SnapshotStoreWrapper = Arc<RwLock<dyn SnapshotStore>>;

/// MultiSignerWrapper wraps a MultiSigner
pub type MultiSignerWrapper = Arc<RwLock<dyn MultiSigner>>;

pub type CertificatePendingStoreWrapper = Arc<RwLock<CertificatePendingStore>>;

pub type CertificateStoreWrapper = Arc<RwLock<CertificateStore>>;

/// DependencyManager handles the dependencies
pub struct DependencyManager {
    pub config: Config,
    pub snapshot_store: Option<SnapshotStoreWrapper>,
    pub multi_signer: Option<MultiSignerWrapper>,
    pub beacon_store: Option<BeaconStoreWrapper>,
    pub certificate_pending_store: Option<CertificatePendingStoreWrapper>,
    pub certificate_store: Option<CertificateStoreWrapper>,
}

impl DependencyManager {
    /// DependencyManager factory
    pub fn new(config: Config) -> Self {
        Self {
            config,
            snapshot_store: None,
            multi_signer: None,
            beacon_store: None,
            certificate_pending_store: None,
            certificate_store: None,
        }
    }

    /// With SnapshotStore
    pub fn with_snapshot_store(&mut self, snapshot_store: SnapshotStoreWrapper) -> &mut Self {
        self.snapshot_store = Some(snapshot_store);
        self
    }

    /// With MultiSigner
    pub fn with_multi_signer(&mut self, multi_signer: MultiSignerWrapper) -> &mut Self {
        self.multi_signer = Some(multi_signer);
        self
    }

    /// With MultiSigner
    pub fn with_beacon_store(&mut self, beacon_store: BeaconStoreWrapper) -> &mut Self {
        self.beacon_store = Some(beacon_store);
        self
    }

    pub fn with_certificate_pending_store(
        &mut self,
        certificate_pending_store: CertificatePendingStoreWrapper,
    ) -> &mut Self {
        self.certificate_pending_store = Some(certificate_pending_store);
        self
    }

    pub fn with_certificate_store(
        &mut self,
        certificate_store: CertificateStoreWrapper,
    ) -> &mut Self {
        self.certificate_store = Some(certificate_store);
        self
    }
}
