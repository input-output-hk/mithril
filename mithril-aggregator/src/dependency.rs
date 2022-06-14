use std::sync::Arc;
use tokio::sync::RwLock;

use mithril_common::store::stake_store::StakeStore;

use super::entities::*;
use super::multi_signer::MultiSigner;
use super::snapshot_stores::SnapshotStore;
use crate::beacon_store::BeaconStore;
use crate::snapshot_uploaders::SnapshotUploader;
use crate::{CertificatePendingStore, CertificateStore, VerificationKeyStore};

/// BeaconStoreWrapper wraps a BeaconStore
pub type BeaconStoreWrapper = Arc<RwLock<dyn BeaconStore>>;

///  SnapshotStoreWrapper wraps a SnapshotStore
pub type SnapshotStoreWrapper = Arc<RwLock<dyn SnapshotStore>>;

/// MultiSignerWrapper wraps a MultiSigner
pub type MultiSignerWrapper = Arc<RwLock<dyn MultiSigner>>;

/// CertificatePendingStoreWrapper wraps a CertificatePendingStore
pub type CertificatePendingStoreWrapper = Arc<RwLock<CertificatePendingStore>>;

///  CertificateStoreWrapper wraps a CertificateStore
pub type CertificateStoreWrapper = Arc<RwLock<CertificateStore>>;

///  VerificationKeyStoreWrapper wraps a VerificationKeyStore
pub type VerificationKeyStoreWrapper = Arc<RwLock<VerificationKeyStore>>;

///  StakeStoreWrapper wraps a StakeStore
pub type StakeStoreWrapper = Arc<RwLock<StakeStore>>;

///  StakeStoreWrapper wraps a StakeStore
pub type SnapshotUploaderWrapper = Arc<RwLock<dyn SnapshotUploader>>;

/// DependencyManager handles the dependencies
pub struct DependencyManager {
    pub config: Config,
    pub snapshot_store: Option<SnapshotStoreWrapper>,
    pub snapshot_uploader: Option<SnapshotUploaderWrapper>,
    pub multi_signer: Option<MultiSignerWrapper>,
    pub beacon_store: Option<BeaconStoreWrapper>,
    pub certificate_pending_store: Option<CertificatePendingStoreWrapper>,
    pub certificate_store: Option<CertificateStoreWrapper>,
    pub verification_key_store: Option<VerificationKeyStoreWrapper>,
    pub stake_store: Option<StakeStoreWrapper>,
}

impl DependencyManager {
    /// DependencyManager factory
    pub fn new(config: Config) -> Self {
        Self {
            config,
            snapshot_store: None,
            snapshot_uploader: None,
            multi_signer: None,
            beacon_store: None,
            certificate_pending_store: None,
            certificate_store: None,
            verification_key_store: None,
            stake_store: None,
        }
    }

    /// With SnapshotStore middleware
    pub fn with_snapshot_store(&mut self, snapshot_store: SnapshotStoreWrapper) -> &mut Self {
        self.snapshot_store = Some(snapshot_store);
        self
    }

    /// With SnapshotUploader middleware
    pub fn with_snapshot_uploader(
        &mut self,
        snapshot_uploader: SnapshotUploaderWrapper,
    ) -> &mut Self {
        self.snapshot_uploader = Some(snapshot_uploader);
        self
    }

    /// With MultiSigner middleware
    pub fn with_multi_signer(&mut self, multi_signer: MultiSignerWrapper) -> &mut Self {
        self.multi_signer = Some(multi_signer);
        self
    }

    /// With BeaconStore middleware
    pub fn with_beacon_store(&mut self, beacon_store: BeaconStoreWrapper) -> &mut Self {
        self.beacon_store = Some(beacon_store);
        self
    }

    /// With CertificatePendingStore middleware
    pub fn with_certificate_pending_store(
        &mut self,
        certificate_pending_store: CertificatePendingStoreWrapper,
    ) -> &mut Self {
        self.certificate_pending_store = Some(certificate_pending_store);
        self
    }

    /// With CertificateStore middleware
    pub fn with_certificate_store(
        &mut self,
        certificate_store: CertificateStoreWrapper,
    ) -> &mut Self {
        self.certificate_store = Some(certificate_store);
        self
    }

    /// With VerificationKeyStore middleware
    pub fn with_verification_key_store(
        &mut self,
        verification_key_store: VerificationKeyStoreWrapper,
    ) -> &mut Self {
        self.verification_key_store = Some(verification_key_store);
        self
    }

    /// With StakeStore middleware
    pub fn with_stake_store(&mut self, stake_store: StakeStoreWrapper) -> &mut Self {
        self.stake_store = Some(stake_store);
        self
    }
}
