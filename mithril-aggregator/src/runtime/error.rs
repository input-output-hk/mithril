use crate::snapshot_stores::SnapshotStoreError;
use crate::{ProtocolError, SnapshotError};

use mithril_common::chain_observer::ChainObserverError;
use mithril_common::digesters::{ImmutableDigesterError, ImmutableFileListingError};
use mithril_common::entities::BeaconComparisonError;
use mithril_common::entities::Epoch;
use mithril_common::store::StoreError;
use mithril_common::BeaconProviderError;
use std::error::Error as StdError;
use std::io;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum RuntimeError {
    #[error("multi signer error: {0}")]
    MultiSigner(#[from] ProtocolError),

    #[error("snapshotter error: {0}")]
    Snapshotter(#[from] SnapshotError),

    #[error("digester error: {0}")]
    Digester(#[from] ImmutableDigesterError),

    #[error("store error: {0}")]
    StoreError(#[from] StoreError),

    #[error("snapshot uploader error: {0}")]
    SnapshotUploader(String),

    #[error("snapshot build error: {0}")]
    SnapshotBuild(#[from] io::Error),

    #[error("immutable file scanning error: {0}")]
    ImmutableFile(#[from] ImmutableFileListingError),

    #[error("chain observer error: {0}")]
    ChainObserver(#[from] ChainObserverError),

    #[error("beacon provider error: {0}")]
    BeaconProvider(#[from] BeaconProviderError),

    #[error("certificate chain gap error: {0} vs {1}")]
    CertificateChainEpochGap(Epoch, Epoch),

    #[error("snapshot store error: {0}")]
    SnapshotStore(#[from] SnapshotStoreError),

    #[error("beacon comparison error: {0}")]
    BeaconComparisonError(#[from] BeaconComparisonError),

    #[error("general error: {0}")]
    General(Box<dyn StdError + Sync + Send>),
}
