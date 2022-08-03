use crate::snapshot_stores::SnapshotStoreError;
use crate::store::StoreError;
use crate::{BeaconStoreError, ProtocolError, SnapshotError};

use mithril_common::chain_observer::ChainObserverError;
use mithril_common::digesters::{ImmutableDigesterError, ImmutableFileListingError};
use mithril_common::store::StakeStoreError;
use mithril_common::BeaconProviderError;
use std::error::Error as StdError;
use std::io;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum RuntimeError {
    #[error("multi signer error: {0}")]
    MultiSigner(#[from] ProtocolError),

    #[error("beacon store error: {0}")]
    BeaconStore(#[from] BeaconStoreError),

    #[error("snapshotter error: {0}")]
    Snapshotter(#[from] SnapshotError),

    #[error("digester error: {0}")]
    Digester(#[from] ImmutableDigesterError),

    #[error("snapshot store error: {0}")]
    SnapshotStore(#[from] SnapshotStoreError),

    #[error("stake store error: {0}")]
    StakeStore(#[from] StakeStoreError),

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

    #[error("general error: {0}")]
    General(Box<dyn StdError + Sync + Send>),
}
