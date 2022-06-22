use crate::snapshot_stores::SnapshotStoreError;
use crate::store::StoreError;
use crate::{BeaconStoreError, ProtocolError, SnapshotError};

use mithril_common::chain_observer::ChainObserverError;
use mithril_common::digesters::{DigesterError, ImmutableFileListingError};
use mithril_common::store::stake_store::StakeStoreError;
use std::error::Error as StdError;
use std::io;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum RuntimeError {
    #[error("multi signer error")]
    MultiSigner(#[from] ProtocolError),

    #[error("beacon store error")]
    BeaconStore(#[from] BeaconStoreError),

    #[error("snapshotter error")]
    Snapshotter(#[from] SnapshotError),

    #[error("digester error")]
    Digester(#[from] DigesterError),

    #[error("snapshot store error")]
    SnapshotStore(#[from] SnapshotStoreError),

    #[error("stake store error")]
    StakeStore(#[from] StakeStoreError),

    #[error("store error")]
    StoreError(#[from] StoreError),

    #[error("snapshot uploader error: {0}")]
    SnapshotUploader(String),

    #[error("snapshot build error")]
    SnapshotBuild(#[from] io::Error),

    #[error("immutable file scanning error")]
    ImmutableFile(#[from] ImmutableFileListingError),

    #[error("chain observer error")]
    ChainObserver(#[from] ChainObserverError),

    #[error("general error: {0}")]
    General(Box<dyn StdError + Sync + Send>),
}
