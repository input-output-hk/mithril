use super::dependency::{BeaconStoreWrapper, MultiSignerWrapper, SnapshotStoreWrapper};
use super::{BeaconStoreError, ProtocolError, SnapshotError, Snapshotter};

use mithril_common::crypto_helper::Bytes;
use mithril_common::digesters::{Digester, DigesterError, ImmutableDigester};
use mithril_common::entities::{Beacon, Certificate};
use mithril_common::fake_data;

use crate::dependency::{CertificatePendingStoreWrapper, CertificateStoreWrapper};
use crate::snapshot_stores::SnapshotStoreError;
use crate::snapshot_uploaders::{SnapshotLocation, SnapshotUploader};
use chrono::{DateTime, Utc};
use hex::ToHex;
use mithril_common::entities::Snapshot;
use slog_scope::{debug, error, info, warn};
use std::fs::File;
use std::io;
use std::io::{Seek, SeekFrom};
use std::path::{Path, PathBuf};
use thiserror::Error;
use tokio::time::{sleep, Duration};

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

    #[error("certificate store error")]
    CertificateStore(String),

    #[error("snapshot uploader error: {0}")]
    SnapshotUploader(String),

    #[error("snapshot build error")]
    SnapshotBuild(#[from] io::Error),
}

/// AggregatorRuntime
pub struct AggregatorRuntime {
    /// Interval between each snapshot, in seconds
    interval: u32,

    /// Cardano network
    network: String,

    /// DB directory to snapshot
    db_directory: PathBuf,

    /// Directory to store snapshot
    snapshot_directory: PathBuf,

    /// Beacon store
    beacon_store: BeaconStoreWrapper,

    /// Multi signer
    multi_signer: MultiSignerWrapper,

    /// Snapshot store
    snapshot_store: SnapshotStoreWrapper,

    /// Snapshot uploader
    snapshot_uploader: Box<dyn SnapshotUploader>,

    /// Pending certificate store
    certificate_pending_store: CertificatePendingStoreWrapper,

    /// Certificate store
    certificate_store: CertificateStoreWrapper,
}

impl AggregatorRuntime {
    /// AggregatorRuntime factory
    // TODO: Fix this by implementing an Aggregator Config that implements the From trait for a general Config
    #[allow(clippy::too_many_arguments)]
    pub fn new(
        interval: u32,
        network: String,
        db_directory: PathBuf,
        snapshot_directory: PathBuf,
        beacon_store: BeaconStoreWrapper,
        multi_signer: MultiSignerWrapper,
        snapshot_store: SnapshotStoreWrapper,
        snapshot_uploader: Box<dyn SnapshotUploader>,
        certificate_pending_store: CertificatePendingStoreWrapper,
        certificate_store: CertificateStoreWrapper,
    ) -> Self {
        Self {
            interval,
            network,
            db_directory,
            snapshot_directory,
            beacon_store,
            multi_signer,
            snapshot_store,
            snapshot_uploader,
            certificate_pending_store,
            certificate_store,
        }
    }

    /// Run snapshotter loop
    pub async fn run(&self) {
        info!("Starting runtime");

        loop {
            if let Err(e) = self.do_work().await {
                error!("{:?}", e)
            }

            info!("Sleeping for {}", self.interval);
            sleep(Duration::from_millis(self.interval.into())).await;
        }
    }

    async fn do_work(&self) -> Result<(), RuntimeError> {
        let snapshotter =
            Snapshotter::new(self.db_directory.clone(), self.snapshot_directory.clone());
        let digester = ImmutableDigester::new(self.db_directory.clone(), slog_scope::logger());

        info!("Computing digest"; "db_directory" => self.db_directory.display());
        match digester.compute_digest() {
            Ok(digest_result) => {
                let mut beacon = fake_data::beacon();
                beacon.immutable_file_number = digest_result.last_immutable_file_number;
                let message = &digest_result.digest.clone().into_bytes();

                match self.manage_trigger_snapshot(message, &beacon).await {
                    Ok(Some(certificate)) => {
                        info!(
                            "Snapshotting immutables up to `{}` in an archive",
                            &beacon.immutable_file_number
                        );
                        let snapshot_name =
                            format!("{}.{}.tar.gz", self.network, &digest_result.digest);
                        let snapshot_path = snapshotter.snapshot(&snapshot_name)?;

                        info!("Uploading snapshot archive");
                        let uploaded_snapshot_location = self
                            .snapshot_uploader
                            .upload_snapshot(&snapshot_path)
                            .await
                            .map_err(RuntimeError::SnapshotUploader)?;

                        info!(
                            "Snapshot archive uploaded, location: `{}`",
                            &uploaded_snapshot_location
                        );

                        let new_snapshot = build_new_snapshot(
                            digest_result.digest,
                            certificate.hash.to_owned(),
                            &snapshot_path,
                            uploaded_snapshot_location,
                        )?;

                        info!("Storing snapshot data"; "snapshot" => format!("{:?}", new_snapshot));
                        let mut snapshot_store = self.snapshot_store.write().await;
                        snapshot_store.add_snapshot(new_snapshot).await?;

                        info!("Storing certificate data"; "certificate" => format!("{:?}", certificate));
                        let mut certificate_store = self.certificate_store.write().await;
                        certificate_store
                            .save(certificate)
                            .await
                            .map_err(|e| RuntimeError::CertificateStore(e.to_string()))?;

                        Ok(())
                    }
                    Ok(None) => Ok(()),
                    Err(err) => Err(err),
                }
            }
            Err(err) => {
                let mut beacon_store = self.beacon_store.write().await;
                beacon_store.reset_current_beacon().await?;
                Err(RuntimeError::Digester(err))
            }
        }
    }

    async fn manage_trigger_snapshot(
        &self,
        message: &Bytes,
        beacon: &Beacon,
    ) -> Result<Option<Certificate>, RuntimeError> {
        let mut multi_signer = self.multi_signer.write().await;
        let mut beacon_store = self.beacon_store.write().await;
        match multi_signer.get_multi_signature() {
            Ok(None) => {
                beacon_store.set_current_beacon(beacon.clone()).await?;
                multi_signer.update_current_message(message.to_owned())?;
                match multi_signer.create_multi_signature() {
                    Ok(Some(_)) => {
                        debug!(
                            "A multi signature has been created for message: {}",
                            multi_signer
                                .get_current_message()
                                .unwrap()
                                .encode_hex::<String>()
                        );
                        let previous_hash = "".to_string();
                        Ok(multi_signer.create_certificate(beacon.clone(), previous_hash)?)
                    }
                    Ok(None) => {
                        warn!("Not ready to create a multi signature: quorum is not reached yet");
                        Ok(None)
                    }
                    Err(e) => {
                        warn!("Error while creating a multi signature: {}", e);
                        Err(RuntimeError::MultiSigner(e))
                    }
                }
            }
            Ok(_) => {
                beacon_store.reset_current_beacon().await?;
                Ok(None)
            }
            Err(err) => {
                beacon_store.reset_current_beacon().await?;
                Err(RuntimeError::MultiSigner(err))
            }
        }
    }
}

fn build_new_snapshot(
    digest: String,
    certificate_hash: String,
    snapshot_filepath: &Path,
    uploaded_snapshot_location: SnapshotLocation,
) -> Result<Snapshot, RuntimeError> {
    let timestamp: DateTime<Utc> = Utc::now();
    let created_at = format!("{:?}", timestamp);

    let mut tar_gz = File::open(&snapshot_filepath)?;
    let size: u64 = tar_gz.seek(SeekFrom::End(0))?;

    Ok(Snapshot::new(
        digest,
        certificate_hash,
        size,
        created_at,
        vec![uploaded_snapshot_location],
    ))
}
