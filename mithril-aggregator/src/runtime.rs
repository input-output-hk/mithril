use super::dependency::{BeaconStoreWrapper, MultiSignerWrapper};
use super::{BeaconStoreError, ProtocolError, SnapshotError, Snapshotter};

use mithril_common::fake_data;
use mithril_common::immutable_digester::{ImmutableDigester, ImmutableDigesterError};

use hex::ToHex;
use slog_scope::{debug, error, info, warn};
use thiserror::Error;
use tokio::time::{sleep, Duration};

#[derive(Error, Debug)]
pub enum RuntimeError {
    #[error("multi signer error")]
    MultiSignerError(#[from] ProtocolError),

    #[error("beacon store error")]
    BeaconStoreError(#[from] BeaconStoreError),

    #[error("snapshotter error")]
    SnapshotterError(#[from] SnapshotError),

    #[error("immutable digester error")]
    ImmutableDigesterError(#[from] ImmutableDigesterError),
}

/// AggregatorRuntime
pub struct AggregatorRuntime {
    /// Interval between each snapshot, in seconds
    interval: u32,

    /// DB directory to snapshot
    db_directory: String,

    /// Beacon store
    beacon_store: BeaconStoreWrapper,

    /// Multi signer
    multi_signer: MultiSignerWrapper,
}

impl AggregatorRuntime {
    /// AggregatorRuntime factory
    pub fn new(
        interval: u32,
        db_directory: String,
        beacon_store: BeaconStoreWrapper,
        multi_signer: MultiSignerWrapper,
    ) -> Self {
        Self {
            interval,
            db_directory,
            beacon_store,
            multi_signer,
        }
    }

    /// Run snapshotter loop
    pub async fn run(&self) {
        info!("Starting Snapshotter");

        loop {
            info!("Snapshotting");

            if let Err(e) = self.do_work().await {
                error!("{:?}", e)
            }

            info!("Sleeping for {}", self.interval);
            sleep(Duration::from_millis(self.interval.into())).await;
        }
    }

    async fn do_work(&self) -> Result<(), RuntimeError> {
        let snapshotter = Snapshotter::new(self.db_directory.clone());
        let digester = ImmutableDigester::new(self.db_directory.clone(), slog_scope::logger());
        debug!("Making snapshot"; "directory" => &self.db_directory);

        match digester.compute_digest() {
            Ok(digest_result) => {
                let mut beacon = fake_data::beacon();
                beacon.immutable_file_number = digest_result.last_immutable_file_number;
                let message = fake_data::digest(&beacon);
                let trigger_snapshot = {
                    let mut multi_signer = self.multi_signer.write().await;
                    let mut beacon_store = self.beacon_store.write().await;
                    match multi_signer.get_multi_signature(message.encode_hex::<String>()) {
                        Ok(None) => {
                            beacon_store.set_current_beacon(beacon.clone()).await?;
                            multi_signer.update_current_message(message)?;
                            match multi_signer.create_multi_signature() {
                                Ok(Some(_)) => {
                                    debug!(
                                        "A multi signature has been created for message: {}",
                                        multi_signer
                                            .get_current_message()
                                            .unwrap()
                                            .encode_hex::<String>()
                                    );
                                }
                                Ok(None) => {
                                    warn!("Not ready to create a multi signature: quorum is not reached yet");
                                }
                                Err(e) => {
                                    warn!("Error while creating a multi signature: {}", e);
                                }
                            }
                            Ok(true)
                        }
                        Ok(_) => {
                            beacon_store.reset_current_beacon().await?;
                            Ok(false)
                        }
                        Err(err) => {
                            beacon_store.reset_current_beacon().await?;
                            Err(err)
                        }
                    }
                };
                match trigger_snapshot {
                    Ok(true) => {
                        snapshotter.snapshot(digest_result.digest).await?;
                        Ok(())
                    }
                    Ok(false) => Ok(()),
                    Err(err) => Err(RuntimeError::MultiSignerError(err)),
                }
            }
            Err(err) => {
                let mut beacon_store = self.beacon_store.write().await;
                beacon_store.reset_current_beacon().await?;
                Err(RuntimeError::ImmutableDigesterError(err))
            }
        }
    }
}
