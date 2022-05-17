use crate::dependency::{BeaconStoreWrapper, MultiSignerWrapper};
use crate::Snapshotter;
use mithril_common::fake_data;
use mithril_common::immutable_digester::ImmutableDigester;
use slog_scope::{debug, error, info};
use tokio::time::{sleep, Duration};

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

    async fn do_work(&self) -> Result<(), String> {
        let snapshotter = Snapshotter::new(self.db_directory.clone());
        let digester = ImmutableDigester::new(self.db_directory.clone(), slog_scope::logger());
        debug!("Making snapshot"; "directory" => &self.db_directory);

        match digester.compute_digest() {
            Ok(digest_result) => {
                let mut beacon_store = self.beacon_store.write().await;
                let mut beacon = fake_data::beacon();
                beacon.immutable_file_number = digest_result.last_immutable_file_number;
                beacon_store.set_current_beacon(beacon.clone()).await?;

                let mut multi_signer = self.multi_signer.write().await;
                let message = fake_data::digest(&beacon);
                multi_signer
                    .update_current_message(message)
                    .map_err(|e| e.to_string())?;

                snapshotter
                    .snapshot(digest_result.digest)
                    .await
                    .map_err(|e| e.to_string())?;
                Ok(())
            }
            Err(error) => {
                let mut beacon_store = self.beacon_store.write().await;
                beacon_store.reset_current_beacon().await?;
                Err(error.to_string())
            }
        }
    }
}
