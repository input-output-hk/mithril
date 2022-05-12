use crate::Snapshotter;
use mithril_common::immutable_digester::ImmutableDigester;
use slog_scope::{error, info};
use tokio::time::{sleep, Duration};

/// AggregatorRuntime
pub struct AggregatorRuntime {
    /// Interval between each snapshot, in seconds
    interval: u32,

    /// DB directory to snapshot
    db_directory: String,
}

impl AggregatorRuntime {
    /// AggregatorRuntime factory
    pub fn new(interval: u32, db_directory: String) -> Self {
        Self {
            interval,
            db_directory,
        }
    }

    /// Run snapshotter loop
    pub async fn run(&self) {
        info!("Starting Snapshotter");
        let snapshotter = Snapshotter::new(self.db_directory.clone());
        let digester = ImmutableDigester::new(self.db_directory.clone(), slog_scope::logger());

        loop {
            info!("Snapshotting");

            match digester.compute_digest() {
                Ok(digest_result) => {
                    if let Err(e) = snapshotter.snapshot(digest_result.digest).await {
                        error!("{:?}", e)
                    }
                }
                Err(e) => {
                    error!("{:?}", e)
                }
            };

            info!("Sleeping for {}", self.interval);
            sleep(Duration::from_millis(self.interval.into())).await;
        }
    }
}
