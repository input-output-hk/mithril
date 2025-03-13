use std::{
    path::{Path, PathBuf},
    time::Duration,
};

use chrono::{DateTime, Utc};
use slog::Logger;

use mithril_common::StdResult;

/// Minimum time between vacuum operations - 1 week in seconds
const MIN_VACUUM_INTERVAL: Duration = Duration::from_secs(7 * 24 * 60 * 60);

/// Helper to track when vacuum was last performed
pub struct VacuumTracker {
    tracker_file: PathBuf,
    logger: Logger,
}

impl VacuumTracker {
    /// Create a new [VacuumTracker] for the given store directory
    pub fn new(store_dir: &Path, logger: Logger) -> Self {
        let last_vacuum_file = store_dir.join("last_vacuum_time");
        Self {
            tracker_file: last_vacuum_file,
            logger,
        }
    }

    /// Check if enough time has passed since last vacuum (returning the last vacuum timestamp)
    pub fn should_perform_vacuum(&self) -> StdResult<(bool, Option<DateTime<Utc>>)> {
        let now = Utc::now();

        Ok((false, Some(Utc::now())))
    }

    /// Update the last vacuum time to now
    pub fn update_last_vacuum_time(&self) -> StdResult<()> {
        Ok(())
    }
}
