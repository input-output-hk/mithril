use std::{
    fs,
    path::{Path, PathBuf},
};

use anyhow::Context;
use chrono::{DateTime, TimeDelta, Utc};
use slog::{Logger, debug, info};

use mithril_common::StdResult;

const LAST_VACUUM_TIME_FILENAME: &str = "last_vacuum_time";

type LastVacuumTime = DateTime<Utc>;

/// Helper to track when vacuum was last performed
#[derive(Debug, Clone)]
pub struct VacuumTracker {
    tracker_file: PathBuf,
    min_interval: TimeDelta,
    logger: Logger,
}

impl VacuumTracker {
    /// Create a new [VacuumTracker] for the given store directory
    pub fn new(store_dir: &Path, interval: TimeDelta, logger: Logger) -> Self {
        let last_vacuum_file = store_dir.join(LAST_VACUUM_TIME_FILENAME);

        Self {
            tracker_file: last_vacuum_file,
            min_interval: interval,
            logger,
        }
    }

    /// Check if enough time has passed since last vacuum (returning the last vacuum timestamp)
    pub fn check_vacuum_needed(&self) -> StdResult<(bool, Option<LastVacuumTime>)> {
        if !self.tracker_file.exists() {
            debug!(
                self.logger,
                "No previous vacuum timestamp found, vacuum can be performed"
            );
            return Ok((true, None));
        }

        let last_vacuum = fs::read_to_string(&self.tracker_file).with_context(|| {
            format!(
                "Failed to read vacuum timestamp file: {:?}",
                self.tracker_file
            )
        })?;
        let last_vacuum = DateTime::parse_from_rfc3339(&last_vacuum)?.with_timezone(&Utc);

        let duration_since_last = Utc::now() - (last_vacuum);

        let should_vacuum = duration_since_last >= self.min_interval;
        let info_message = if should_vacuum {
            "Sufficient time has passed since last vacuum"
        } else {
            "Not enough time elapsed since last vacuum"
        };

        info!(
            self.logger,
            "{}", info_message;
            "last_vacuum" => last_vacuum.to_string(),
            "elapsed_days" => duration_since_last.num_days(),
            "min_interval_days" => self.min_interval.num_days()
        );

        Ok((should_vacuum, Some(last_vacuum)))
    }

    /// Update the last vacuum time to now
    pub fn update_last_vacuum_time(&self) -> StdResult<LastVacuumTime> {
        let timestamp = Utc::now();

        fs::write(&self.tracker_file, timestamp.to_rfc3339()).with_context(|| {
            format!(
                "Failed to write to last vacuum time file: {:?}",
                self.tracker_file
            )
        })?;

        Ok(timestamp)
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::temp_dir_create;

    use crate::test_tools::TestLogger;

    use super::*;

    const DUMMY_INTERVAL: TimeDelta = TimeDelta::milliseconds(99);

    #[test]
    fn update_last_vacuum_time_creates_file_with_current_timestamp() {
        let tracker = VacuumTracker::new(&temp_dir_create!(), DUMMY_INTERVAL, TestLogger::stdout());

        assert!(!tracker.tracker_file.exists());

        let saved_timestamp = tracker.update_last_vacuum_time().unwrap();
        let approximative_expected_saved_timestamp = Utc::now();

        let vacuum_file_content = fs::read_to_string(tracker.tracker_file).unwrap();
        let timestamp_retrieved = DateTime::parse_from_rfc3339(&vacuum_file_content).unwrap();
        let diff = timestamp_retrieved
            .signed_duration_since(approximative_expected_saved_timestamp)
            .num_milliseconds();
        assert!(diff < 1);
        assert_eq!(timestamp_retrieved, saved_timestamp);
    }

    #[test]
    fn update_last_vacuum_time_overwrites_previous_timestamp() {
        let tracker = VacuumTracker::new(&temp_dir_create!(), DUMMY_INTERVAL, TestLogger::stdout());

        let initial_saved_timestamp = tracker.update_last_vacuum_time().unwrap();
        let last_saved_timestamp = tracker.update_last_vacuum_time().unwrap();

        let vacuum_file_content = fs::read_to_string(tracker.tracker_file).unwrap();
        let timestamp_retrieved = DateTime::parse_from_rfc3339(&vacuum_file_content).unwrap();
        assert!(last_saved_timestamp > initial_saved_timestamp);
        assert_eq!(timestamp_retrieved, last_saved_timestamp);
    }

    #[test]
    fn update_last_vacuum_time_fails_on_write_error() {
        let dir_not_exist = Path::new("path-does-not-exist");
        let tracker = VacuumTracker::new(dir_not_exist, DUMMY_INTERVAL, TestLogger::stdout());

        tracker
            .update_last_vacuum_time()
            .expect_err("Update last vacuum time should fail when error while writing to file");
    }

    #[test]
    fn check_vacuum_needed_returns_true_when_no_previous_record() {
        let tracker = VacuumTracker::new(&temp_dir_create!(), DUMMY_INTERVAL, TestLogger::stdout());

        let (is_vacuum_needed, last_timestamp) = tracker.check_vacuum_needed().unwrap();

        assert!(is_vacuum_needed);
        assert!(last_timestamp.is_none());
    }

    #[test]
    fn check_vacuum_needed_returns_true_after_interval_elapsed() {
        let min_interval = TimeDelta::milliseconds(10);
        let tracker = VacuumTracker::new(&temp_dir_create!(), min_interval, TestLogger::stdout());

        let saved_timestamp = Utc::now() - TimeDelta::milliseconds(10);
        fs::write(tracker.clone().tracker_file, saved_timestamp.to_rfc3339()).unwrap();

        let (is_vacuum_needed, last_timestamp) = tracker.check_vacuum_needed().unwrap();

        assert!(is_vacuum_needed);
        assert_eq!(last_timestamp, Some(saved_timestamp));
    }

    #[test]
    fn check_vacuum_needed_returns_false_within_interval() {
        let min_interval = TimeDelta::minutes(2);
        let tracker = VacuumTracker::new(&temp_dir_create!(), min_interval, TestLogger::stdout());

        let saved_timestamp = tracker.update_last_vacuum_time().unwrap();

        let (is_vacuum_needed, last_timestamp) = tracker.check_vacuum_needed().unwrap();

        assert!(!is_vacuum_needed);
        assert_eq!(last_timestamp, Some(saved_timestamp));
    }
}
