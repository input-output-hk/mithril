use chrono::Utc;
use std::path::PathBuf;

/// Trait to provide a temporary directory path
#[cfg_attr(test, mockall::automock)]
pub trait TempDirectoryProvider: Send + Sync {
    /// Provides a temporary directory path
    fn temp_dir(&self) -> PathBuf;
}

/// Timestamp based temporary directory provider implementation
pub struct TimestampTempDirectoryProvider {
    temp_dir: PathBuf,
}

impl TimestampTempDirectoryProvider {
    /// Constructs a new `TimestampTempDirectoryProvider`.
    pub fn new(prefix: &str) -> Self {
        let temp_dir = std::env::temp_dir().join(format!(
            "mithril_client_{prefix}_{}",
            Utc::now().timestamp_micros()
        ));
        Self { temp_dir }
    }
}

impl TempDirectoryProvider for TimestampTempDirectoryProvider {
    fn temp_dir(&self) -> PathBuf {
        self.temp_dir.clone()
    }
}
