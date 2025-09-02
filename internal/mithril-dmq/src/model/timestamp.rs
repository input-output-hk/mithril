use std::time::{SystemTime, UNIX_EPOCH};

use mithril_common::StdResult;

/// Provides the current timestamp in seconds since the UNIX epoch.
#[cfg_attr(test, mockall::automock)]
pub trait UnixTimestampProvider: Send + Sync {
    /// Returns the current timestamp in seconds since the UNIX epoch.
    fn current_timestamp(&self) -> StdResult<u64>;
}

/// Provides the current timestamp in seconds since the UNIX epoch from the system.
pub struct SystemUnixTimestampProvider;

impl UnixTimestampProvider for SystemUnixTimestampProvider {
    fn current_timestamp(&self) -> StdResult<u64> {
        Ok(SystemTime::now().duration_since(UNIX_EPOCH)?.as_secs())
    }
}
