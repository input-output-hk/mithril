use mithril_common::StdResult;

use crate::model::UnixTimestampProvider;

/// Fake implementation of a Unix current timestamp provider. For testing purposes only.
pub struct FakeUnixTimestampProvider(u64);

impl FakeUnixTimestampProvider {
    /// Creates a new `FakeUnixTimestampProvider` with the given timestamp.
    pub fn new(timestamp: u64) -> Self {
        Self(timestamp)
    }

    /// Computes the maximum timestamp that can be used with the given TTL and builds a new FakeUnixTimestampProvider.
    ///
    /// This is useful to create messages that are valid for the maximum possible time.
    pub fn max_timestamp_for_ttl(ttl: u16) -> Self {
        Self(u32::MAX as u64 - ttl as u64 - 1)
    }
}

impl UnixTimestampProvider for FakeUnixTimestampProvider {
    fn current_timestamp(&self) -> StdResult<u64> {
        Ok(self.0)
    }
}
