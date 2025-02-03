use async_trait::async_trait;
use std::fmt::Debug;

use crate::{
    entities::{
        BlockNumber, CardanoDatabaseSnapshot, CardanoDbBeacon, CardanoStakeDistribution,
        CardanoTransactionsSnapshot, Epoch, MithrilStakeDistribution, ProtocolMessage,
        ProtocolMessagePartValue, Snapshot,
    },
    StdResult,
};

#[cfg(test)]
use mockall::automock;

/// Beacon trait
pub trait Beacon: Send + Sync {}

/// Artifact is a trait for types that represent signed artifacts
#[typetag::serde(tag = "type")]
pub trait Artifact: Debug + Send + Sync {
    /// Get artifact identifier
    fn get_id(&self) -> String;
}

/// SignableBuilder is a trait for building a protocol message for a beacon
#[cfg_attr(test, automock)]
#[async_trait]
pub trait SignableBuilder<U>: Send + Sync
where
    U: Beacon,
{
    /// Compute a protocol message
    async fn compute_protocol_message(&self, beacon: U) -> StdResult<ProtocolMessage>;
}

/// SignableSeedBuilder is a trait for building seed protocol message part values
#[cfg_attr(test, automock)]
#[async_trait]
pub trait SignableSeedBuilder: Send + Sync {
    /// Compute next aggregate verification key protocol message part value
    async fn compute_next_aggregate_verification_key(&self) -> StdResult<ProtocolMessagePartValue>;

    /// Compute next protocol parameters protocol message part value
    async fn compute_next_protocol_parameters(&self) -> StdResult<ProtocolMessagePartValue>;

    /// Compute current epoch protocol message part value
    async fn compute_current_epoch(&self) -> StdResult<ProtocolMessagePartValue>;
}

impl Beacon for BlockNumber {}

impl Beacon for CardanoDbBeacon {}

impl Beacon for Epoch {}

#[typetag::serde]
impl Artifact for CardanoDatabaseSnapshot {
    fn get_id(&self) -> String {
        self.hash.clone()
    }
}

#[typetag::serde]
impl Artifact for CardanoStakeDistribution {
    fn get_id(&self) -> String {
        self.hash.clone()
    }
}

#[typetag::serde]
impl Artifact for CardanoTransactionsSnapshot {
    fn get_id(&self) -> String {
        self.hash.clone()
    }
}

#[typetag::serde]
impl Artifact for MithrilStakeDistribution {
    fn get_id(&self) -> String {
        self.hash.clone()
    }
}

#[typetag::serde]
impl Artifact for Snapshot {
    fn get_id(&self) -> String {
        self.digest.clone()
    }
}
