use async_trait::async_trait;
use std::fmt::Debug;

use crate::{entities::ProtocolMessage, StdResult};

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
    async fn compute_protocol_message(
        &self,
        beacon: U,
        seed_protocol_message: ProtocolMessage,
    ) -> StdResult<ProtocolMessage>;
}

/// SignableSeedBuilder is a trait for building a seed protocol message
#[cfg_attr(test, automock)]
#[async_trait]
pub trait SignableSeedBuilder: Send + Sync {
    /// Compute seed protocol message
    async fn compute_seed_protocol_message(&self) -> StdResult<ProtocolMessage>;
}
