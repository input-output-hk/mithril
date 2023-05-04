use async_trait::async_trait;
use std::fmt::Debug;

use crate::{entities::ProtocolMessage, StdResult};

#[cfg(test)]
use mockall::automock;

/// Beacon trait
pub trait Beacon: Send + Sync {}

/// Signable is a trait for types that can be converted to a protocol message
pub trait Signable: Send + Sync {
    /// Compute protocol message
    fn compute_protocol_message(&self) -> StdResult<ProtocolMessage>;
}

/// Artifact is a trait for types that represent signed artifacts
#[typetag::serde(tag = "type")]
pub trait Artifact: Debug + Send + Sync {}

/// SignableBuilder is trait for building a signable for a beacon
#[cfg_attr(test, automock)]
#[async_trait]
pub trait SignableBuilder<U, V>: Send + Sync
where
    U: Beacon,
    V: Signable,
{
    /// Compute a signable
    async fn compute_signable(&self, beacon: U) -> StdResult<V>;
}
