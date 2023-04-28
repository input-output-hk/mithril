use async_trait::async_trait;
use serde::{de::DeserializeOwned, Serialize};
use std::fmt::Debug;

use crate::{entities::ProtocolMessage, StdResult};

/// Beacon trait
pub trait Beacon: Send + Sync {}

/// Signable is a trait for types that can be converted to a protocol message
pub trait Signable: Send + Sync {
    /// Compute protocol message
    fn compute_protocol_message(&self) -> StdResult<ProtocolMessage>;
}

/// Artifact is a trait for types that represent signed artifacts
pub trait Artifact: Serialize + DeserializeOwned + PartialEq + Debug + Send + Sync {}

/// SignableBuilder is trait for building a signable for a beacon
#[async_trait]
pub trait SignableBuilder<U, V>
where
    U: Beacon,
    V: Signable,
{
    /// Compute a signable
    async fn compute_signable(&self, beacon: U) -> StdResult<V>;
}
