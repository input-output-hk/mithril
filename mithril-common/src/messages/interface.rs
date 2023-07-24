use serde::{Deserialize, Serialize};

use crate::StdResult;

/// From message adapter trait
pub trait FromMessageAdapter<U, V> {
    /// Adapt entity to message
    fn adapt(from: U) -> V;
}

/// TryFrom message adapter trait
pub trait TryFromMessageAdapter<U, V> {
    /// Adapt entity to message
    fn try_adapt(from: U) -> StdResult<V>;
}

/// To message adapter trait
pub trait ToMessageAdapter<U, V>
where
    V: Serialize + for<'a> Deserialize<'a>,
{
    /// Adapt message to entity
    fn adapt(from: U) -> V;
}
