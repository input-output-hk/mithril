use serde::{Deserialize, Serialize};

/// From message adapter trait
pub trait FromMessageAdapter<U, V> {
    /// Adapt entity to message
    fn adapt(from: U) -> V;
}

/// To message adapter trait
pub trait ToMessageAdapter<U, V>
where
    V: Serialize + for<'a> Deserialize<'a>,
{
    /// Adapt message to entity
    fn adapt(from: U) -> V;
}
