use serde::{Deserialize, Serialize};

/// Message adapter trait
pub trait MessageAdapter<U, V>
where
    V: Serialize + for<'a> Deserialize<'a>,
{
    /// Adapt entity to message
    fn adapt(from: U) -> V;
}
