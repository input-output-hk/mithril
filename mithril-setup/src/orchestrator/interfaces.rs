//! Discoverer module interface
use crate::orchestrator::store::Store;

/// Discoverer trait defines the interface for discovering configuration values
pub trait Discoverer {
    /// Run the discoverer to discover configuration values and get a new store with the discovered values
    fn run(store: Store) -> anyhow::Result<Store>;
}
