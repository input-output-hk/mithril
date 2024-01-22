//! Define a generic way to store data with the [Store Adapters][adapter], and the [StakeStorer]
//! to store stakes.

pub mod adapter;
mod stake_store;
mod store_pruner;
mod transaction_store;

pub use stake_store::{StakeStore, StakeStorer};
pub use store_pruner::StorePruner;
#[cfg(test)]
pub use transaction_store::MockTransactionStore;
pub use transaction_store::TransactionStore;
