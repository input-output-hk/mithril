//! Define a generic way to store data with the [Store Adapters][adapter], and the [StakeStorer]
//! to store stakes.

pub mod adapter;
pub mod adapter_migration;
mod error;
mod limit_key_store;
mod stake_store;

pub use error::StoreError;
pub use limit_key_store::LimitKeyStore;
pub use stake_store::{StakeStore, StakeStorer};
