//! Define a generic way to store data with the [Store Adapters][adapter], and the [StakeStorer]
//! to store stakes.

pub mod adapter;
pub mod adapter_migration;
mod error;
mod stake_store;

pub use error::StoreError;
pub use stake_store::{StakeStore, StakeStorer};
