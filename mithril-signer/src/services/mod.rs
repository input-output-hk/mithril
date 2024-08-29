//! # Services
//!
//! This module regroups services. Services are adapters in charge of the different  bounded contexts of the application:
//!
//! * Aggregator Client: communicate with the Aggregator
//! * Cardano Transactions: handle Cardano transactions (import, preload, etc.)
//! * Single Signer: create single signatures
//! * Upkeep: perform maintenance tasks
//!
//! Each service is defined by a public API (a trait) that is used in the controllers (runtimes).

mod aggregator_client;
mod cardano_transactions;
mod single_signer;
mod upkeep_service;

#[cfg(test)]
pub use aggregator_client::dumb::DumbAggregatorClient;
pub use aggregator_client::*;
pub use cardano_transactions::*;
pub use single_signer::*;
pub use upkeep_service::*;
