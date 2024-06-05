//! # Services
//!
//! This module regroups services. Services are adapters in charge of the different  bounded contexts of the application:
//!
//! * Ticker: provides the time of the blockchain
//! * StakeEntity: fetches Cardano stake distribution information
//! * Certifier: registers signers and create certificates once ready
//! * SignedEntity: provides information about signed entities.
//!
//! Each service is defined by a public API (a trait) that is used in the controllers (runtimes).

mod cardano_transactions_importer;
mod certifier;
mod epoch_service;
mod message;
mod prover;
mod signed_entity;
mod stake_distribution;

pub use cardano_transactions_importer::*;
pub use certifier::*;
pub use epoch_service::*;
pub use message::*;
pub use prover::*;
pub use signed_entity::*;
pub use stake_distribution::*;
