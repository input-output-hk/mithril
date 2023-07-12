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

mod signed_entity;
mod stake_distribution_service;
mod ticker_service;

pub use signed_entity::*;
pub use stake_distribution_service::*;
pub use ticker_service::*;
