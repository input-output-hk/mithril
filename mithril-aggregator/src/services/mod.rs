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

mod certifier;
mod epoch_service;
mod http_message;
mod signed_entity;
mod stake_distribution;
mod ticker;

pub use certifier::*;
pub use epoch_service::*;
pub use http_message::*;
pub use signed_entity::*;
pub use stake_distribution::*;
pub use ticker::*;
