#![warn(missing_docs)]
//! Provides the [TickerService] that reads time information from the chain and helps
//! create beacons for every message type.

mod ticker_service;

pub use ticker_service::*;
