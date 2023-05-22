//! Services
//! This module contains the different services tied with their bounded context.
mod snapshot;

#[cfg(test)]
mod mock;

pub use snapshot::*;

#[cfg(test)]
pub use mock::*;
