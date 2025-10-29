#![warn(missing_docs)]
//! This crate provides mechanisms to discover aggregators in a Mithril network.

mod interface;
mod model;
pub mod test;

pub use interface::AggregatorDiscoverer;
pub use model::{AggregatorEndpoint, MithrilNetwork};
