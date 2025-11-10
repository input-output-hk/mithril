#![warn(missing_docs)]
//! This crate provides mechanisms to discover aggregators in a Mithril network.

mod http_config_discoverer;
mod interface;
mod model;
pub mod test;

pub use capabilities_discoverer::CapableAggregatorDiscoverer;
pub use http_config_discoverer::HttpConfigAggregatorDiscoverer;
pub use interface::AggregatorDiscoverer;
pub use model::{AggregatorEndpoint, MithrilNetwork};
