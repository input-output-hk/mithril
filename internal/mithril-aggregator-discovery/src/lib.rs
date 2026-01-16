#![warn(missing_docs)]
//! This crate provides mechanisms to discover aggregators in a Mithril network.

mod capabilities_discoverer;
mod http_config_discoverer;
mod interface;
mod model;
mod rand_discoverer;
pub mod test;

pub use capabilities_discoverer::{CapableAggregatorDiscoverer, RequiredAggregatorCapabilities};
pub use http_config_discoverer::HttpConfigAggregatorDiscoverer;
pub use interface::AggregatorDiscoverer;
pub use model::{AggregatorEndpoint, AggregatorEndpointWithCapabilities};
pub use rand_discoverer::ShuffleAggregatorDiscoverer;
