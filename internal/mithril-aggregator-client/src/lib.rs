#![warn(missing_docs)]
//! This crate provides a client to request data from a Mithril Aggregator.
//!

mod builder;
mod client;
mod error;
pub mod query;
#[cfg(test)]
mod test;

pub use builder::AggregatorClientBuilder;
pub use client::AggregatorClient;
pub use error::AggregatorClientError;

pub(crate) const JSON_CONTENT_TYPE: reqwest::header::HeaderValue =
    reqwest::header::HeaderValue::from_static("application/json");

pub type AggregatorClientResult<T> = Result<T, error::AggregatorClientError>;
