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
pub use client::AggregatorHttpClient;
pub use error::AggregatorHttpClientError;

pub(crate) const JSON_CONTENT_TYPE: reqwest::header::HeaderValue =
    reqwest::header::HeaderValue::from_static("application/json");

/// Aggregator-client result type
pub type AggregatorHttpClientResult<T> = Result<T, AggregatorHttpClientError>;
