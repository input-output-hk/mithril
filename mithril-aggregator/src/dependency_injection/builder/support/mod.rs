//! Gathers builders for the low level components of the application such as compatibility
//! between nodes, the observability of the aggregator process, stores for the app data, and
//! the upkeep of the app.

mod compatibility;
mod observability;
mod sqlite;
mod stores;
mod upkeep;
