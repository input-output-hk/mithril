mod pending_certificate_store;
mod store_adapter;

use store_adapter::{AdapterError, StoreAdapter};
#[cfg(test)]
mod dumb_adapter;
#[cfg(test)]
use dumb_adapter::DumbStoreAdapter;
