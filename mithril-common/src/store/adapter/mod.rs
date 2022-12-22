//! Define a generic way to store data with the [Store Adapter][store_adapter::StoreAdapter], with
//! an adapter [in memory][MemoryAdapter] and another [sqlite][SQLiteAdapter].

mod memory_adapter;
mod sqlite_adapter;
mod store_adapter;

pub use memory_adapter::MemoryAdapter;
pub use sqlite_adapter::{SQLiteAdapter, SQLiteResultIterator};
pub use store_adapter::*;

#[cfg(any(test, feature = "test_only"))]
mod dumb_adapter;
#[cfg(any(test, feature = "test_only"))]
pub use dumb_adapter::DumbStoreAdapter;
#[cfg(any(test, feature = "test_only"))]
mod fail_adapter;
#[cfg(any(test, feature = "test_only"))]
pub use fail_adapter::FailStoreAdapter;
