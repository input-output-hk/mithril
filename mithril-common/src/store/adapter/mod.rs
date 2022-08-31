//! Define a generic way to store data with the [Store Adapter][store_adapter::StoreAdapter], with
//! two main implementations ([in memory][MemoryAdapter] or [filesystem as json][JsonFileStoreAdapter])
//! and two more for testing ([a stub with one record][DumbStoreAdapter] and one which
//! [always fails][FailStoreAdapter]).

mod jsonfile_store_adapter;
mod memory_adapter;
mod sqlite_adapter;
mod store_adapter;

pub use jsonfile_store_adapter::JsonFileStoreAdapter;
pub use memory_adapter::MemoryAdapter;
pub use sqlite_adapter::{SQLiteAdapter, SQLiteResultIterator};
pub use store_adapter::*;

#[cfg(feature = "test-utils")]
mod dumb_adapter;
#[cfg(feature = "test-utils")]
pub use dumb_adapter::DumbStoreAdapter;
#[cfg(feature = "test-utils")]
mod fail_adapter;
#[cfg(feature = "test-utils")]
pub use fail_adapter::FailStoreAdapter;
