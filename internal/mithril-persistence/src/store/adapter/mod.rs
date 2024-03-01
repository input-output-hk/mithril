//! Define a generic way to store data with the [Store Adapter][store_adapter::StoreAdapter], with
//! an adapter [in memory][MemoryAdapter] and another [sqlite][SQLiteAdapter].

mod memory_adapter;
mod sqlite_adapter;
mod store_adapter;

pub use memory_adapter::MemoryAdapter;
pub use sqlite_adapter::{SQLiteAdapter, SQLiteResultIterator};
pub use store_adapter::*;

mod dumb_adapter;
pub use dumb_adapter::DumbStoreAdapter;
mod fail_adapter;
pub use fail_adapter::FailStoreAdapter;
