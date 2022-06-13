mod jsonfile_store_adapter;
mod memory_adapter;
mod store_adapter;

pub use jsonfile_store_adapter::JsonFileStoreAdapter;
pub use memory_adapter::MemoryAdapter;
pub use store_adapter::*;

mod dumb_adapter;
pub use dumb_adapter::DumbStoreAdapter;
mod fail_adapter;
pub use fail_adapter::FailStoreAdapter;
