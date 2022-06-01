mod dumb_adapter;
mod fail_adapter;
mod jsonfile_store_adapter;
mod memory_adapter;
mod store_adapter;

pub use jsonfile_store_adapter::JsonFileStoreAdapter;
pub use memory_adapter::MemoryAdapter;
pub use store_adapter::*;

#[cfg(test)]
pub use fail_adapter::FailStoreAdapter;

#[cfg(test)]
pub use dumb_adapter::DumbStoreAdapter;
