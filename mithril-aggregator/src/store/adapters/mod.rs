mod adapter;
mod adapter_error;
mod dumb_adapter;
mod memory_adapter;

pub use adapter::Adapter;
pub use adapter_error::AdapterError;
pub use memory_adapter::MemoryAdapter;

#[cfg(test)]
pub use dumb_adapter::DumbAdapter;
