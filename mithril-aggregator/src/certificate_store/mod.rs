mod jsonfile_store_adapter;
mod pending_certificate_store;
mod store_adapter;

pub use jsonfile_store_adapter::JsonFileStoreAdapter;
pub use pending_certificate_store::CertificatePendingStore;
pub use store_adapter::{AdapterError, StoreAdapter};

#[cfg(test)]
mod dumb_adapter;
