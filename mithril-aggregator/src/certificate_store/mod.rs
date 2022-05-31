mod certificate_store;
mod jsonfile_store_adapter;
mod pending_certificate_store;
mod store_adapter;

pub use certificate_store::{CertificateStore, StoreError as CertificateStoreError};
pub use jsonfile_store_adapter::JsonFileStoreAdapter;
pub use pending_certificate_store::CertificatePendingStore;
pub use store_adapter::{AdapterError, StoreAdapter};

#[cfg(test)]
pub mod dumb_adapter;
pub mod fail_adapter;
