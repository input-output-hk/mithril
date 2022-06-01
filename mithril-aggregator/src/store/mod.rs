// TODO: Rename the module to avoid module inception 9https://rust-lang.github.io/rust-clippy/master/index.html#module_inception)
#[allow(clippy::module_inception)]
pub mod adapter;
mod certificate_store;
mod pending_certificate_store;
mod verification_key_store;

pub use adapter::{AdapterError, JsonFileStoreAdapter, MemoryAdapter, StoreAdapter};
pub use certificate_store::{CertificateStore, StoreError as CertificateStoreError};
pub use pending_certificate_store::CertificatePendingStore;

#[cfg(test)]
pub use adapter::{DumbStoreAdapter, FailStoreAdapter};
