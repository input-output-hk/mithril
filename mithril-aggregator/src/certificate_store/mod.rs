// TODO: Rename the module to avoid module inception 9https://rust-lang.github.io/rust-clippy/master/index.html#module_inception)
#[allow(clippy::module_inception)]
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

#[cfg(test)]
pub mod fail_adapter;
