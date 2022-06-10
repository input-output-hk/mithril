pub mod adapter;
mod certificate_store;
mod error;
mod pending_certificate_store;
mod stake_store;
mod verification_key_store;

pub use adapter::{AdapterError, JsonFileStoreAdapter, MemoryAdapter, StoreAdapter};
pub use certificate_store::CertificateStore;
pub use error::StoreError;
pub use pending_certificate_store::CertificatePendingStore;
pub use stake_store::{StakeStore, StakeStoreError, StakeStorer};
pub use verification_key_store::{
    VerificationKeyStore, VerificationKeyStoreError, VerificationKeyStoreTrait,
};

#[cfg(test)]
pub use adapter::{DumbStoreAdapter, FailStoreAdapter};
