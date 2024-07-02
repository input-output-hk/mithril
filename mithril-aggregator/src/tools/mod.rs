mod certificates_hash_migrator;
mod digest_helpers;
mod era;
mod genesis;
#[cfg(test)]
pub mod mocks;
mod remote_file_uploader;
mod signer_importer;

pub use certificates_hash_migrator::CertificatesHashMigrator;
pub use digest_helpers::extract_digest_from_path;
pub use era::EraTools;
pub use genesis::{GenesisTools, GenesisToolsDependency};
pub use remote_file_uploader::{GcpFileUploader, RemoteFileUploader};
pub use signer_importer::{
    CExplorerSignerRetriever, SignersImporter, SignersImporterPersister, SignersImporterRetriever,
};

#[cfg(test)]
pub use remote_file_uploader::MockRemoteFileUploader;

/// Downcast the error to the specified error type and check if the error satisfies the condition.
pub(crate) fn downcast_check<E>(
    error: &mithril_common::StdError,
    check: impl Fn(&E) -> bool,
) -> bool
where
    E: std::fmt::Display + std::fmt::Debug + Send + Sync + 'static,
{
    if let Some(inner_error) = error.downcast_ref::<E>() {
        return check(inner_error);
    }
    false
}
