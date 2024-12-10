mod certificates_hash_migrator;
mod digest_helpers;
mod era;
mod gcp_file_uploader;
mod genesis;
#[cfg(test)]
pub mod mocks;
mod signer_importer;
mod single_signature_authenticator;

pub use certificates_hash_migrator::CertificatesHashMigrator;
pub use digest_helpers::extract_digest_from_path;
pub use era::EraTools;
pub use gcp_file_uploader::GcpFileUploader;
pub use genesis::{GenesisTools, GenesisToolsDependency};
pub use signer_importer::{
    CExplorerSignerRetriever, SignersImporter, SignersImporterPersister, SignersImporterRetriever,
};
pub use single_signature_authenticator::*;

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
