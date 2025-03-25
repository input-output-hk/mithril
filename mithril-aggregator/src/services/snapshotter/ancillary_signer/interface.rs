use async_trait::async_trait;

use mithril_common::crypto_helper::ManifestSignature;
use mithril_common::entities::AncillaryFilesManifest;
use mithril_common::StdResult;

#[cfg_attr(test, mockall::automock)]
#[async_trait]
/// Define how to sign the ancillary manifest.
pub trait AncillarySigner: Sync + Send {
    /// Compute the signature of the ancillary manifest.
    async fn compute_ancillary_manifest_signature(
        &self,
        manifest: &AncillaryFilesManifest,
    ) -> StdResult<ManifestSignature>;
}
