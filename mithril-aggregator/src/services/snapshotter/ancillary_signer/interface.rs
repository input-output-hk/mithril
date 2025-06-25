use async_trait::async_trait;

use mithril_cardano_node_internal_database::entities::AncillaryFilesManifest;
use mithril_common::StdResult;
use mithril_common::crypto_helper::ManifestSignature;

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

#[cfg(test)]
impl MockAncillarySigner {
    pub(crate) fn that_succeeds_with_signature<T>(signature_to_return: T) -> Self
    where
        T: TryInto<ManifestSignature>,
        T::Error: std::fmt::Debug,
    {
        let expected_signature = signature_to_return.try_into().unwrap();
        let mut mock = MockAncillarySigner::new();
        mock.expect_compute_ancillary_manifest_signature()
            .returning(move |_| Ok(expected_signature));
        mock
    }

    pub(crate) fn that_fails_with_message<T: Into<String>>(message: T) -> Self {
        let error = anyhow::anyhow!("{}", message.into());
        let mut mock = MockAncillarySigner::new();
        mock.expect_compute_ancillary_manifest_signature()
            .return_once(move |_| Err(error));
        mock
    }
}
