use async_trait::async_trait;
use slog::{Logger, debug};

use mithril_cardano_node_internal_database::entities::AncillaryFilesManifest;
use mithril_common::StdResult;
use mithril_common::crypto_helper::{ManifestSignature, ManifestSigner};
use mithril_common::logging::LoggerExtensions;

use super::AncillarySigner;

/// Ancillary signer that uses an in memory secret key to sign the ancillary manifest.
pub struct AncillarySignerWithSecretKey {
    signer: ManifestSigner,
    logger: Logger,
}

impl AncillarySignerWithSecretKey {
    /// Create a new instance of `AncillarySignerWithSecretKey`.
    pub fn new(signer: ManifestSigner, logger: Logger) -> Self {
        Self {
            signer,
            logger: logger.new_with_component_name::<Self>(),
        }
    }
}

#[async_trait]
impl AncillarySigner for AncillarySignerWithSecretKey {
    async fn compute_ancillary_manifest_signature(
        &self,
        manifest: &AncillaryFilesManifest,
    ) -> StdResult<ManifestSignature> {
        debug!(self.logger, ">> compute_ancillary_manifest_signature");
        let manifest_hash = manifest.compute_hash();
        Ok(self.signer.sign(&manifest_hash))
    }
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeMap;
    use std::path::PathBuf;

    use crate::test::TestLogger;

    use super::*;

    #[tokio::test]
    async fn computed_signature_signs_manifest_hash() {
        let manifest = AncillaryFilesManifest::new_without_signature(BTreeMap::from([(
            PathBuf::from("path/whatever"),
            "whatever_hash".to_string(),
        )]));

        let signer = ManifestSigner::create_deterministic_signer();
        let verifier = signer.create_verifier();
        let ancillary_signer = AncillarySignerWithSecretKey::new(signer, TestLogger::stdout());

        let signature = ancillary_signer
            .compute_ancillary_manifest_signature(&manifest)
            .await
            .unwrap();

        verifier
            .verify(&manifest.compute_hash(), &signature)
            .expect("signature should be valid");
    }
}
