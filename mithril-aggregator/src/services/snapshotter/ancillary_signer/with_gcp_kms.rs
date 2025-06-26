use anyhow::Context;
use async_trait::async_trait;
use gcloud_kms::client::google_cloud_auth::credentials::CredentialsFile;
use gcloud_kms::client::{Client as GcpKmsClient, ClientConfig};
use gcloud_kms::grpc::kms::v1::AsymmetricSignRequest;
use slog::{debug, Logger};

use mithril_cardano_node_internal_database::entities::AncillaryFilesManifest;
use mithril_common::crypto_helper::ManifestSignature;
use mithril_common::StdResult;

use crate::services::ancillary_signer::{AncillarySigner, GcpCryptoKeyVersionResourceName};

/// Ancillary signer that uses a key stored in a Google Cloud Platform KMS account to sign
/// ancillary manifests.
pub struct AncillarySignerWithGcpKms {
    kms_client: GcpKmsClient,
    resource_name: GcpCryptoKeyVersionResourceName,
    logger: Logger,
}

impl AncillarySignerWithGcpKms {
    /// Creates a new instance of `AncillarySignerWithGcpKms`
    pub async fn new(
        resource_name: GcpCryptoKeyVersionResourceName,
        credentials_json_env_var: String,
        logger: Logger,
    ) -> StdResult<Self> {
        const BASE_ERROR_CONTEXT: &str =
            "Failed to create Google Cloud KMS client for Ancillary manifest signing";
        let unparsed_credentials_json = std::env::var(&credentials_json_env_var)
            .with_context(|| {
                format!("Environment variable `{credentials_json_env_var}` must be set",)
            })
            .with_context(|| BASE_ERROR_CONTEXT)?;
        let credentials_file = CredentialsFile::new_from_str(&unparsed_credentials_json)
            .await
            .with_context(|| BASE_ERROR_CONTEXT)?;
        let config = ClientConfig::default()
            .with_credentials(credentials_file)
            .await
            .with_context(|| BASE_ERROR_CONTEXT)?;
        let kms_client = GcpKmsClient::new(config).await.with_context(|| BASE_ERROR_CONTEXT)?;

        Ok(AncillarySignerWithGcpKms {
            kms_client,
            resource_name,
            logger,
        })
    }
}

#[async_trait]
impl AncillarySigner for AncillarySignerWithGcpKms {
    async fn compute_ancillary_manifest_signature(
        &self,
        manifest: &AncillaryFilesManifest,
    ) -> StdResult<ManifestSignature> {
        debug!(
            self.logger,
            ">> AncillarySignerWithGcpKms::compute_ancillary_manifest_signature"
        );
        let data = manifest.compute_hash();
        let signature_response = self
            .kms_client
            .asymmetric_sign(
                AsymmetricSignRequest {
                    name: self.resource_name.to_string(),
                    digest: None,
                    digest_crc32c: None,
                    data,
                    data_crc32c: None,
                },
                None,
            )
            .await
            .with_context(|| "Failed to sign the ancillary manifest with GCP KMS".to_string())?;

        ManifestSignature::from_bytes(&signature_response.signature).with_context(|| {
            "Failed to convert the signature response from GCP KMS to a ManifestSignature"
                .to_string()
        })
    }
}
