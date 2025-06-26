use std::path::Path;

use anyhow::Context;
use async_trait::async_trait;
use gcloud_storage::client::google_cloud_auth::credentials::CredentialsFile;
use gcloud_storage::client::{Client as GcpStorageClient, ClientConfig};
use gcloud_storage::http::object_access_controls::insert::{
    InsertObjectAccessControlRequest, ObjectAccessControlCreationConfig,
};
use gcloud_storage::http::object_access_controls::ObjectACLRole;
use gcloud_storage::http::objects::get::GetObjectRequest;
use gcloud_storage::http::objects::upload::{Media, UploadObjectRequest, UploadType};
use slog::{info, Logger};
use tokio_util::codec::{BytesCodec, FramedRead};

use mithril_common::entities::FileUri;
use mithril_common::StdResult;

use crate::file_uploaders::cloud_uploader::{gcp_percent_encode, CloudBackendUploader};
use crate::file_uploaders::CloudRemotePath;

/// Google Cloud Platform file uploader using `gcloud-storage` crate
pub struct GCloudBackendUploader {
    bucket: String,
    use_cdn_domain: bool,
    storage_client: GcpStorageClient,
    logger: Logger,
}

impl GCloudBackendUploader {
    /// Creates a new instance of `GCloudBackendUploader`
    pub async fn try_new(
        bucket: String,
        use_cdn_domain: bool,
        credentials_json_env_var: String,
        logger: Logger,
    ) -> StdResult<Self> {
        const BASE_ERROR_CONTEXT: &str =
            "Failed to create Google Cloud Storage client for file uploading";
        let unparsed_credentials_json = std::env::var(&credentials_json_env_var)
            .with_context(|| {
                format!("Environment variable `{credentials_json_env_var}` must be set")
            })
            .with_context(|| BASE_ERROR_CONTEXT)?;
        let credentials_file = CredentialsFile::new_from_str(&unparsed_credentials_json)
            .await
            .with_context(|| BASE_ERROR_CONTEXT)?;
        let config = ClientConfig::default()
            .with_credentials(credentials_file)
            .await
            .with_context(|| BASE_ERROR_CONTEXT)?;

        Ok(Self {
            bucket,
            use_cdn_domain,
            storage_client: GcpStorageClient::new(config),
            logger,
        })
    }
}

#[async_trait]
impl CloudBackendUploader for GCloudBackendUploader {
    async fn file_exists(&self, remote_file_path: &CloudRemotePath) -> StdResult<Option<FileUri>> {
        info!(self.logger, "Reading file metadata {remote_file_path}");
        let request = GetObjectRequest {
            bucket: self.bucket.clone(),
            object: remote_file_path.to_string(),
            ..Default::default()
        };

        let file_uri = match self
            .storage_client
            .get_object(&request)
            .await
            .with_context(|| "remote reading file metadata failure")
        {
            Ok(_) => {
                info!(self.logger, "Found file metadata {remote_file_path}");

                Some(remote_file_path.to_gcloud_storage_location(&self.bucket, self.use_cdn_domain))
            }
            Err(_) => {
                info!(self.logger, "Missing file metadata {remote_file_path}");

                None
            }
        };

        Ok(file_uri)
    }

    async fn upload_file(
        &self,
        local_file_path: &Path,
        remote_file_path: &CloudRemotePath,
    ) -> StdResult<FileUri> {
        info!(
            self.logger,
            "Uploading {} to {remote_file_path}",
            local_file_path.display()
        );
        let file = tokio::fs::File::open(local_file_path).await.with_context(|| {
            format!("Opening file failure, path: {}", local_file_path.display())
        })?;
        let stream = FramedRead::new(file, BytesCodec::new());

        self.storage_client
            .upload_streamed_object(
                &UploadObjectRequest {
                    bucket: self.bucket.clone(),
                    ..Default::default()
                },
                stream,
                &UploadType::Simple(Media::new(remote_file_path.to_string())),
            )
            .await
            .with_context(|| "Remote uploading failure")?;
        info!(
            self.logger,
            "Uploaded {} to {remote_file_path}",
            local_file_path.display()
        );

        Ok(remote_file_path.to_gcloud_storage_location(&self.bucket, self.use_cdn_domain))
    }

    async fn make_file_public(&self, remote_file_path: &CloudRemotePath) -> StdResult<()> {
        let new_bucket_access_control = ObjectAccessControlCreationConfig {
            entity: "allUsers".to_string(),
            role: ObjectACLRole::READER,
        };
        info!(
            self.logger, "Updating acl for {remote_file_path}";
            "inserted_acl" => ?new_bucket_access_control
        );
        self.storage_client
            .insert_object_access_control(&InsertObjectAccessControlRequest {
                bucket: self.bucket.clone(),
                object: gcp_percent_encode(&remote_file_path.to_string()),
                acl: new_bucket_access_control,
                ..Default::default()
            })
            .await
            .with_context(|| "Updating acl failure")?;

        info!(self.logger, "Updated acl for {remote_file_path}");

        Ok(())
    }
}
