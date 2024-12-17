use async_trait::async_trait;
use std::{path::Path, sync::Arc};

use mithril_common::{entities::AncillaryLocation, StdResult};

use crate::{FileUploader, LocalUploader};

/// The [AncillaryFileUploader] trait allows identifying uploaders that return locations for ancillary archive files.
#[cfg_attr(test, mockall::automock)]
#[async_trait]
pub trait AncillaryFileUploader: Send + Sync {
    /// Uploads the archive at the given filepath and returns the location of the uploaded file.
    async fn upload(&self, filepath: &Path) -> StdResult<AncillaryLocation>;
}

#[async_trait]
impl AncillaryFileUploader for LocalUploader {
    async fn upload(&self, filepath: &Path) -> StdResult<AncillaryLocation> {
        let uri = FileUploader::upload(self, filepath).await?.into();

        Ok(AncillaryLocation::CloudStorage { uri })
    }
}

/// The [AncillaryArtifactBuilder] creates an ancillary archive from the cardano database directory (including ledger and volatile directories).
/// The archive is uploaded with the provided uploaders.
pub struct AncillaryArtifactBuilder {
    uploaders: Vec<Arc<dyn AncillaryFileUploader>>,
}

impl AncillaryArtifactBuilder {
    pub fn new(uploaders: Vec<Arc<dyn AncillaryFileUploader>>) -> Self {
        Self { uploaders }
    }

    pub async fn upload_archive(&self, db_directory: &Path) -> StdResult<Vec<AncillaryLocation>> {
        let mut locations = Vec::new();
        for uploader in &self.uploaders {
            // TODO: Temporary preparation work, `db_directory` is used as the ancillary archive path for now.
            let location = uploader.upload(db_directory).await?;
            locations.push(location);
        }

        Ok(locations)
    }
}

#[cfg(test)]
mod tests {
    use mockall::predicate::eq;

    use super::*;

    #[tokio::test]
    async fn upload_archive_should_return_empty_locations_with_no_uploader() {
        let builder = AncillaryArtifactBuilder::new(vec![]);

        let locations = builder.upload_archive(Path::new("whatever")).await.unwrap();

        assert!(locations.is_empty());
    }

    #[tokio::test]
    async fn upload_archive_should_return_all_uploaders_returned_locations() {
        let mut first_uploader = MockAncillaryFileUploader::new();
        first_uploader
            .expect_upload()
            .with(eq(Path::new("archive_path")))
            .times(1)
            .return_once(|_| {
                Ok(AncillaryLocation::CloudStorage {
                    uri: "an_uri".to_string(),
                })
            });

        let mut second_uploader = MockAncillaryFileUploader::new();
        second_uploader
            .expect_upload()
            .with(eq(Path::new("archive_path")))
            .times(1)
            .return_once(|_| {
                Ok(AncillaryLocation::CloudStorage {
                    uri: "another_uri".to_string(),
                })
            });

        let uploaders: Vec<Arc<dyn AncillaryFileUploader>> =
            vec![Arc::new(first_uploader), Arc::new(second_uploader)];

        let builder = AncillaryArtifactBuilder::new(uploaders);

        let locations = builder
            .upload_archive(Path::new("archive_path"))
            .await
            .unwrap();

        assert_eq!(
            locations,
            vec![
                AncillaryLocation::CloudStorage {
                    uri: "an_uri".to_string()
                },
                AncillaryLocation::CloudStorage {
                    uri: "another_uri".to_string()
                }
            ]
        );
    }
}
