use std::{path::Path, sync::Arc};

use mithril_common::{entities::ImmutablesLocation, StdResult};

use crate::{ArtifactLocation, ArtifactUploader};

pub struct ImmutableFilesArtifactBuilder {
    uploaders: Vec<Arc<dyn ArtifactUploader>>,
}

impl ImmutableFilesArtifactBuilder {
    pub fn new(uploaders: Vec<Arc<dyn ArtifactUploader>>) -> Self {
        Self { uploaders }
    }

    pub async fn create_and_upload_archive(
        &self,
        db_directory: &Path,
    ) -> StdResult<Vec<ImmutablesLocation>> {
        let mut locations = Vec::new();
        for uploader in &self.uploaders {
            // TODO: Temporary preparation work, `db_directory` is used as path to the new immutable files archives to upload for now.
            let location = uploader.upload(db_directory).await?;
            match location {
                ArtifactLocation::URI(uri) => {
                    locations.push(ImmutablesLocation::CloudStorage {
                        uri: uri.to_string(),
                    });
                }
            }
        }

        Ok(locations)
    }
}

#[cfg(test)]
mod tests {
    use crate::{artifact_uploaders::MockArtifactUploader, FakeArtifactUploader};

    use super::*;

    #[tokio::test]
    async fn create_and_upload_archive_should_return_empty_locations_with_no_artifact_uploader() {
        let builder = ImmutableFilesArtifactBuilder::new(vec![]);

        let locations = builder
            .create_and_upload_archive(Path::new("whatever"))
            .await
            .unwrap();

        assert!(locations.is_empty());
    }

    #[tokio::test]
    async fn create_and_upload_archive_should_return_uploader_cloud_storage_location() {
        let expected_uri = "uploaded_archive";
        let uploader = Arc::new(FakeArtifactUploader::new(ArtifactLocation::URI(
            expected_uri.parse().unwrap(),
        )));
        let builder = ImmutableFilesArtifactBuilder::new(vec![uploader]);

        let locations = builder
            .create_and_upload_archive(Path::new("whatever"))
            .await
            .unwrap();

        match &locations[0] {
            ImmutablesLocation::CloudStorage { uri } => {
                assert_eq!(expected_uri, uri);
            }
        }
    }

    #[tokio::test]
    async fn create_and_upload_archive_should_return_uploader_cloud_storage_location_with_mock() {
        let expected_uri = "uploaded_archive";

        let mut uploader = MockArtifactUploader::new();
        uploader
            .expect_upload()
            .returning(move |_| Ok(ArtifactLocation::URI(expected_uri.parse().unwrap())));

        let builder = ImmutableFilesArtifactBuilder::new(vec![Arc::new(uploader)]);

        let locations = builder
            .create_and_upload_archive(Path::new("whatever"))
            .await
            .unwrap();

        match &locations[0] {
            ImmutablesLocation::CloudStorage { uri } => {
                assert_eq!(expected_uri, uri);
            }
        }
    }
}
