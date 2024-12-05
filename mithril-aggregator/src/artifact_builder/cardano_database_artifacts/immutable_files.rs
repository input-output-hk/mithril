use std::{path::Path, sync::Arc};

use mithril_common::{entities::ImmutablesLocation, StdResult};

use crate::FileUploader;

pub struct ImmutableFilesArtifactBuilder {
    uploaders: Vec<Arc<dyn FileUploader>>,
}

impl ImmutableFilesArtifactBuilder {
    pub fn new(uploaders: Vec<Arc<dyn FileUploader>>) -> Self {
        Self { uploaders }
    }

    pub async fn create_and_upload_archive(
        &self,
        db_directory: &Path,
    ) -> StdResult<Vec<ImmutablesLocation>> {
        let mut locations = Vec::new();
        for uploader in &self.uploaders {
            // TODO: Temporary preparation work, `db_directory` is used as path to the new immutable files archives to upload for now.
            let uri = uploader.upload(db_directory).await?.into();
            locations.push(ImmutablesLocation::CloudStorage { uri });
        }

        Ok(locations)
    }
}

#[cfg(test)]
mod tests {
    use crate::DumbUploader;

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
        let uploader = Arc::new(DumbUploader::new());
        let builder = ImmutableFilesArtifactBuilder::new(vec![uploader]);

        let locations = builder
            .create_and_upload_archive(Path::new("whatever"))
            .await
            .unwrap();

        match &locations[0] {
            ImmutablesLocation::CloudStorage { uri } => {
                assert_eq!("whatever", uri);
            }
        }
    }
}
