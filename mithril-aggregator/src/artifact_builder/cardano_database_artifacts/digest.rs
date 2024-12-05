use std::{path::Path, sync::Arc};

use mithril_common::{entities::DigestLocation, StdResult};

use crate::FileUploader;

pub struct DigestArtifactBuilder {
    uploaders: Vec<Arc<dyn FileUploader>>,
}

impl DigestArtifactBuilder {
    pub fn new(uploaders: Vec<Arc<dyn FileUploader>>) -> Self {
        Self { uploaders }
    }

    pub async fn create_and_upload_archive(
        &self,
        db_directory: &Path,
    ) -> StdResult<Vec<DigestLocation>> {
        let mut locations = Vec::new();
        for uploader in &self.uploaders {
            // TODO: Temporary preparation work, `db_directory` is used as the digest json file path for now.
            let uri = uploader.upload(db_directory).await?.into();
            locations.push(DigestLocation::CloudStorage { uri });
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
        let builder = DigestArtifactBuilder::new(vec![]);

        let locations = builder
            .create_and_upload_archive(Path::new("whatever"))
            .await
            .unwrap();

        assert!(locations.is_empty());
    }

    #[tokio::test]
    async fn create_and_upload_archive_should_return_uploader_cloud_storage_location() {
        let uploader = Arc::new(DumbUploader::new());
        let builder = DigestArtifactBuilder::new(vec![uploader]);

        let locations = builder
            .create_and_upload_archive(Path::new("whatever"))
            .await
            .unwrap();

        match &locations[0] {
            DigestLocation::CloudStorage { uri } => {
                assert_eq!("whatever", uri);
            }
            _ => panic!("Expected CloudStorage location"),
        }
    }
}
