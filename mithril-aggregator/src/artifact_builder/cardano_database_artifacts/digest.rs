use std::{path::Path, sync::Arc};

use mithril_common::{entities::DigestLocation, StdResult};

use crate::{ArtifactLocation, ArtifactUploader};

pub struct DigestArtifactBuilder {
    uploaders: Vec<Arc<dyn ArtifactUploader>>,
}

impl DigestArtifactBuilder {
    pub fn new(uploaders: Vec<Arc<dyn ArtifactUploader>>) -> Self {
        Self { uploaders }
    }

    pub async fn create_and_upload_archive(
        &self,
        db_directory: &Path,
    ) -> StdResult<Vec<DigestLocation>> {
        let mut locations = Vec::new();
        for uploader in &self.uploaders {
            // TODO: Temporary preparation work, `db_directory` is used as the digest json file path for now.
            let location = uploader.upload(db_directory).await?;
            match location {
                ArtifactLocation::URI(uri) => {
                    locations.push(DigestLocation::CloudStorage {
                        uri: uri.to_string(),
                    });
                } // TODO: how to distinguish between Aggregator and CloudStorage returned location?
            }
        }

        Ok(locations)
    }
}

#[cfg(test)]
mod tests {
    use crate::FakeArtifactUploader;

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
        let expected_uri = "uploaded_archive";
        let uploader = Arc::new(FakeArtifactUploader::new(ArtifactLocation::URI(
            expected_uri.parse().unwrap(),
        )));
        let builder = DigestArtifactBuilder::new(vec![uploader]);

        let locations = builder
            .create_and_upload_archive(Path::new("whatever"))
            .await
            .unwrap();

        match &locations[0] {
            DigestLocation::CloudStorage { uri } => {
                assert_eq!(expected_uri, uri);
            }
            _ => panic!("Expected CloudStorage location"),
        }
    }
}
