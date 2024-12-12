use std::{path::Path, sync::Arc};

use mithril_common::{
    entities::{AncillaryLocation, AncillaryLocationDiscriminants},
    StdResult,
};

use crate::FileUploader;

pub struct AncillaryArtifactBuilder {
    uploaders: Vec<Arc<dyn FileUploader>>,
}

impl AncillaryArtifactBuilder {
    pub fn new(uploaders: Vec<Arc<dyn FileUploader>>) -> Self {
        Self { uploaders }
    }

    pub async fn create_and_upload_archive(
        &self,
        db_directory: &Path,
    ) -> StdResult<Vec<AncillaryLocation>> {
        let mut locations = Vec::new();
        for uploader in &self.uploaders {
            // TODO: Temporary preparation work, `db_directory` is used as the ancillary archive path for now.
            let uri = uploader.upload(db_directory).await?.into();
            locations.push(AncillaryLocation::CloudStorage { uri });
        }

        Ok(locations)
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::entities::AncillaryLocationDiscriminants;

    use crate::{file_uploaders::TypedFileUploader, DumbUploader};

    use super::*;

    #[tokio::test]
    async fn create_and_upload_archive_should_return_empty_locations_with_no_artifact_uploader() {
        let builder = AncillaryArtifactBuilder::new(vec![]);

        let locations = builder
            .create_and_upload_archive(Path::new("whatever"))
            .await
            .unwrap();

        assert!(locations.is_empty());
    }

    #[tokio::test]
    async fn create_and_upload_archive_should_return_uploader_cloud_storage_location() {
        let uploader = Arc::new(TypedFileUploader::new(
            AncillaryLocationDiscriminants::CloudStorage,
            DumbUploader::new(),
        ));
        let builder = AncillaryArtifactBuilder::new(vec![uploader]);

        let locations = builder
            .create_and_upload_archive(Path::new("whatever"))
            .await
            .unwrap();

        match &locations[0] {
            AncillaryLocation::CloudStorage { uri } => {
                assert_eq!("whatever", uri);
            }
        }
    }
}
