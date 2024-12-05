use std::path::Path;

use async_trait::async_trait;

use mithril_common::StdResult;

use crate::artifact_uploaders::{ArtifactLocation, ArtifactUploader};

pub struct FakeArtifactUploader {
    uploaded_artifact_location: ArtifactLocation,
}

impl FakeArtifactUploader {
    pub fn new(uploaded_artifact_location: ArtifactLocation) -> Self {
        Self {
            uploaded_artifact_location,
        }
    }
}

#[async_trait]
impl ArtifactUploader for FakeArtifactUploader {
    async fn upload(&self, _artifact_path: &Path) -> StdResult<ArtifactLocation> {
        let location = self.uploaded_artifact_location.clone();

        Ok(location)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn upload_should_return_inner_uploaded_artifact_location() {
        let uploader =
            FakeArtifactUploader::new(ArtifactLocation::URI("test_path".parse().unwrap()));
        let location = uploader.upload(Path::new("whatever")).await.unwrap();

        assert_eq!(
            ArtifactLocation::URI("test_path".parse().unwrap()),
            location
        );
    }
}
