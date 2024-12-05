use std::path::Path;

use async_trait::async_trait;
use warp::hyper::Uri;

use mithril_common::StdResult;

use crate::artifact_uploader::{ArtifactLocation, ArtifactUploader};

pub struct FakeArtifactUploader;

#[async_trait]
impl ArtifactUploader for FakeArtifactUploader {
    async fn upload(&self, artifact_path: &Path) -> StdResult<ArtifactLocation> {
        let location = artifact_path.to_string_lossy().to_string().parse::<Uri>()?;

        Ok(ArtifactLocation::URI(location))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn temporary_try_convert() {
        let path = Path::new("test");
        let uploader = FakeArtifactUploader;
        let location = uploader.upload(path).await.unwrap();

        match location {
            ArtifactLocation::URI(uri) => {
                assert_eq!(uri.to_string(), "test");
            }
        }
    }
}
