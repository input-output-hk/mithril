use std::path::Path;

use async_trait::async_trait;
use warp::hyper::Uri;

use mithril_common::StdResult;

pub enum ArtifactLocation {
    URI(Uri),
}

#[async_trait]
pub trait ArtifactUploader: Sync + Send {
    async fn upload(&self, artifact_path: &Path) -> StdResult<ArtifactLocation>;
}
