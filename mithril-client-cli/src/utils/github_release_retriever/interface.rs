use async_trait::async_trait;

use mithril_client::MithrilResult;

use super::model::GitHubRelease;

/// Trait for interacting with the GitHub API to retrieve Cardano node release.
#[cfg_attr(test, mockall::automock)]
#[async_trait]
pub trait GitHubReleaseRetriever {
    /// Retrieves a release by its tag.
    async fn get_release_by_tag(
        &self,
        owner: &str,
        repo: &str,
        tag: &str,
    ) -> MithrilResult<GitHubRelease>;

    /// Retrieves the latest release.
    async fn get_latest_release(&self, owner: &str, repo: &str) -> MithrilResult<GitHubRelease>;

    /// Retrieves the prerelease.
    async fn get_prerelease(&self, owner: &str, repo: &str) -> MithrilResult<GitHubRelease>;

    /// Retrieves all available releases.
    async fn get_all_releases(&self, owner: &str, repo: &str) -> MithrilResult<Vec<GitHubRelease>>;
}
