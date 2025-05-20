use anyhow::{anyhow, Context};
use async_trait::async_trait;
use reqwest::{Client, IntoUrl};
use serde::de::DeserializeOwned;

use mithril_client::MithrilResult;

use super::{GitHubRelease, GitHubReleaseRetriever};

pub struct ReqwestGitHubApiClient {
    client: Client,
}

impl ReqwestGitHubApiClient {
    pub fn new() -> MithrilResult<Self> {
        let client = Client::builder()
            .user_agent("mithril-client")
            .build()
            .context("Failed to build Reqwest GitHub API client")?;

        Ok(Self { client })
    }

    async fn download<U: IntoUrl, T: DeserializeOwned>(&self, source_url: U) -> MithrilResult<T> {
        let url = source_url
            .into_url()
            .with_context(|| "Given `source_url` is not a valid Url")?;
        let response = self
            .client
            .get(url.clone())
            .send()
            .await
            .with_context(|| format!("Failed to send request to GitHub API: {}", url))?;
        let body = response.text().await?;
        let parsed_body = serde_json::from_str::<T>(&body)
            .with_context(|| format!("Failed to parse response from GitHub API: {:?}", body))?;

        Ok(parsed_body)
    }
}

#[async_trait]
impl GitHubReleaseRetriever for ReqwestGitHubApiClient {
    async fn get_release_by_tag(
        &self,
        organization: &str,
        repository: &str,
        tag: &str,
    ) -> MithrilResult<GitHubRelease> {
        let url =
            format!("https://api.github.com/repos/{organization}/{repository}/releases/tags/{tag}");
        let release = self.download(url).await?;

        Ok(release)
    }

    async fn get_latest_release(
        &self,
        organization: &str,
        repository: &str,
    ) -> MithrilResult<GitHubRelease> {
        let url =
            format!("https://api.github.com/repos/{organization}/{repository}/releases/latest");
        let release = self.download(url).await?;

        Ok(release)
    }

    async fn get_prerelease(
        &self,
        organization: &str,
        repository: &str,
    ) -> MithrilResult<GitHubRelease> {
        let releases = self.get_all_releases(organization, repository).await?;
        let prerelease = releases
            .into_iter()
            .find(|release| release.prerelease)
            .ok_or_else(|| anyhow!("No prerelease found"))?;

        Ok(prerelease)
    }

    async fn get_all_releases(
        &self,
        organization: &str,
        repository: &str,
    ) -> MithrilResult<Vec<GitHubRelease>> {
        let url = format!("https://api.github.com/repos/{organization}/{repository}/releases");
        let releases = self.download(url).await?;

        Ok(releases)
    }
}
