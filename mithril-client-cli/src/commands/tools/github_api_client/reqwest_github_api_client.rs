use anyhow::{anyhow, Context};
use async_trait::async_trait;
use reqwest::{Client, Url};

use mithril_client::MithrilResult;

use crate::commands::tools::github_release::GitHubRelease;

use super::GitHubApiClient;

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
}

#[async_trait]
impl GitHubApiClient for ReqwestGitHubApiClient {
    async fn get_release_by_tag(
        &self,
        organization: &str,
        repository: &str,
        tag: &str,
    ) -> MithrilResult<GitHubRelease> {
        let url =
            format!("https://api.github.com/repos/{organization}/{repository}/releases/tags/{tag}");
        let url = Url::parse(&url)
            .with_context(|| format!("Failed to parse URL for GitHub API: {}", url))?;

        let response = self
            .client
            .get(url.clone())
            .send()
            .await
            .with_context(|| format!("Failed to send request to GitHub API: {}", url))?;

        let response = response.text().await?;
        let release: GitHubRelease = serde_json::from_str(&response)
            .with_context(|| format!("Failed to parse response from GitHub API: {:?}", response))?;

        Ok(release)
    }

    async fn get_latest_release(
        &self,
        organization: &str,
        repository: &str,
    ) -> MithrilResult<GitHubRelease> {
        let url =
            format!("https://api.github.com/repos/{organization}/{repository}/releases/latest");
        let url = Url::parse(&url)
            .with_context(|| format!("Failed to parse URL for GitHub API: {}", url))?;

        let response = self
            .client
            .get(url.clone())
            .send()
            .await
            .with_context(|| format!("Failed to send request to GitHub API: {}", url))?;

        let response = response.text().await?;
        let release: GitHubRelease = serde_json::from_str(&response)
            .with_context(|| format!("Failed to parse response from GitHub API: {:?}", response))?;

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
            .find(|release| release.is_prerelease())
            .ok_or_else(|| anyhow!("No prerelease found"))?;

        Ok(prerelease)
    }

    async fn get_all_releases(
        &self,
        organization: &str,
        repository: &str,
    ) -> MithrilResult<Vec<GitHubRelease>> {
        let url = format!("https://api.github.com/repos/{organization}/{repository}/releases");
        let url = Url::parse(&url)
            .with_context(|| format!("Failed to parse URL for GitHub API: {}", url))?;

        let response = self
            .client
            .get(url.clone())
            .send()
            .await
            .with_context(|| format!("Failed to send request to GitHub API: {}", url))?;

        let response = response.text().await?;
        let releases: Vec<GitHubRelease> = serde_json::from_str(&response)
            .with_context(|| format!("Failed to parse response from GitHub API: {:?}", response))?;

        Ok(releases)
    }
}
