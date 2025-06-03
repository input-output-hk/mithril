use std::{
    env, fmt,
    path::{Path, PathBuf},
};

use anyhow::{anyhow, Context};
use clap::{Parser, ValueEnum};

use mithril_client::MithrilResult;

use crate::utils::{
    ArchiveUnpacker, GitHubReleaseRetriever, HttpDownloader, ReqwestGitHubApiClient,
    ReqwestHttpDownloader,
};

const GITHUB_ORGANIZATION: &str = "IntersectMBO";
const GITHUB_REPOSITORY: &str = "cardano-node";

const LATEST_DISTRIBUTION_TAG: &str = "latest";
const PRERELEASE_DISTRIBUTION_TAG: &str = "prerelease";

const CARDANO_DISTRIBUTION_TEMP_DIR: &str = "cardano-node-distribution-tmp";

#[derive(Debug, Clone, ValueEnum)]
enum UTxOHDFlavor {
    #[clap(name = "Legacy")]
    Legacy,
    #[clap(name = "LMDB")]
    Lmdb,
}

impl fmt::Display for UTxOHDFlavor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Legacy => write!(f, "Legacy"),
            Self::Lmdb => write!(f, "LMDB"),
        }
    }
}

/// Clap command to convert a restored `InMemory` Mithril snapshot to another flavor.
#[derive(Parser, Debug, Clone)]
pub struct SnapshotConverterCommand {
    /// Path to the Cardano node database directory.
    #[clap(long)]
    db_directory: PathBuf,

    /// Cardano node version of the Mithril signed snapshot.
    ///
    /// `latest` and `prerelease` are also supported to download the latest or preprelease distribution.
    #[clap(long)]
    cardano_node_version: String,

    /// UTxO-HD flavor to convert the ledger snapshot to.
    #[clap(long)]
    utxo_hd_flavor: UTxOHDFlavor,
}

impl SnapshotConverterCommand {
    /// Main command execution
    pub async fn execute(&self) -> MithrilResult<()> {
        let distribution_temp_dir = self.db_directory.join(CARDANO_DISTRIBUTION_TEMP_DIR);
        std::fs::create_dir(&distribution_temp_dir).with_context(|| {
            format!(
                "Failed to create directory: {}",
                distribution_temp_dir.display()
            )
        })?;

        let archive_path = Self::download_cardano_node_distribution(
            ReqwestGitHubApiClient::new()?,
            ReqwestHttpDownloader::new()?,
            &self.cardano_node_version,
            &distribution_temp_dir,
        )
        .await
        .with_context(|| {
            "Failed to download 'snapshot-converter' binary from Cardano node distribution"
        })?;

        ArchiveUnpacker::default()
            .unpack(&archive_path, &distribution_temp_dir)
            .with_context(|| {
                format!(
                    "Failed to unpack 'snapshot-converter' binary to directory: {}",
                    distribution_temp_dir.display()
                )
            })?;

        Ok(())
    }

    async fn download_cardano_node_distribution(
        github_api_client: impl GitHubReleaseRetriever,
        http_downloader: impl HttpDownloader,
        tag: &str,
        target_dir: &Path,
    ) -> MithrilResult<PathBuf> {
        println!(
            "Downloading Cardano node distribution for tag: '{}'...",
            tag
        );
        let release = match tag {
            LATEST_DISTRIBUTION_TAG => github_api_client
                .get_latest_release(GITHUB_ORGANIZATION, GITHUB_REPOSITORY)
                .await
                .with_context(|| "Failed to get latest release")?,
            PRERELEASE_DISTRIBUTION_TAG => github_api_client
                .get_prerelease(GITHUB_ORGANIZATION, GITHUB_REPOSITORY)
                .await
                .with_context(|| "Failed to get prerelease")?,
            _ => github_api_client
                .get_release_by_tag(GITHUB_ORGANIZATION, GITHUB_REPOSITORY, tag)
                .await
                .with_context(|| format!("Failed to get release by tag: {}", tag))?,
        };

        let asset = release
            .get_asset_for_os(env::consts::OS)?
            .ok_or_else(|| anyhow!("No asset found for platform: {}", env::consts::OS))
            .with_context(|| {
                format!(
                    "Failed to find asset for current platform: {}",
                    env::consts::OS
                )
            })?;

        let archive_path = http_downloader
            .download_file(asset.browser_download_url.parse()?, target_dir, &asset.name)
            .await?;

        println!(
            "Distribution downloaded successfully. Archive location: {}",
            archive_path.display()
        );

        Ok(archive_path)
    }
}

#[cfg(test)]
mod tests {
    use mockall::predicate::eq;
    use reqwest::Url;

    use mithril_common::temp_dir_create;

    use crate::utils::{GitHubRelease, MockGitHubReleaseRetriever, MockHttpDownloader};

    use super::*;

    #[tokio::test]
    async fn call_get_latest_release_with_latest_tag() {
        let temp_dir = temp_dir_create!();
        let release = GitHubRelease::dummy_with_all_supported_assets();
        let asset = release.get_asset_for_os(env::consts::OS).unwrap().unwrap();

        let cloned_release = release.clone();
        let mut github_api_client = MockGitHubReleaseRetriever::new();
        github_api_client
            .expect_get_latest_release()
            .with(eq(GITHUB_ORGANIZATION), eq(GITHUB_REPOSITORY))
            .returning(move |_, _| Ok(cloned_release.clone()));

        let mut http_downloader = MockHttpDownloader::new();
        http_downloader
            .expect_download_file()
            .with(
                eq(Url::parse(&asset.browser_download_url).unwrap()),
                eq(temp_dir.clone()),
                eq(asset.name.clone()),
            )
            .returning(|_, _, _| Ok(PathBuf::new()));

        SnapshotConverterCommand::download_cardano_node_distribution(
            github_api_client,
            http_downloader,
            LATEST_DISTRIBUTION_TAG,
            &temp_dir,
        )
        .await
        .unwrap();
    }

    #[tokio::test]
    async fn call_get_prerelease_with_prerelease_tag() {
        let temp_dir = temp_dir_create!();
        let release = GitHubRelease::dummy_with_all_supported_assets();
        let asset = release.get_asset_for_os(env::consts::OS).unwrap().unwrap();

        let cloned_release = release.clone();
        let mut github_api_client = MockGitHubReleaseRetriever::new();
        github_api_client
            .expect_get_prerelease()
            .with(eq(GITHUB_ORGANIZATION), eq(GITHUB_REPOSITORY))
            .returning(move |_, _| Ok(cloned_release.clone()));

        let mut http_downloader = MockHttpDownloader::new();
        http_downloader
            .expect_download_file()
            .with(
                eq(Url::parse(&asset.browser_download_url).unwrap()),
                eq(temp_dir.clone()),
                eq(asset.name.clone()),
            )
            .returning(|_, _, _| Ok(PathBuf::new()));

        SnapshotConverterCommand::download_cardano_node_distribution(
            github_api_client,
            http_downloader,
            PRERELEASE_DISTRIBUTION_TAG,
            &temp_dir,
        )
        .await
        .unwrap();
    }

    #[tokio::test]
    async fn call_get_release_by_tag_with_specific_cardano_node_version() {
        let cardano_node_version = "10.3.1";
        let temp_dir = temp_dir_create!();
        let release = GitHubRelease::dummy_with_all_supported_assets();
        let asset = release.get_asset_for_os(env::consts::OS).unwrap().unwrap();

        let cloned_release = release.clone();
        let mut github_api_client = MockGitHubReleaseRetriever::new();
        github_api_client
            .expect_get_release_by_tag()
            .with(
                eq(GITHUB_ORGANIZATION),
                eq(GITHUB_REPOSITORY),
                eq(cardano_node_version),
            )
            .returning(move |_, _, _| Ok(cloned_release.clone()));

        let mut http_downloader = MockHttpDownloader::new();
        http_downloader
            .expect_download_file()
            .with(
                eq(Url::parse(&asset.browser_download_url).unwrap()),
                eq(temp_dir.clone()),
                eq(asset.name.clone()),
            )
            .returning(|_, _, _| Ok(PathBuf::new()));

        SnapshotConverterCommand::download_cardano_node_distribution(
            github_api_client,
            http_downloader,
            cardano_node_version,
            &temp_dir,
        )
        .await
        .unwrap();
    }
}
