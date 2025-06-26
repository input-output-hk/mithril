use std::{
    env, fmt,
    fs::{create_dir, read_dir, remove_dir_all, rename},
    path::{Path, PathBuf},
    process::Command,
};

use anyhow::{anyhow, Context};
use clap::{Parser, ValueEnum};

use mithril_client::MithrilResult;

use crate::utils::{
    copy_dir, remove_dir_contents, ArchiveUnpacker, GitHubReleaseRetriever, HttpDownloader,
    ReqwestGitHubApiClient, ReqwestHttpDownloader,
};

const GITHUB_ORGANIZATION: &str = "IntersectMBO";
const GITHUB_REPOSITORY: &str = "cardano-node";

const LATEST_DISTRIBUTION_TAG: &str = "latest";
const PRERELEASE_DISTRIBUTION_TAG: &str = "pre-release";

const WORK_DIR: &str = "tmp";
const CARDANO_DISTRIBUTION_DIR: &str = "cardano-node-distribution";
const SNAPSHOTS_DIR: &str = "snapshots";

const SNAPSHOT_CONVERTER_BIN_DIR: &str = "bin";
const SNAPSHOT_CONVERTER_BIN_NAME_UNIX: &str = "snapshot-converter";
const SNAPSHOT_CONVERTER_BIN_NAME_WINDOWS: &str = "snapshot-converter.exe";
const SNAPSHOT_CONVERTER_CONFIG_DIR: &str = "share";
const SNAPSHOT_CONVERTER_CONFIG_FILE: &str = "config.json";

const LEDGER_DIR: &str = "ledger";
const PROTOCOL_MAGIC_ID_FILE: &str = "protocolMagicId";

const MAINNET_MAGIC_ID: u32 = 764824073;
const PREPROD_MAGIC_ID: u32 = 1;
const PREVIEW_MAGIC_ID: u32 = 2;

const CONVERSION_FALLBACK_LIMIT: usize = 2;

#[derive(Debug, Clone, ValueEnum, Eq, PartialEq)]
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

#[derive(Debug, Clone, ValueEnum, Eq, PartialEq)]
enum CardanoNetwork {
    Preview,
    Preprod,
    Mainnet,
}

impl fmt::Display for CardanoNetwork {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Preview => write!(f, "preview"),
            Self::Preprod => write!(f, "preprod"),
            Self::Mainnet => write!(f, "mainnet"),
        }
    }
}

#[cfg_attr(test, mockall::automock)]
trait SnapshotConverter {
    fn convert(&self, input_path: &Path, output_path: &Path) -> MithrilResult<()>;
}

struct SnapshotConverterBin {
    pub converter_bin: PathBuf,
    pub config_path: PathBuf,
    pub utxo_hd_flavor: UTxOHDFlavor,
}

impl SnapshotConverter for SnapshotConverterBin {
    fn convert(&self, input_path: &Path, output_path: &Path) -> MithrilResult<()> {
        let status = Command::new(self.converter_bin.clone())
            .arg("Mem")
            .arg(input_path)
            .arg(self.utxo_hd_flavor.to_string())
            .arg(output_path)
            .arg("cardano")
            .arg("--config")
            .arg(self.config_path.clone())
            .status()
            .with_context(|| {
                format!(
                    "Failed to execute snapshot-converter binary at {}",
                    self.converter_bin.display()
                )
            })?;

        if !status.success() {
            return Err(anyhow!(
                "Failure while running snapshot-converter binary, exited with status code: {:?}",
                status.code().map_or(String::from("unknown"), |c| c.to_string())
            ));
        }

        Ok(())
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
    /// `latest` and `pre-release` are also supported to download the latest or pre-release distribution.
    #[clap(long)]
    cardano_node_version: String,

    /// Cardano network.
    #[clap(long)]
    #[deprecated(
        since = "0.12.12",
        note = "optional: automatically detected from the protocolMagicId file"
    )]
    cardano_network: Option<CardanoNetwork>,

    /// UTxO-HD flavor to convert the ledger snapshot to.
    #[clap(long)]
    utxo_hd_flavor: UTxOHDFlavor,

    /// If set, the converted snapshot replaces the current ledger state in the `db_directory`.
    #[clap(long)]
    commit: bool,

    /// GitHub token for authenticated API calls.
    #[clap(long, env = "GITHUB_TOKEN")]
    github_token: Option<String>,
}

impl SnapshotConverterCommand {
    /// Main command execution
    pub async fn execute(&self) -> MithrilResult<()> {
        let work_dir = self.db_directory.join(WORK_DIR);
        create_dir(&work_dir).with_context(|| {
            format!(
                "Failed to create snapshot converter work directory: {}",
                work_dir.display()
            )
        })?;
        let distribution_dir = work_dir.join(CARDANO_DISTRIBUTION_DIR);

        let result = {
            create_dir(&distribution_dir).with_context(|| {
                format!(
                    "Failed to create distribution directory: {}",
                    distribution_dir.display()
                )
            })?;
            let archive_path = Self::download_cardano_node_distribution(
                ReqwestGitHubApiClient::new(self.github_token.clone())?,
                ReqwestHttpDownloader::new()?,
                &self.cardano_node_version,
                &distribution_dir,
            )
            .await
            .with_context(|| "Failed to download Cardano node distribution")?;

            println!(
                "Unpacking distribution from archive: {}",
                archive_path.display()
            );
            ArchiveUnpacker::default()
                .unpack(&archive_path, &distribution_dir)
                .with_context(|| {
                    format!(
                        "Failed to unpack distribution to directory: {}",
                        distribution_dir.display()
                    )
                })?;
            println!(
                "Distribution unpacked successfully to: {}",
                distribution_dir.display()
            );

            #[allow(deprecated)]
            let cardano_network = if let Some(network) = &self.cardano_network {
                network.clone()
            } else {
                Self::detect_cardano_network(&self.db_directory).with_context(|| {
                    format!(
                        "Could not detect Cardano network from the database directory: {}",
                        self.db_directory.display()
                    )
                })?
            };
            Self::convert_ledger_state_snapshot(
                &work_dir,
                &self.db_directory,
                &distribution_dir,
                &cardano_network,
                &self.utxo_hd_flavor,
                self.commit,
            )
            .with_context(|| {
                format!(
                    "Failed to convert ledger snapshot to flavor: {}",
                    self.utxo_hd_flavor
                )
            })?;

            Ok(())
        };

        if let Err(e) = Self::cleanup(&work_dir, &distribution_dir, self.commit, result.is_ok()) {
            eprintln!(
                "Failed to clean up temporary directory {} after execution: {}",
                distribution_dir.display(),
                e
            );
        }

        result
    }

    async fn download_cardano_node_distribution(
        github_api_client: impl GitHubReleaseRetriever,
        http_downloader: impl HttpDownloader,
        tag: &str,
        target_dir: &Path,
    ) -> MithrilResult<PathBuf> {
        println!("Downloading Cardano node distribution for tag: '{tag}'...");
        let release = match tag {
            LATEST_DISTRIBUTION_TAG => github_api_client
                .get_latest_release(GITHUB_ORGANIZATION, GITHUB_REPOSITORY)
                .await
                .with_context(|| "Failed to get latest release")?,
            PRERELEASE_DISTRIBUTION_TAG => github_api_client
                .get_prerelease(GITHUB_ORGANIZATION, GITHUB_REPOSITORY)
                .await
                .with_context(|| "Failed to get pre-release")?,
            _ => github_api_client
                .get_release_by_tag(GITHUB_ORGANIZATION, GITHUB_REPOSITORY, tag)
                .await
                .with_context(|| format!("Failed to get release by tag: {tag}"))?,
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

    fn convert_ledger_state_snapshot(
        work_dir: &Path,
        db_dir: &Path,
        distribution_dir: &Path,
        cardano_network: &CardanoNetwork,
        utxo_hd_flavor: &UTxOHDFlavor,
        commit: bool,
    ) -> MithrilResult<()> {
        println!("Converting ledger state snapshot to '{utxo_hd_flavor}' flavor");
        let converter_bin =
            Self::get_snapshot_converter_binary_path(distribution_dir, env::consts::OS)?;
        let config_path =
            Self::get_snapshot_converter_config_path(distribution_dir, cardano_network);
        let snapshots = Self::find_most_recent_snapshots(db_dir, CONVERSION_FALLBACK_LIMIT)?;
        let converter_bin = SnapshotConverterBin {
            converter_bin,
            config_path,
            utxo_hd_flavor: utxo_hd_flavor.clone(),
        };
        let converted_snapshot_path = Self::try_convert(
            work_dir,
            utxo_hd_flavor,
            &snapshots,
            Box::new(converter_bin),
        )?;

        if commit {
            Self::commit_converted_snapshot(db_dir, &converted_snapshot_path).with_context(
                || "Failed to overwrite the ledger state with the converted snapshot.",
            )?;
        } else {
            println!("Snapshot location: {}", converted_snapshot_path.display());
        }

        Ok(())
    }

    fn try_convert(
        work_dir: &Path,
        utxo_hd_flavor: &UTxOHDFlavor,
        snapshots: &[PathBuf],
        converter: Box<dyn SnapshotConverter>,
    ) -> MithrilResult<PathBuf> {
        let snapshots_dir = work_dir.join(SNAPSHOTS_DIR);

        for (i, snapshot) in snapshots.iter().enumerate() {
            let attempt = i + 1;
            println!("Converting '{}' (attempt #{})", snapshot.display(), attempt);

            let input_path = copy_dir(snapshot, &snapshots_dir)?;
            let output_path = Self::compute_converted_snapshot_output_path(
                &snapshots_dir,
                &input_path,
                utxo_hd_flavor,
            )?;

            match converter.convert(&input_path, &output_path) {
                Ok(()) => {
                    return {
                        println!(
                            "Successfully converted ledger state snapshot: '{}'",
                            snapshot.display()
                        );

                        Ok(output_path)
                    }
                }
                Err(e) => {
                    eprintln!(
                        "Failed to convert ledger state snapshot '{}': {e}",
                        snapshot.display()
                    );
                    continue;
                }
            };
        }

        Err(anyhow!(
            "Failed to convert any of the provided ledger state snapshots to the desired flavor: {}",
            utxo_hd_flavor
        ))
    }

    fn get_snapshot_converter_binary_path(
        distribution_dir: &Path,
        target_os: &str,
    ) -> MithrilResult<PathBuf> {
        let base_path = distribution_dir.join(SNAPSHOT_CONVERTER_BIN_DIR);
        let binary_name = match target_os {
            "linux" | "macos" => SNAPSHOT_CONVERTER_BIN_NAME_UNIX,
            "windows" => SNAPSHOT_CONVERTER_BIN_NAME_WINDOWS,
            _ => return Err(anyhow!("Unsupported platform: {}", target_os)),
        };

        Ok(base_path.join(binary_name))
    }

    fn get_snapshot_converter_config_path(
        distribution_dir: &Path,
        network: &CardanoNetwork,
    ) -> PathBuf {
        distribution_dir
            .join(SNAPSHOT_CONVERTER_CONFIG_DIR)
            .join(network.to_string())
            .join(SNAPSHOT_CONVERTER_CONFIG_FILE)
    }

    /// Returns the list of valid ledger snapshot directories sorted in ascending order of slot number.
    ///
    /// Only directories with numeric names are considered valid snapshots.
    fn get_sorted_snapshot_dirs(ledger_dir: &Path) -> MithrilResult<Vec<(u64, PathBuf)>> {
        let entries = read_dir(ledger_dir).with_context(|| {
            format!(
                "Failed to read ledger state snapshots directory: {}",
                ledger_dir.display()
            )
        })?;

        let mut snapshots = entries
            .filter_map(|entry| {
                let path = entry.ok()?.path();
                if !path.is_dir() {
                    return None;
                }
                SnapshotConverterCommand::extract_slot_number(&path)
                    .ok()
                    .map(|slot| (slot, path))
            })
            .collect::<Vec<_>>();

        snapshots.sort_by_key(|(slot, _)| *slot);

        Ok(snapshots)
    }

    fn find_most_recent_snapshots(db_dir: &Path, count: usize) -> MithrilResult<Vec<PathBuf>> {
        let ledger_dir = db_dir.join(LEDGER_DIR);
        let snapshots = Self::get_sorted_snapshot_dirs(&ledger_dir)?
            .into_iter()
            .rev()
            .take(count)
            .map(|(_, path)| path)
            .collect::<Vec<_>>();

        if snapshots.is_empty() {
            return Err(anyhow!(
                "No valid ledger state snapshots found in directory: {}",
                ledger_dir.display()
            ));
        }

        Ok(snapshots)
    }

    fn compute_converted_snapshot_output_path(
        snapshots_dir: &Path,
        input_snapshot: &Path,
        flavor: &UTxOHDFlavor,
    ) -> MithrilResult<PathBuf> {
        let slot_number = Self::extract_slot_number(input_snapshot).with_context(|| {
            format!(
                "Failed to extract slot number from: {}",
                input_snapshot.display()
            )
        })?;
        let converted_snapshot_path = snapshots_dir.join(format!(
            "{}_{}",
            slot_number,
            flavor.to_string().to_lowercase()
        ));

        Ok(converted_snapshot_path)
    }

    fn extract_slot_number(path: &Path) -> MithrilResult<u64> {
        let file_name = path
            .file_name()
            .ok_or_else(|| anyhow!("No filename in path: {}", path.display()))?;
        let file_name_str = file_name
            .to_str()
            .ok_or_else(|| anyhow!("Invalid UTF-8 in path filename: {:?}", file_name))?;

        file_name_str
            .parse::<u64>()
            .with_context(|| format!("Invalid slot number in path filename: {file_name_str}"))
    }

    /// Commits the converted snapshot by replacing the current ledger state snapshots in the database directory.
    fn commit_converted_snapshot(
        db_dir: &Path,
        converted_snapshot_path: &Path,
    ) -> MithrilResult<()> {
        let ledger_dir = db_dir.join(LEDGER_DIR);
        println!(
            "Upgrading and replacing ledger state in {} with converted snapshot: {}",
            ledger_dir.display(),
            converted_snapshot_path.display()
        );
        let filename = converted_snapshot_path
            .file_name()
            .ok_or_else(|| anyhow!("Missing filename in converted snapshot path"))?
            .to_string_lossy();
        let (slot_number, _) = filename
            .split_once('_')
            .ok_or_else(|| anyhow!("Invalid converted snapshot name format: {}", filename))?;
        remove_dir_contents(&ledger_dir).with_context(|| {
            format!(
                "Failed to remove contents of ledger directory: {}",
                ledger_dir.display()
            )
        })?;
        let destination = ledger_dir.join(slot_number);
        rename(converted_snapshot_path, &destination).with_context(|| {
            format!(
                "Failed to move converted snapshot to ledger directory: {}",
                destination.display()
            )
        })?;

        Ok(())
    }

    fn cleanup(
        work_dir: &Path,
        distribution_dir: &Path,
        commit: bool,
        success: bool,
    ) -> MithrilResult<()> {
        match (success, commit) {
            (true, true) => {
                remove_dir_all(distribution_dir)?;
                remove_dir_all(work_dir)?;
            }
            (true, false) => {
                remove_dir_all(distribution_dir)?;
            }
            (false, _) => {
                remove_dir_all(distribution_dir)?;
                remove_dir_all(work_dir)?;
            }
        }

        Ok(())
    }

    fn detect_cardano_network(db_dir: &Path) -> MithrilResult<CardanoNetwork> {
        let magic_id_path = db_dir.join(PROTOCOL_MAGIC_ID_FILE);
        let content = std::fs::read_to_string(&magic_id_path).with_context(|| {
            format!(
                "Failed to read protocolMagicId file: {}",
                magic_id_path.display()
            )
        })?;
        let id: u32 = content
            .trim()
            .parse()
            .with_context(|| format!("Invalid protocolMagicId value: '{}'", content.trim()))?;

        match id {
            MAINNET_MAGIC_ID => Ok(CardanoNetwork::Mainnet),
            PREPROD_MAGIC_ID => Ok(CardanoNetwork::Preprod),
            PREVIEW_MAGIC_ID => Ok(CardanoNetwork::Preview),
            _ => Err(anyhow!("Unknown protocolMagicId value: '{}'", id)),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::fs::File;

    use mithril_common::temp_dir_create;

    use super::*;

    mod download_cardano_node_distribution {
        use mockall::predicate::eq;
        use reqwest::Url;

        use crate::utils::{GitHubRelease, MockGitHubReleaseRetriever, MockHttpDownloader};

        use super::*;

        #[tokio::test]
        async fn downloads_latest_release_distribution() {
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
        async fn downloads_prerelease_distribution() {
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
        async fn downloads_tagged_release_distribution() {
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

    mod get_snapshot_converter_binary_path {
        use super::*;

        #[test]
        fn returns_correct_binary_path_for_linux() {
            let distribution_dir = PathBuf::from("/path/to/distribution");

            let binary_path = SnapshotConverterCommand::get_snapshot_converter_binary_path(
                &distribution_dir,
                "linux",
            )
            .unwrap();

            assert_eq!(
                binary_path,
                distribution_dir
                    .join(SNAPSHOT_CONVERTER_BIN_DIR)
                    .join(SNAPSHOT_CONVERTER_BIN_NAME_UNIX)
            );
        }

        #[test]
        fn returns_correct_binary_path_for_macos() {
            let distribution_dir = PathBuf::from("/path/to/distribution");

            let binary_path = SnapshotConverterCommand::get_snapshot_converter_binary_path(
                &distribution_dir,
                "macos",
            )
            .unwrap();

            assert_eq!(
                binary_path,
                distribution_dir
                    .join(SNAPSHOT_CONVERTER_BIN_DIR)
                    .join(SNAPSHOT_CONVERTER_BIN_NAME_UNIX)
            );
        }

        #[test]
        fn returns_correct_binary_path_for_windows() {
            let distribution_dir = PathBuf::from("/path/to/distribution");

            let binary_path = SnapshotConverterCommand::get_snapshot_converter_binary_path(
                &distribution_dir,
                "windows",
            )
            .unwrap();

            assert_eq!(
                binary_path,
                distribution_dir
                    .join(SNAPSHOT_CONVERTER_BIN_DIR)
                    .join(SNAPSHOT_CONVERTER_BIN_NAME_WINDOWS)
            );
        }
    }

    mod get_snapshot_converter_config_path {
        use super::*;

        #[test]
        fn returns_config_path_for_mainnet() {
            let distribution_dir = PathBuf::from("/path/to/distribution");
            let network = CardanoNetwork::Mainnet;

            let config_path = SnapshotConverterCommand::get_snapshot_converter_config_path(
                &distribution_dir,
                &network,
            );

            assert_eq!(
                config_path,
                distribution_dir
                    .join(SNAPSHOT_CONVERTER_CONFIG_DIR)
                    .join(network.to_string())
                    .join(SNAPSHOT_CONVERTER_CONFIG_FILE)
            );
        }

        #[test]
        fn returns_config_path_for_preprod() {
            let distribution_dir = PathBuf::from("/path/to/distribution");
            let network = CardanoNetwork::Preprod;

            let config_path = SnapshotConverterCommand::get_snapshot_converter_config_path(
                &distribution_dir,
                &network,
            );

            assert_eq!(
                config_path,
                distribution_dir
                    .join(SNAPSHOT_CONVERTER_CONFIG_DIR)
                    .join(network.to_string())
                    .join(SNAPSHOT_CONVERTER_CONFIG_FILE)
            );
        }

        #[test]
        fn returns_config_path_for_preview() {
            let distribution_dir = PathBuf::from("/path/to/distribution");
            let network = CardanoNetwork::Preview;

            let config_path = SnapshotConverterCommand::get_snapshot_converter_config_path(
                &distribution_dir,
                &network,
            );

            assert_eq!(
                config_path,
                distribution_dir
                    .join(SNAPSHOT_CONVERTER_CONFIG_DIR)
                    .join(network.to_string())
                    .join(SNAPSHOT_CONVERTER_CONFIG_FILE)
            );
        }
    }

    mod extract_slot_number {
        use super::*;

        #[test]
        fn parses_valid_numeric_path() {
            let path = PathBuf::from("/whatever").join("123456");

            let slot = SnapshotConverterCommand::extract_slot_number(&path).unwrap();

            assert_eq!(slot, 123456);
        }

        #[test]
        fn fails_with_non_numeric_filename() {
            let path = PathBuf::from("/whatever").join("notanumber");

            SnapshotConverterCommand::extract_slot_number(&path)
                .expect_err("Should fail with non-numeric filename");
        }

        #[test]
        fn fails_if_no_filename() {
            let path = PathBuf::from("/");

            SnapshotConverterCommand::extract_slot_number(&path)
                .expect_err("Should fail if path has no filename");
        }
    }

    mod compute_converted_snapshot_output_path {
        use super::*;

        #[test]
        fn compute_output_path_from_numeric_file_name() {
            let snapshots_dir = PathBuf::from("/snapshots");
            let input_snapshot = PathBuf::from("/whatever").join("123456");

            {
                let snapshot_path =
                    SnapshotConverterCommand::compute_converted_snapshot_output_path(
                        &snapshots_dir,
                        &input_snapshot,
                        &UTxOHDFlavor::Lmdb,
                    )
                    .unwrap();

                assert_eq!(snapshot_path, snapshots_dir.join("123456_lmdb"));
            }

            {
                let snapshot_path =
                    SnapshotConverterCommand::compute_converted_snapshot_output_path(
                        &snapshots_dir,
                        &input_snapshot,
                        &UTxOHDFlavor::Legacy,
                    )
                    .unwrap();

                assert_eq!(snapshot_path, snapshots_dir.join("123456_legacy"));
            }
        }

        #[test]
        fn fails_with_invalid_slot_number() {
            let snapshots_dir = PathBuf::from("/snapshots");
            let input_snapshot = PathBuf::from("/whatever/notanumber");

            SnapshotConverterCommand::compute_converted_snapshot_output_path(
                &snapshots_dir,
                &input_snapshot,
                &UTxOHDFlavor::Lmdb,
            )
            .expect_err("Should fail with invalid slot number");
        }
    }

    mod commit_converted_snapshot {
        use super::*;

        #[test]
        fn moves_converted_snapshot_to_ledger_directory() {
            let tmp_dir = temp_dir_create!();
            let ledger_dir = tmp_dir.join(LEDGER_DIR);
            create_dir(&ledger_dir).unwrap();
            let previous_snapshot = ledger_dir.join("123");
            File::create(&previous_snapshot).unwrap();

            let converted_snapshot = tmp_dir.join("456_lmdb");
            File::create(&converted_snapshot).unwrap();

            assert!(previous_snapshot.exists());
            SnapshotConverterCommand::commit_converted_snapshot(&tmp_dir, &converted_snapshot)
                .unwrap();

            assert!(!previous_snapshot.exists());
            assert!(ledger_dir.join("456").exists());
        }

        #[test]
        fn fails_if_converted_snapshot_has_invalid_filename() {
            let tmp_dir = temp_dir_create!();
            let ledger_dir = tmp_dir.join(LEDGER_DIR);
            create_dir(&ledger_dir).unwrap();
            let previous_snapshot = ledger_dir.join("123");
            File::create(&previous_snapshot).unwrap();

            let converted_snapshot = tmp_dir.join("456");
            File::create(&converted_snapshot).unwrap();

            SnapshotConverterCommand::commit_converted_snapshot(&tmp_dir, &converted_snapshot)
                .expect_err("Should fail if converted snapshot has invalid filename");

            assert!(previous_snapshot.exists());
        }
    }

    mod cleanup {
        use super::*;

        #[test]
        fn removes_both_dirs_on_success_when_commit_is_true() {
            let tmp = temp_dir_create!();
            let work_dir = tmp.join("workdir_dir");
            let distribution_dir = tmp.join("distribution_dir");
            create_dir(&work_dir).unwrap();
            create_dir(&distribution_dir).unwrap();

            SnapshotConverterCommand::cleanup(&work_dir, &distribution_dir, true, true).unwrap();

            assert!(!distribution_dir.exists());
            assert!(!work_dir.exists());
        }

        #[test]
        fn removes_only_distribution_on_success_when_commit_is_false() {
            let tmp = temp_dir_create!();
            let work_dir = tmp.join("workdir_dir");
            let distribution_dir = tmp.join("distribution_dir");
            create_dir(&work_dir).unwrap();
            create_dir(&distribution_dir).unwrap();

            SnapshotConverterCommand::cleanup(&work_dir, &distribution_dir, false, true).unwrap();

            assert!(!distribution_dir.exists());
            assert!(work_dir.exists());
        }

        #[test]
        fn removes_both_dirs_on_success_when_commit_is_true_and_distribution_is_nested() {
            let tmp = temp_dir_create!();
            let work_dir = tmp.join("workdir_dir");
            let distribution_dir = work_dir.join("distribution_dir");
            create_dir(&work_dir).unwrap();
            create_dir(&distribution_dir).unwrap();

            SnapshotConverterCommand::cleanup(&work_dir, &distribution_dir, true, true).unwrap();

            assert!(!distribution_dir.exists());
            assert!(!work_dir.exists());
        }

        #[test]
        fn removes_only_distribution_on_success_when_commit_is_false_and_distribution_is_nested() {
            let tmp = temp_dir_create!();
            let work_dir = tmp.join("workdir_dir");
            let distribution_dir = work_dir.join("distribution_dir");
            create_dir(&work_dir).unwrap();
            create_dir(&distribution_dir).unwrap();

            SnapshotConverterCommand::cleanup(&work_dir, &distribution_dir, false, true).unwrap();

            assert!(!distribution_dir.exists());
            assert!(work_dir.exists());
        }

        #[test]
        fn removes_both_dirs_on_failure() {
            let tmp = temp_dir_create!();
            let work_dir = tmp.join("workdir_dir");
            let distribution_dir = tmp.join("distribution_dir");
            create_dir(&work_dir).unwrap();
            create_dir(&distribution_dir).unwrap();

            SnapshotConverterCommand::cleanup(&work_dir, &distribution_dir, false, false).unwrap();

            assert!(!distribution_dir.exists());
            assert!(!work_dir.exists());
        }
    }

    mod detect_cardano_network {
        use super::*;

        fn create_protocol_magic_id_file(db_dir: &Path, magic_id: u32) -> PathBuf {
            let file_path = db_dir.join(PROTOCOL_MAGIC_ID_FILE);
            std::fs::write(&file_path, magic_id.to_string()).unwrap();

            file_path
        }

        #[test]
        fn detects_mainnet() {
            let db_dir = temp_dir_create!();
            create_protocol_magic_id_file(&db_dir, MAINNET_MAGIC_ID);

            let network = SnapshotConverterCommand::detect_cardano_network(&db_dir).unwrap();

            assert_eq!(network, CardanoNetwork::Mainnet);
        }

        #[test]
        fn detects_preprod() {
            let db_dir = temp_dir_create!();
            create_protocol_magic_id_file(&db_dir, PREPROD_MAGIC_ID);

            let network = SnapshotConverterCommand::detect_cardano_network(&db_dir).unwrap();

            assert_eq!(network, CardanoNetwork::Preprod);
        }

        #[test]
        fn detects_preview() {
            let db_dir = temp_dir_create!();
            create_protocol_magic_id_file(&db_dir, PREVIEW_MAGIC_ID);

            let network = SnapshotConverterCommand::detect_cardano_network(&db_dir).unwrap();

            assert_eq!(network, CardanoNetwork::Preview);
        }

        #[test]
        fn fails_on_invalid_network() {
            let db_dir = temp_dir_create!();
            let invalid_magic_id = 999999;
            create_protocol_magic_id_file(&db_dir, invalid_magic_id);

            SnapshotConverterCommand::detect_cardano_network(&db_dir)
                .expect_err("Should fail with invalid network magic ID");
        }

        #[test]
        fn fails_when_protocol_magic_id_file_is_empty() {
            let db_dir = temp_dir_create!();
            File::create(db_dir.join(PROTOCOL_MAGIC_ID_FILE)).unwrap();

            SnapshotConverterCommand::detect_cardano_network(&db_dir)
                .expect_err("Should fail when protocol magic ID file is empty");
        }

        #[test]
        fn fails_when_protocol_magic_id_file_is_missing() {
            let db_dir = temp_dir_create!();

            assert!(!db_dir.join(PROTOCOL_MAGIC_ID_FILE).exists());

            SnapshotConverterCommand::detect_cardano_network(&db_dir)
                .expect_err("Should fail when protocol magic ID file is missing");
        }
    }

    mod snapshots_indexing {
        use mithril_common::temp_dir_create;

        use super::*;

        #[test]
        fn returns_the_two_most_recent_snapshots() {
            let db_dir = temp_dir_create!();
            let ledger_dir = db_dir.join(LEDGER_DIR);
            create_dir(&ledger_dir).unwrap();

            create_dir(ledger_dir.join("1500")).unwrap();
            create_dir(ledger_dir.join("500")).unwrap();
            create_dir(ledger_dir.join("1000")).unwrap();

            let found = SnapshotConverterCommand::find_most_recent_snapshots(&db_dir, 2).unwrap();

            assert_eq!(
                found,
                vec![ledger_dir.join("1500"), ledger_dir.join("1000")]
            );
        }

        #[test]
        fn returns_list_with_one_entry_if_only_one_valid_snapshot() {
            let db_dir = temp_dir_create!();
            let ledger_dir = db_dir.join(LEDGER_DIR);
            create_dir(&ledger_dir).unwrap();

            create_dir(ledger_dir.join("500")).unwrap();

            let found = SnapshotConverterCommand::find_most_recent_snapshots(&db_dir, 2).unwrap();

            assert_eq!(found, vec![ledger_dir.join("500")]);
        }

        #[test]
        fn ignores_non_numeric_and_non_directory_entries() {
            let temp_dir = temp_dir_create!();
            let ledger_dir = temp_dir.join(LEDGER_DIR);
            create_dir(&ledger_dir).unwrap();

            create_dir(ledger_dir.join("1000")).unwrap();
            File::create(ledger_dir.join("500")).unwrap();
            create_dir(ledger_dir.join("invalid")).unwrap();

            let found = SnapshotConverterCommand::find_most_recent_snapshots(&temp_dir, 2).unwrap();

            assert_eq!(found, vec![ledger_dir.join("1000")]);
        }

        #[test]
        fn returns_all_available_snapshots_when_count_exceeds_available() {
            let db_dir = temp_dir_create!();
            let ledger_dir = db_dir.join(LEDGER_DIR);
            create_dir(&ledger_dir).unwrap();

            create_dir(ledger_dir.join("1000")).unwrap();
            create_dir(ledger_dir.join("1500")).unwrap();

            let found = SnapshotConverterCommand::find_most_recent_snapshots(&db_dir, 99).unwrap();

            assert_eq!(
                found,
                vec![ledger_dir.join("1500"), ledger_dir.join("1000")]
            );
        }

        #[test]
        fn returns_error_if_no_valid_snapshot_found() {
            let temp_dir = temp_dir_create!();
            let ledger_dir = temp_dir.join(LEDGER_DIR);
            create_dir(&ledger_dir).unwrap();

            File::create(ledger_dir.join("invalid")).unwrap();

            SnapshotConverterCommand::find_most_recent_snapshots(&temp_dir, 2)
                .expect_err("Should return error if no valid ledger snapshot directory found");
        }

        #[test]
        fn get_sorted_snapshot_dirs_returns_sorted_valid_directories() {
            let temp_dir = temp_dir_create!();
            let ledger_dir = temp_dir.join(LEDGER_DIR);
            create_dir(&ledger_dir).unwrap();

            create_dir(ledger_dir.join("1500")).unwrap();
            create_dir(ledger_dir.join("1000")).unwrap();
            create_dir(ledger_dir.join("2000")).unwrap();
            File::create(ledger_dir.join("500")).unwrap();
            create_dir(ledger_dir.join("notanumber")).unwrap();

            let snapshots =
                SnapshotConverterCommand::get_sorted_snapshot_dirs(&ledger_dir).unwrap();

            assert_eq!(
                snapshots,
                vec![
                    (1000, ledger_dir.join("1000")),
                    (1500, ledger_dir.join("1500")),
                    (2000, ledger_dir.join("2000")),
                ]
            );
        }
    }

    mod snapshot_conversion_fallback {
        use mockall::predicate::{self, always};

        use super::*;

        fn create_dummy_snapshots(dir: &Path, count: usize) -> Vec<PathBuf> {
            (1..=count)
                .map(|i| {
                    let snapshot_path = dir.join(format!("{i}"));
                    create_dir(&snapshot_path).unwrap();
                    snapshot_path
                })
                .collect()
        }

        fn contains_filename(expected: &Path) -> impl Fn(&Path) -> bool {
            let filename = expected.file_name().unwrap().to_string_lossy().to_string();

            move |p: &Path| p.to_string_lossy().contains(&filename)
        }

        #[test]
        fn conversion_succeed_and_is_called_once_if_first_conversion_succeeds() {
            let temp_dir = temp_dir_create!();
            let snapshots = create_dummy_snapshots(&temp_dir, 2);

            let mut converter = MockSnapshotConverter::new();
            converter
                .expect_convert()
                .with(
                    predicate::function(contains_filename(&snapshots[0])),
                    always(),
                )
                .return_once(|_, _| Ok(()))
                .once();

            SnapshotConverterCommand::try_convert(
                Path::new(""),
                &UTxOHDFlavor::Lmdb,
                &snapshots,
                Box::new(converter),
            )
            .unwrap();
        }

        #[test]
        fn conversion_falls_back_if_first_conversion_fails() {
            let temp_dir = temp_dir_create!();
            let snapshots = create_dummy_snapshots(&temp_dir, 2);

            let mut converter = MockSnapshotConverter::new();
            converter
                .expect_convert()
                .with(
                    predicate::function(contains_filename(&snapshots[0])),
                    always(),
                )
                .return_once(|_, _| Err(anyhow!("Error during conversion")))
                .once();
            converter
                .expect_convert()
                .with(
                    predicate::function(contains_filename(&snapshots[1])),
                    always(),
                )
                .return_once(|_, _| Ok(()))
                .once();

            SnapshotConverterCommand::try_convert(
                Path::new(""),
                &UTxOHDFlavor::Lmdb,
                &snapshots,
                Box::new(converter),
            )
            .expect("Should succeed even if the first conversion fails");
        }

        #[test]
        fn conversion_fails_if_all_attempts_fail() {
            let temp_dir = temp_dir_create!();
            let snapshots = create_dummy_snapshots(&temp_dir, 2);
            let mut converter = MockSnapshotConverter::new();
            converter
                .expect_convert()
                .returning(|_, _| Err(anyhow!("Conversion failed")))
                .times(2);

            SnapshotConverterCommand::try_convert(
                Path::new(""),
                &UTxOHDFlavor::Lmdb,
                &snapshots,
                Box::new(converter),
            )
            .expect_err("Should fail if all conversion attempts fail");
        }
    }
}
