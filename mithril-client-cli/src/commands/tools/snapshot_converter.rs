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
const PRERELEASE_DISTRIBUTION_TAG: &str = "prerelease";

const WORK_DIR: &str = "tmp";
const CARDANO_DISTRIBUTION_DIR: &str = "cardano-node-distribution";
const SNAPSHOTS_DIR: &str = "snapshots";

const SNAPSHOT_CONVERTER_BIN_DIR: &str = "bin";
const SNAPSHOT_CONVERTER_BIN_NAME_UNIX: &str = "snapshot-converter";
const SNAPSHOT_CONVERTER_BIN_NAME_WINDOWS: &str = "snapshot-converter.exe";
const SNAPSHOT_CONVERTER_CONFIG_DIR: &str = "share";
const SNAPSHOT_CONVERTER_CONFIG_FILE: &str = "config.json";

const LEDGER_DIR: &str = "ledger";

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

#[derive(Debug, Clone, ValueEnum)]
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

    /// Cardano network.
    #[clap(long)]
    cardano_network: CardanoNetwork,

    /// UTxO-HD flavor to convert the ledger snapshot to.
    #[clap(long)]
    utxo_hd_flavor: UTxOHDFlavor,

    /// If set, the converted snapshot replaces the current ledger state in the `db_directory`.
    #[clap(long)]
    commit: bool,
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
                ReqwestGitHubApiClient::new()?,
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

            Self::convert_ledger_state_snapshot(
                &work_dir,
                &self.db_directory,
                &distribution_dir,
                &self.cardano_network,
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

    fn convert_ledger_state_snapshot(
        work_dir: &Path,
        db_dir: &Path,
        distribution_dir: &Path,
        cardano_network: &CardanoNetwork,
        utxo_hd_flavor: &UTxOHDFlavor,
        commit: bool,
    ) -> MithrilResult<()> {
        println!(
            "Converting ledger state snapshot to '{}' flavor",
            utxo_hd_flavor
        );
        let snapshots_path = work_dir.join(SNAPSHOTS_DIR);
        let copied_snapshot_path =
            Self::copy_oldest_ledger_state_snapshot(db_dir, &snapshots_path)?;
        let converted_snapshot_path = Self::compute_converted_snapshot_output_path(
            &snapshots_path,
            &copied_snapshot_path,
            utxo_hd_flavor,
        )?;
        let converter_bin =
            Self::get_snapshot_converter_binary_path(distribution_dir, env::consts::OS)?;
        let config_path =
            Self::get_snapshot_converter_config_path(distribution_dir, cardano_network);
        Self::execute_snapshot_converter(
            &converter_bin,
            &copied_snapshot_path,
            &converted_snapshot_path,
            &config_path,
            utxo_hd_flavor,
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

    fn execute_snapshot_converter(
        bin_path: &Path,
        input_path: &Path,
        output_path: &Path,
        config_path: &Path,
        flavor: &UTxOHDFlavor,
    ) -> MithrilResult<()> {
        Command::new(bin_path)
            .arg("Mem")
            .arg(input_path)
            .arg(flavor.to_string())
            .arg(output_path)
            .arg("cardano")
            .arg("--config")
            .arg(config_path)
            .status()
            .with_context(|| {
                format!(
                    "Failed to execute snapshot-converter binary at {}",
                    bin_path.display()
                )
            })?;

        Ok(())
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

    /// Finds the oldest ledger snapshot (by slot number) in the `ledger/` directory of a Cardano node database.
    fn find_oldest_ledger_state_snapshot(db_dir: &Path) -> MithrilResult<PathBuf> {
        let ledger_dir = db_dir.join(LEDGER_DIR);
        let entries = read_dir(&ledger_dir).with_context(|| {
            format!(
                "Failed to read ledger state snapshots directory: {}",
                ledger_dir.display()
            )
        })?;
        let mut min_slot: Option<(u64, PathBuf)> = None;

        for entry in entries {
            let entry = entry?;
            let slot = match Self::extract_slot_number(&entry.path()) {
                Ok(number) => number,
                Err(_) => continue,
            };

            let path = entry.path();
            if path.is_dir()
                && (min_slot
                    .as_ref()
                    .map(|(min, _)| slot < *min)
                    .unwrap_or(true))
            {
                min_slot = Some((slot, path));
            }
        }

        min_slot.map(|(_, path)| path).ok_or_else(|| {
            anyhow!(
                "No valid ledger state snapshot found in directory: {}",
                ledger_dir.display()
            )
        })
    }

    fn copy_oldest_ledger_state_snapshot(
        db_dir: &Path,
        target_dir: &Path,
    ) -> MithrilResult<PathBuf> {
        let snapshot_path = Self::find_oldest_ledger_state_snapshot(db_dir)?;
        let copied_snapshot_path = copy_dir(&snapshot_path, target_dir)?;

        Ok(copied_snapshot_path)
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
            .with_context(|| format!("Invalid slot number in path filename: {}", file_name_str))
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
}

#[cfg(test)]
mod tests {
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

    mod find_oldest_ledger_state_snapshot {
        use std::fs::File;

        use mithril_common::temp_dir_create;

        use super::*;

        #[test]
        fn finds_ledger_state_snapshot_with_lowest_slot_number() {
            let db_dir = temp_dir_create!();
            let ledger_dir = db_dir.join(LEDGER_DIR);
            create_dir(&ledger_dir).unwrap();

            create_dir(ledger_dir.join("500")).unwrap();
            create_dir(ledger_dir.join("1000")).unwrap();
            create_dir(ledger_dir.join("1500")).unwrap();

            let found =
                SnapshotConverterCommand::find_oldest_ledger_state_snapshot(&db_dir).unwrap();

            assert_eq!(found, ledger_dir.join("500"));
        }

        #[test]
        fn returns_snapshot_when_only_one_valid_directory() {
            let db_dir = temp_dir_create!();
            let ledger_dir = db_dir.join(LEDGER_DIR);
            create_dir(&ledger_dir).unwrap();

            create_dir(ledger_dir.join("500")).unwrap();

            let found =
                SnapshotConverterCommand::find_oldest_ledger_state_snapshot(&db_dir).unwrap();

            assert_eq!(found, ledger_dir.join("500"));
        }

        #[test]
        fn ignores_non_numeric_and_non_directory_entries() {
            let temp_dir = temp_dir_create!();
            let ledger_dir = temp_dir.join(LEDGER_DIR);
            create_dir(&ledger_dir).unwrap();

            create_dir(ledger_dir.join("1000")).unwrap();
            File::create(ledger_dir.join("500")).unwrap();
            create_dir(ledger_dir.join("notanumber")).unwrap();

            let found =
                SnapshotConverterCommand::find_oldest_ledger_state_snapshot(&temp_dir).unwrap();

            assert_eq!(found, ledger_dir.join("1000"));
        }

        #[test]
        fn returns_error_if_no_valid_snapshot_found() {
            let temp_dir = temp_dir_create!();
            let ledger_dir = temp_dir.join(LEDGER_DIR);
            create_dir(&ledger_dir).unwrap();

            File::create(ledger_dir.join("invalid")).unwrap();

            SnapshotConverterCommand::find_oldest_ledger_state_snapshot(&temp_dir)
                .expect_err("Should return error if no valid ledger snapshot directory found");
        }
    }

    mod commit_converted_snapshot {
        use std::fs::File;

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
}
