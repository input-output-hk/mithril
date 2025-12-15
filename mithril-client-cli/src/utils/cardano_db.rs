use std::path::Path;
use std::time::Duration;

use anyhow::anyhow;
use futures::Future;
use indicatif::{MultiProgress, ProgressBar};
use mithril_client::{MithrilError, MithrilResult};

use super::CardanoDbDownloadCheckerError;

/// Utility functions for to the CardanoDb commands
pub struct CardanoDbUtils;

/// Known formats of a ledger state snapshot
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LedgerFormat {
    /// Legacy in-memory format (maximum supported by cardano-node `10.3.0`)
    Legacy,
    /// UTxO-HD in-memory format (since cardano-node `10.4.1`)
    InMemory,
    /// UTxO-HD Lmdb format (since cardano-node `10.4.1`)
    Lmdb,
}

impl CardanoDbUtils {
    /// Handle the error return by `check_prerequisites`
    pub fn check_disk_space_error(error: MithrilError) -> MithrilResult<String> {
        match error.downcast_ref::<CardanoDbDownloadCheckerError>() {
            Some(CardanoDbDownloadCheckerError::NotEnoughSpaceForArchive { .. })
            | Some(CardanoDbDownloadCheckerError::NotEnoughSpaceForUncompressedData { .. }) => {
                Ok(format!("Warning: {error}"))
            }
            _ => Err(error),
        }
    }

    /// Display a spinner while waiting for the result of a future
    pub async fn wait_spinner<T, E>(
        progress_bar: &MultiProgress,
        future: impl Future<Output = Result<T, E>>,
    ) -> MithrilResult<T>
    where
        MithrilError: From<E>,
    {
        let pb = progress_bar.add(ProgressBar::new_spinner());
        let spinner = async move {
            loop {
                pb.tick();
                tokio::time::sleep(Duration::from_millis(50)).await;
            }
        };

        tokio::select! {
            _ = spinner => Err(anyhow!("timeout")),
            res = future => res.map_err(Into::into),
        }
    }

    pub fn format_bytes_to_gigabytes(bytes: u64) -> String {
        let size_in_giga = bytes as f64 / (1024.0 * 1024.0 * 1024.0);

        format!("{size_in_giga:.2} GiB")
    }

    /// Returns the docker run command to run cardano-node with the given ledger format.
    ///
    /// If the ledger format is [legacy](LedgerFormat::Legacy), the cardano node version will
    /// be forced to `10.3.1`.
    pub fn get_docker_run_command<P: AsRef<Path>>(
        canonical_db_filepath: P,
        cardano_network: &str,
        cardano_node_version: &str,
        ledger_format: LedgerFormat,
    ) -> String {
        let db_path = canonical_db_filepath.as_ref();
        let cardano_node_version = if matches!(ledger_format, LedgerFormat::Legacy) {
            "10.3.1"
        } else {
            cardano_node_version
        };
        let cardano_node_config = match ledger_format {
            LedgerFormat::Lmdb => {
                r#" -e CARDANO_CONFIG_JSON_MERGE='{"LedgerDB": { "Backend": "V1LMDB" }}'"#
            }
            _ => "",
        };

        let docker_cmd = format!(
            "docker run -v cardano-node-ipc:/ipc -v cardano-node-data:/data --mount type=bind,source=\"{db_path}\",target=/data/db/ -e NETWORK={cardano_network}{cardano_node_config} ghcr.io/intersectmbo/cardano-node:{cardano_node_version}",
            db_path = db_path.display(),
        );

        docker_cmd
    }
}

#[cfg(test)]
mod test {
    use std::path::PathBuf;

    use super::*;

    #[test]
    fn check_disk_space_error_should_return_warning_message_if_error_is_not_enough_space_for_archive()
     {
        let not_enough_space_error = CardanoDbDownloadCheckerError::NotEnoughSpaceForArchive {
            left_space: 1_f64,
            pathdir: PathBuf::new(),
            archive_size: 2_f64,
        };
        let expected = format!("Warning: {not_enough_space_error}");

        let result = CardanoDbUtils::check_disk_space_error(anyhow!(not_enough_space_error))
            .expect("check_disk_space_error should not error");

        assert!(result.contains(&expected));
    }

    #[test]
    fn check_disk_space_error_should_return_warning_message_if_error_is_not_enough_space_for_uncompressed_data()
     {
        let not_enough_space_error =
            CardanoDbDownloadCheckerError::NotEnoughSpaceForUncompressedData {
                left_space: 1_f64,
                pathdir: PathBuf::new(),
                db_size: 2_f64,
            };
        let expected = format!("Warning: {not_enough_space_error}");

        let result = CardanoDbUtils::check_disk_space_error(anyhow!(not_enough_space_error))
            .expect("check_disk_space_error should not error");

        assert!(result.contains(&expected));
    }

    #[test]
    fn check_disk_space_error_should_return_error_if_error_is_not_error_not_enough_space() {
        let error = CardanoDbDownloadCheckerError::UnpackDirectoryNotEmpty(PathBuf::new());

        let error = CardanoDbUtils::check_disk_space_error(anyhow!(error))
            .expect_err("check_disk_space_error should fail");

        assert!(
            matches!(
                error.downcast_ref::<CardanoDbDownloadCheckerError>(),
                Some(CardanoDbDownloadCheckerError::UnpackDirectoryNotEmpty(_))
            ),
            "Unexpected error: {error:?}"
        );
    }

    #[test]
    fn format_bytes_to_gigabytes_zero() {
        let one_gigabyte = 1024 * 1024 * 1024;

        assert_eq!(CardanoDbUtils::format_bytes_to_gigabytes(0), "0.00 GiB");

        assert_eq!(
            CardanoDbUtils::format_bytes_to_gigabytes(one_gigabyte),
            "1.00 GiB"
        );

        assert_eq!(
            CardanoDbUtils::format_bytes_to_gigabytes(one_gigabyte / 2),
            "0.50 GiB"
        );

        assert_eq!(
            CardanoDbUtils::format_bytes_to_gigabytes(one_gigabyte * 10),
            "10.00 GiB"
        );
    }

    #[test]
    fn get_docker_run_command_for_legacy_ledger() {
        let run_command = CardanoDbUtils::get_docker_run_command(
            Path::new("/path/to/db"),
            "mainnet",
            "whatever",
            LedgerFormat::Legacy,
        );

        assert_eq!(
            run_command,
            r#"docker run -v cardano-node-ipc:/ipc -v cardano-node-data:/data --mount type=bind,source="/path/to/db",target=/data/db/ -e NETWORK=mainnet ghcr.io/intersectmbo/cardano-node:10.3.1"#
        )
    }

    #[test]
    fn get_docker_run_command_for_in_memory_ledger() {
        let run_command = CardanoDbUtils::get_docker_run_command(
            Path::new("/path/to/db"),
            "mainnet",
            "10.5.4",
            LedgerFormat::InMemory,
        );

        assert_eq!(
            run_command,
            r#"docker run -v cardano-node-ipc:/ipc -v cardano-node-data:/data --mount type=bind,source="/path/to/db",target=/data/db/ -e NETWORK=mainnet ghcr.io/intersectmbo/cardano-node:10.5.4"#
        )
    }

    #[test]
    fn get_docker_run_command_for_lmdb_ledger() {
        let run_command = CardanoDbUtils::get_docker_run_command(
            Path::new("/path/to/db"),
            "mainnet",
            "10.6.2",
            LedgerFormat::Lmdb,
        );

        assert_eq!(
            run_command,
            r#"docker run -v cardano-node-ipc:/ipc -v cardano-node-data:/data --mount type=bind,source="/path/to/db",target=/data/db/ -e NETWORK=mainnet -e CARDANO_CONFIG_JSON_MERGE='{"LedgerDB": { "Backend": "V1LMDB" }}' ghcr.io/intersectmbo/cardano-node:10.6.2"#
        )
    }
}
