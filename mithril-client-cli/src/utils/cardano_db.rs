use anyhow::anyhow;
use futures::Future;
use indicatif::{MultiProgress, ProgressBar};
use std::time::Duration;

use super::CardanoDbDownloadCheckerError;
use mithril_client::{MithrilError, MithrilResult};

/// Utility functions for to the CardanoDb commands
pub struct CardanoDbUtils;

impl CardanoDbUtils {
    /// Handle the error return by `check_prerequisites`
    pub fn check_disk_space_error(error: MithrilError) -> MithrilResult<String> {
        match error.downcast_ref::<CardanoDbDownloadCheckerError>() {
            Some(CardanoDbDownloadCheckerError::NotEnoughSpaceForArchive { .. })
            | Some(CardanoDbDownloadCheckerError::NotEnoughSpaceForUncompressedData { .. }) => {
                Ok(format!("Warning: {}", error))
            }
            _ => Err(error),
        }
    }

    /// Display a spinner while waiting for the result of a future
    pub async fn wait_spinner<T>(
        progress_bar: &MultiProgress,
        future: impl Future<Output = MithrilResult<T>>,
    ) -> MithrilResult<T> {
        let pb = progress_bar.add(ProgressBar::new_spinner());
        let spinner = async move {
            loop {
                pb.tick();
                tokio::time::sleep(Duration::from_millis(50)).await;
            }
        };

        tokio::select! {
            _ = spinner => Err(anyhow!("timeout")),
            res = future => res,
        }
    }

    pub fn format_bytes_to_gigabytes(bytes: u64) -> String {
        let size_in_giga = bytes as f64 / (1024.0 * 1024.0 * 1024.0);

        format!("{:.2} GiB", size_in_giga)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use std::path::PathBuf;

    #[test]
    fn check_disk_space_error_should_return_warning_message_if_error_is_not_enough_space_for_archive(
    ) {
        let not_enough_space_error = CardanoDbDownloadCheckerError::NotEnoughSpaceForArchive {
            left_space: 1_f64,
            pathdir: PathBuf::new(),
            archive_size: 2_f64,
        };
        let expected = format!("Warning: {}", not_enough_space_error);

        let result = CardanoDbUtils::check_disk_space_error(anyhow!(not_enough_space_error))
            .expect("check_disk_space_error should not error");

        assert!(result.contains(&expected));
    }

    #[test]
    fn check_disk_space_error_should_return_warning_message_if_error_is_not_enough_space_for_uncompressed_data(
    ) {
        let not_enough_space_error =
            CardanoDbDownloadCheckerError::NotEnoughSpaceForUncompressedData {
                left_space: 1_f64,
                pathdir: PathBuf::new(),
                db_size: 2_f64,
            };
        let expected = format!("Warning: {}", not_enough_space_error);

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
            "Unexpected error: {:?}",
            error
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
}
