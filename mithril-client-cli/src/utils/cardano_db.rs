use anyhow::anyhow;
use futures::Future;
use indicatif::{MultiProgress, ProgressBar};
use std::time::Duration;

use super::CardanoDbUnpackerError;
use mithril_client::{MithrilError, MithrilResult};

/// Utility functions for to the CardanoDb commands
pub struct CardanoDbUtils;

impl CardanoDbUtils {
    /// Handle the error return by `check_prerequisites`
    pub fn check_disk_space_error(error: MithrilError) -> MithrilResult<String> {
        if let Some(CardanoDbUnpackerError::NotEnoughSpace {
            left_space: _,
            pathdir: _,
            archive_size: _,
        }) = error.downcast_ref::<CardanoDbUnpackerError>()
        {
            Ok(format!("Warning: {}", error))
        } else {
            Err(error)
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
}

#[cfg(test)]
mod test {
    use super::*;
    use std::path::PathBuf;

    #[test]
    fn check_disk_space_error_should_return_warning_message_if_error_is_not_enough_space() {
        let not_enough_space_error = CardanoDbUnpackerError::NotEnoughSpace {
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
    fn check_disk_space_error_should_return_error_if_error_is_not_error_not_enough_space() {
        let error = CardanoDbUnpackerError::UnpackDirectoryDoesNotExist(PathBuf::new());

        let error = CardanoDbUtils::check_disk_space_error(anyhow!(error))
            .expect_err("check_disk_space_error should fail");

        assert!(
            matches!(
                error.downcast_ref::<CardanoDbUnpackerError>(),
                Some(CardanoDbUnpackerError::UnpackDirectoryDoesNotExist(_))
            ),
            "Unexpected error: {:?}",
            error
        );
    }
}
