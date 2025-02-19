use std::collections::HashMap;

use indicatif::{MultiProgress, ProgressBar};
use mithril_client::MithrilResult;
use slog::Logger;
use tokio::sync::RwLock;

use super::{DownloadProgressReporter, ProgressOutputType};

/// A progress reporter that can handle multiple downloads at once.
///
/// It shows a global progress bar for all downloads and individual progress bars for each download.
pub struct MultiDownloadProgressReporter {
    title: String,
    output_type: ProgressOutputType,
    multi_pb: MultiProgress,
    main_reporter: DownloadProgressReporter,
    dl_reporters: RwLock<HashMap<String, DownloadProgressReporter>>,
    logger: Logger,
}

impl MultiDownloadProgressReporter {
    /// Initialize a new `MultiDownloadProgressReporter`.
    pub fn new(
        title: String,
        total_files: u64,
        output_type: ProgressOutputType,
        logger: Logger,
    ) -> Self {
        let multi_pb = MultiProgress::new();
        let main_pb = multi_pb.add(ProgressBar::new(total_files));
        let main_reporter = DownloadProgressReporter::new(main_pb, output_type, logger.clone());

        Self {
            title,
            output_type,
            multi_pb,
            main_reporter,
            dl_reporters: RwLock::new(HashMap::new()),
            logger,
        }
    }

    /// Add a new download to the progress reporter.
    pub async fn add_download<T: Into<String>>(
        &self,
        name: T,
        total_bytes: u64,
    ) -> MithrilResult<()> {
        let dl_progress_bar = self.multi_pb.add(ProgressBar::new(total_bytes));
        let dl_reporter =
            DownloadProgressReporter::new(dl_progress_bar, self.output_type, self.logger.clone());

        let mut reporters = self.dl_reporters.write().await;
        reporters.insert(name.into(), dl_reporter);

        Ok(())
    }

    // todo:
    // progress_download
    // finish_download (remove from dl_reporters + main_reporter + 1)
    // finish_all_downloads (remove all from dl_reporters + main_reporter + output message)

    async fn get_progress_bar(&self, name: &str) -> Option<DownloadProgressReporter> {
        let cdl_reporters = self.dl_reporters.read().await;
        cdl_reporters.get(name).cloned()
    }
}

#[cfg(test)]
mod tests {
    use slog::o;

    use super::*;

    #[tokio::test]
    async fn adding_new_progress_bar() {
        let multi_download_reporter = MultiDownloadProgressReporter::new(
            "Title".to_string(),
            1,
            ProgressOutputType::Hidden,
            slog::Logger::root(slog::Discard, o!()),
        );

        multi_download_reporter
            .add_download("name", 1000)
            .await
            .unwrap();

        assert!(multi_download_reporter
            .get_progress_bar("name")
            .await
            .is_some());
    }
}
