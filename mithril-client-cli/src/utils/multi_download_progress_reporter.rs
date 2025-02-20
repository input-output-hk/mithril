use std::collections::HashMap;

use indicatif::{MultiProgress, ProgressBar};
use slog::Logger;
use tokio::sync::RwLock;

use super::{DownloadProgressReporter, ProgressBarKind, ProgressOutputType};

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
        multi_pb.set_draw_target(output_type.into());
        let main_pb = multi_pb.add(ProgressBar::new(total_files));
        let main_reporter = DownloadProgressReporter::new(
            main_pb,
            output_type,
            ProgressBarKind::Files,
            logger.clone(),
        );

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
    pub async fn add_download<T: Into<String>>(&self, name: T, total_bytes: u64) {
        let dl_progress_bar = self.multi_pb.add(ProgressBar::new(total_bytes));
        let dl_reporter = DownloadProgressReporter::new(
            dl_progress_bar,
            self.output_type,
            ProgressBarKind::Bytes,
            self.logger.clone(),
        );

        let mut reporters = self.dl_reporters.write().await;
        reporters.insert(name.into(), dl_reporter);
    }

    /// Report progress of a download, updating the progress bar to the given actual_position.
    pub async fn progress_download<T: AsRef<str>>(&self, name: T, actual_position: u64) {
        if let Some(child_reporter) = self.get_progress_bar(name.as_ref()).await {
            child_reporter.report(actual_position);
        }
    }

    /// Finish a download, removing it from the progress reporter an bumping the main progress bar.
    pub async fn finish_download<T: Into<String>>(&self, name: T) {
        let name = name.into();
        if let Some(child_reporter) = self.get_progress_bar(&name).await {
            child_reporter.finish_and_clear();
            self.multi_pb.remove(child_reporter.inner_progress_bar());

            let mut reporters = self.dl_reporters.write().await;
            reporters.remove(&name);

            self.main_reporter.inc(1);
        }
    }

    /// Finish all downloads.
    ///
    /// Removes all progress bars, including the main progress bar, and prints a message.
    pub async fn finish_all(&self, message: &str) {
        let mut reporters = self.dl_reporters.write().await;
        for (_name, reporter) in reporters.iter() {
            reporter.finish_and_clear();
            self.multi_pb.remove(reporter.inner_progress_bar());
        }
        reporters.clear();

        self.main_reporter.finish(message);
    }

    async fn get_progress_bar(&self, name: &str) -> Option<DownloadProgressReporter> {
        let cdl_reporters = self.dl_reporters.read().await;
        cdl_reporters.get(name).cloned()
    }
}

#[cfg(test)]
mod tests {
    use slog::o;

    use super::*;

    #[test]
    fn main_progress_bar_is_of_kind() {
        let multi_dl_reporter = MultiDownloadProgressReporter::new(
            "Title".to_string(),
            1,
            ProgressOutputType::Hidden,
            slog::Logger::root(slog::Discard, o!()),
        );

        assert_eq!(
            multi_dl_reporter.main_reporter.kind(),
            ProgressBarKind::Files
        );
    }

    #[tokio::test]
    async fn adding_new_progress_bar() {
        let multi_dl_reporter = MultiDownloadProgressReporter::new(
            "Title".to_string(),
            1,
            ProgressOutputType::Hidden,
            slog::Logger::root(slog::Discard, o!()),
        );

        multi_dl_reporter.add_download("name", 1000).await;

        assert!(multi_dl_reporter
            .get_progress_bar("name")
            .await
            .is_some_and(|dl_reporter| dl_reporter.kind() == ProgressBarKind::Bytes));
    }

    #[tokio::test]
    async fn finishing_progress_bar() {
        let multi_dl_reporter = MultiDownloadProgressReporter::new(
            "Title".to_string(),
            1,
            ProgressOutputType::Hidden,
            slog::Logger::root(slog::Discard, o!()),
        );

        multi_dl_reporter.add_download("name", 1000).await;

        assert_eq!(
            multi_dl_reporter
                .main_reporter
                .inner_progress_bar()
                .position(),
            0
        );

        multi_dl_reporter.finish_download("name").await;

        assert_eq!(
            multi_dl_reporter
                .main_reporter
                .inner_progress_bar()
                .position(),
            1
        );
        assert!(multi_dl_reporter.get_progress_bar("name").await.is_none());
    }

    #[tokio::test]
    async fn finishing_progress_bar_that_does_not_exist() {
        let multi_dl_reporter = MultiDownloadProgressReporter::new(
            "Title".to_string(),
            1,
            ProgressOutputType::Hidden,
            slog::Logger::root(slog::Discard, o!()),
        );

        assert!(multi_dl_reporter.get_progress_bar("name").await.is_none());

        multi_dl_reporter.finish_download("name").await;

        assert_eq!(
            multi_dl_reporter
                .main_reporter
                .inner_progress_bar()
                .position(),
            0
        );
    }

    #[tokio::test]
    async fn finishing_all_remove_all_progress_bars() {
        let total_files = 132;
        let multi_dl_reporter = MultiDownloadProgressReporter::new(
            "Title".to_string(),
            total_files,
            ProgressOutputType::Hidden,
            slog::Logger::root(slog::Discard, o!()),
        );

        multi_dl_reporter.add_download("first", 10).await;
        multi_dl_reporter.add_download("second", 20).await;
        assert_eq!(multi_dl_reporter.dl_reporters.read().await.len(), 2);

        multi_dl_reporter.finish_all("message").await;

        assert_eq!(multi_dl_reporter.dl_reporters.read().await.len(), 0);
        assert_eq!(
            multi_dl_reporter
                .main_reporter
                .inner_progress_bar()
                .position(),
            total_files
        );
        assert!(multi_dl_reporter
            .main_reporter
            .inner_progress_bar()
            .is_finished());
    }

    #[tokio::test]
    async fn progress_download_to_the_given_bytes() {
        let multi_dl_reporter = MultiDownloadProgressReporter::new(
            "Title".to_string(),
            4,
            ProgressOutputType::Hidden,
            slog::Logger::root(slog::Discard, o!()),
        );

        multi_dl_reporter.add_download("updated", 10).await;
        multi_dl_reporter.add_download("other", 20).await;

        let updated_progress_bar = multi_dl_reporter.get_progress_bar("updated").await.unwrap();
        let other_progress_bar = multi_dl_reporter.get_progress_bar("other").await.unwrap();

        assert_eq!(updated_progress_bar.inner_progress_bar().position(), 0);

        multi_dl_reporter.progress_download("updated", 5).await;
        assert_eq!(updated_progress_bar.inner_progress_bar().position(), 5);

        multi_dl_reporter.progress_download("updated", 9).await;
        assert_eq!(updated_progress_bar.inner_progress_bar().position(), 9);

        assert_eq!(
            other_progress_bar.inner_progress_bar().position(),
            0,
            "Other progress bar should not be affected by updating the 'updated' progress bar"
        );
    }

    #[tokio::test]
    async fn progress_download_a_progress_bar_that_does_not_exist() {
        let multi_dl_reporter = MultiDownloadProgressReporter::new(
            "Title".to_string(),
            2,
            ProgressOutputType::Hidden,
            slog::Logger::root(slog::Discard, o!()),
        );

        multi_dl_reporter.progress_download("not_exist", 5).await;

        assert!(multi_dl_reporter
            .get_progress_bar("not_exist")
            .await
            .is_none());
    }
}
