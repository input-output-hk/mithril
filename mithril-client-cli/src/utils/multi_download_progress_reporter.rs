use std::collections::HashMap;

use indicatif::{MultiProgress, ProgressBar};
use slog::Logger;
use tokio::sync::RwLock;

use super::{
    DownloadProgressReporter, DownloadProgressReporterParams, ProgressBarKind, ProgressOutputType,
};

/// A progress reporter that can handle multiple downloads at once.
///
/// It shows a global progress bar for all downloads and individual progress bars for each download.
pub struct MultiDownloadProgressReporter {
    output_type: ProgressOutputType,
    parent_container: MultiProgress,
    main_reporter: DownloadProgressReporter,
    dl_reporters: RwLock<HashMap<String, DownloadProgressReporter>>,
    logger: Logger,
}

impl MultiDownloadProgressReporter {
    /// Initialize a new `MultiDownloadProgressReporter`.
    pub fn new(total_files: u64, output_type: ProgressOutputType, logger: Logger) -> Self {
        let parent_container = MultiProgress::new();
        parent_container.set_draw_target(output_type.into());
        let main_pb = parent_container.add(ProgressBar::new(total_files));
        let main_reporter = DownloadProgressReporter::new(
            main_pb,
            DownloadProgressReporterParams {
                label: "Files".to_string(),
                output_type,
                progress_bar_kind: ProgressBarKind::Files,
                include_label_in_tty: false,
            },
            logger.clone(),
        );

        Self {
            output_type,
            parent_container,
            main_reporter,
            dl_reporters: RwLock::new(HashMap::new()),
            logger,
        }
    }

    #[cfg(test)]
    /// Get the position of the main progress bar.
    pub fn position(&self) -> u64 {
        self.main_reporter.inner_progress_bar().position()
    }

    #[cfg(test)]
    /// Get the total number of downloads.
    pub fn total_downloads(&self) -> u64 {
        self.main_reporter.inner_progress_bar().length().unwrap_or(0)
    }

    #[cfg(test)]
    /// Get the number of active downloads.
    pub async fn number_of_active_downloads(&self) -> usize {
        self.dl_reporters.read().await.len()
    }

    /// Bump the main progress bar by one.
    pub fn bump_main_bar_progress(&self) {
        self.main_reporter.inc(1);
    }

    /// Add a new child bar to the progress reporter.
    pub async fn add_child_bar<T: Into<String>>(&self, name: T, kind: ProgressBarKind, total: u64) {
        let name = name.into();
        let dl_progress_bar = self.parent_container.add(ProgressBar::new(total));
        let dl_reporter = DownloadProgressReporter::new(
            dl_progress_bar,
            DownloadProgressReporterParams {
                label: name.to_owned(),
                output_type: self.output_type,
                progress_bar_kind: kind,
                include_label_in_tty: true,
            },
            self.logger.clone(),
        );

        let mut reporters = self.dl_reporters.write().await;
        reporters.insert(name, dl_reporter);
    }

    /// Report progress of a child bar, updating the progress bar to the given actual_position.
    pub async fn progress_child_bar<T: AsRef<str>>(&self, name: T, actual_position: u64) {
        if let Some(child_reporter) = self.get_child_bar(name.as_ref()).await {
            child_reporter.report(actual_position);
        }
    }

    /// Finish a child bar, removing it from the progress reporter an bumping the main progress bar.
    pub async fn finish_child_bar<T: Into<String>>(&self, name: T) {
        let name = name.into();
        if let Some(child_reporter) = self.get_child_bar(&name).await {
            child_reporter.finish_and_clear();
            self.parent_container.remove(child_reporter.inner_progress_bar());

            let mut reporters = self.dl_reporters.write().await;
            reporters.remove(&name);

            self.bump_main_bar_progress();
        }
    }

    /// Finish all child bars and the main progress bar and prints a message.
    pub async fn finish_all(&self, message: &str) {
        let mut reporters = self.dl_reporters.write().await;
        for (_name, reporter) in reporters.iter() {
            reporter.finish_and_clear();
            self.parent_container.remove(reporter.inner_progress_bar());
        }
        reporters.clear();

        self.main_reporter.finish(message);
    }

    async fn get_child_bar(&self, name: &str) -> Option<DownloadProgressReporter> {
        let cdl_reporters = self.dl_reporters.read().await;
        cdl_reporters.get(name).cloned()
    }
}

#[cfg(test)]
mod tests {
    use slog::o;

    use super::*;

    #[test]
    fn main_progress_bar_is_of_file_kind() {
        let multi_dl_reporter = MultiDownloadProgressReporter::new(
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
    async fn adding_new_child_bar() {
        let multi_dl_reporter = MultiDownloadProgressReporter::new(
            1,
            ProgressOutputType::Hidden,
            slog::Logger::root(slog::Discard, o!()),
        );

        multi_dl_reporter
            .add_child_bar("name", ProgressBarKind::Bytes, 1000)
            .await;

        assert!(multi_dl_reporter
            .get_child_bar("name")
            .await
            .is_some_and(|dl_reporter| dl_reporter.kind() == ProgressBarKind::Bytes));
    }

    #[tokio::test]
    async fn finishing_child_bar() {
        let multi_dl_reporter = MultiDownloadProgressReporter::new(
            1,
            ProgressOutputType::Hidden,
            slog::Logger::root(slog::Discard, o!()),
        );

        multi_dl_reporter
            .add_child_bar("name", ProgressBarKind::Bytes, 1000)
            .await;

        assert_eq!(
            multi_dl_reporter.main_reporter.inner_progress_bar().position(),
            0
        );

        multi_dl_reporter.finish_child_bar("name").await;

        assert_eq!(
            multi_dl_reporter.main_reporter.inner_progress_bar().position(),
            1
        );
        assert!(multi_dl_reporter.get_child_bar("name").await.is_none());
    }

    #[tokio::test]
    async fn finishing_child_bar_that_does_not_exist() {
        let multi_dl_reporter = MultiDownloadProgressReporter::new(
            1,
            ProgressOutputType::Hidden,
            slog::Logger::root(slog::Discard, o!()),
        );

        assert!(multi_dl_reporter.get_child_bar("name").await.is_none());

        multi_dl_reporter.finish_child_bar("name").await;

        assert_eq!(
            multi_dl_reporter.main_reporter.inner_progress_bar().position(),
            0
        );
    }

    #[tokio::test]
    async fn finishing_all_remove_all_child_bars() {
        let total_files = 132;
        let multi_dl_reporter = MultiDownloadProgressReporter::new(
            total_files,
            ProgressOutputType::Hidden,
            slog::Logger::root(slog::Discard, o!()),
        );

        multi_dl_reporter
            .add_child_bar("first", ProgressBarKind::Bytes, 10)
            .await;
        multi_dl_reporter
            .add_child_bar("second", ProgressBarKind::Bytes, 20)
            .await;
        assert_eq!(multi_dl_reporter.dl_reporters.read().await.len(), 2);

        multi_dl_reporter.finish_all("message").await;

        assert_eq!(multi_dl_reporter.dl_reporters.read().await.len(), 0);
        assert_eq!(
            multi_dl_reporter.main_reporter.inner_progress_bar().position(),
            total_files
        );
        assert!(multi_dl_reporter.main_reporter.inner_progress_bar().is_finished());
    }

    #[tokio::test]
    async fn progress_child_bar_to_the_given_bytes() {
        let multi_dl_reporter = MultiDownloadProgressReporter::new(
            4,
            ProgressOutputType::Hidden,
            slog::Logger::root(slog::Discard, o!()),
        );

        multi_dl_reporter
            .add_child_bar("updated", ProgressBarKind::Bytes, 10)
            .await;
        multi_dl_reporter
            .add_child_bar("other", ProgressBarKind::Bytes, 20)
            .await;

        let updated_progress_bar = multi_dl_reporter.get_child_bar("updated").await.unwrap();
        let other_progress_bar = multi_dl_reporter.get_child_bar("other").await.unwrap();

        assert_eq!(updated_progress_bar.inner_progress_bar().position(), 0);

        multi_dl_reporter.progress_child_bar("updated", 5).await;
        assert_eq!(updated_progress_bar.inner_progress_bar().position(), 5);

        multi_dl_reporter.progress_child_bar("updated", 9).await;
        assert_eq!(updated_progress_bar.inner_progress_bar().position(), 9);

        assert_eq!(
            other_progress_bar.inner_progress_bar().position(),
            0,
            "Other progress bar should not be affected by updating the 'updated' progress bar"
        );
    }

    #[tokio::test]
    async fn progress_child_bar_that_does_not_exist_do_nothing() {
        let multi_dl_reporter = MultiDownloadProgressReporter::new(
            2,
            ProgressOutputType::Hidden,
            slog::Logger::root(slog::Discard, o!()),
        );

        multi_dl_reporter.progress_child_bar("not_exist", 5).await;

        assert!(multi_dl_reporter.get_child_bar("not_exist").await.is_none());
    }

    #[test]
    fn bump_main_bar_progress_increase_its_value_by_one() {
        let multi_dl_reporter = MultiDownloadProgressReporter::new(
            2,
            ProgressOutputType::Hidden,
            slog::Logger::root(slog::Discard, o!()),
        );

        assert_eq!(
            0,
            multi_dl_reporter.main_reporter.inner_progress_bar().position()
        );

        multi_dl_reporter.bump_main_bar_progress();

        assert_eq!(
            1,
            multi_dl_reporter.main_reporter.inner_progress_bar().position()
        );
    }
}
