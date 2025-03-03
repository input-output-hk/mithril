use anyhow::anyhow;
use mockall::predicate;
use std::path::{Path, PathBuf};

use mithril_common::{
    entities::{CompressionAlgorithm, FileUri},
    StdResult,
};

use super::{DownloadEvent, FileDownloaderUri, MockFileDownloader};

type MockFileDownloaderBuilderReturningFunc = Box<
    dyn FnMut(
            &FileDownloaderUri,
            u64,
            &Path,
            Option<CompressionAlgorithm>,
            DownloadEvent,
        ) -> StdResult<()>
        + Send
        + 'static,
>;

/// A mock file downloader builder
pub struct MockFileDownloaderBuilder {
    mock_file_downloader: Option<MockFileDownloader>,
    times: usize,
    param_file_downloader_uri: Option<FileDownloaderUri>,
    param_target_dir: Option<PathBuf>,
    param_compression_algorithm: Option<Option<CompressionAlgorithm>>,
    returning_func: Option<MockFileDownloaderBuilderReturningFunc>,
}

impl Default for MockFileDownloaderBuilder {
    fn default() -> Self {
        Self {
            mock_file_downloader: None,
            times: 1,
            param_file_downloader_uri: None,
            param_target_dir: None,
            param_compression_algorithm: Some(Some(CompressionAlgorithm::default())),
            returning_func: None,
        }
    }
}

impl MockFileDownloaderBuilder {
    /// Constructs a new MockFileDownloaderBuilder from an existing MockFileDownloader.
    pub fn from_mock(mock: MockFileDownloader) -> Self {
        Self {
            mock_file_downloader: Some(mock),
            ..Self::default()
        }
    }

    /// The MockFileDownloader will succeed
    pub fn with_success(self) -> Self {
        self.with_returning(Box::new(|_, _, _, _, _| Ok(())))
    }

    /// The MockFileDownloader will fail
    pub fn with_failure(self) -> Self {
        self.with_returning(Box::new(|_, _, _, _, _| {
            Err(anyhow!("Download unpack failed"))
        }))
    }

    /// The MockFileDownloader expected number of calls of download_unpack
    pub fn with_times(self, times: usize) -> Self {
        let mut self_mut = self;
        self_mut.times = times;

        self_mut
    }

    /// The MockFileDownloader expected FileDownloaderUri when download_unpack is called
    pub fn with_file_uri<T: AsRef<str>>(self, file_uri: T) -> Self {
        let mut self_mut = self;
        self_mut.param_file_downloader_uri = Some(FileDownloaderUri::FileUri(FileUri(
            file_uri.as_ref().to_string(),
        )));

        self_mut
    }

    /// The MockFileDownloader expected target_dir when download_unpack is called
    pub fn with_target_dir(self, target_dir: PathBuf) -> Self {
        let mut self_mut = self;
        self_mut.param_target_dir = Some(target_dir);

        self_mut
    }

    /// The MockFileDownloader expected compression_algorithm when download_unpack is called
    pub fn with_compression(self, compression: Option<CompressionAlgorithm>) -> Self {
        let mut self_mut = self;
        self_mut.param_compression_algorithm = Some(compression);

        self_mut
    }

    /// The MockFileDownloader will return the result of the returning_func when download_unpack is called
    pub fn with_returning(self, returning_func: MockFileDownloaderBuilderReturningFunc) -> Self {
        let mut self_mut = self;
        self_mut.returning_func = Some(returning_func);

        self_mut
    }

    /// Builds the MockFileDownloader
    pub fn build(self) -> MockFileDownloader {
        let predicate_file_downloader_uri = predicate::function(move |u| {
            self.param_file_downloader_uri
                .as_ref()
                .map(|x| x == u)
                .unwrap_or(true)
        });
        let predicate_target_dir = predicate::function(move |u| {
            self.param_target_dir
                .as_ref()
                .map(|x| x == u)
                .unwrap_or(true)
        });
        let predicate_compression_algorithm = predicate::function(move |u| {
            self.param_compression_algorithm
                .as_ref()
                .map(|x| x == u)
                .unwrap_or(true)
        });
        let predicate_download_event_type = predicate::always();

        let mut mock_file_downloader = self.mock_file_downloader.unwrap_or_default();
        mock_file_downloader
            .expect_download_unpack()
            .with(
                predicate_file_downloader_uri,
                predicate::function(|_| true),
                predicate_target_dir,
                predicate_compression_algorithm,
                predicate_download_event_type,
            )
            .times(self.times)
            .returning(self.returning_func.unwrap());

        mock_file_downloader
    }

    /// Builds the MockFileDownloader and returns a new MockFileDownloaderBuilder
    ///
    /// This helps building multiple expectations for the mock.
    pub fn next_call(self) -> Self {
        let mock_file_downloader = self.build();

        Self::from_mock(mock_file_downloader)
    }
}
