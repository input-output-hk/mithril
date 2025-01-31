use std::{collections::HashMap, path::Path};

use async_trait::async_trait;

use mithril_common::{
    entities::{CompressionAlgorithm, FileUri, ImmutableFileNumber, ImmutablesLocation},
    StdResult,
};

/// A file downloader URI
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum FileDownloaderUri {
    /// A single file URI
    FileUri(FileUri),
}

impl FileDownloaderUri {
    /// Expand the immutable locations to a list of file URIs
    pub fn expand_immutable_files_location_to_file_downloader_uris(
        immutable_files_location: &ImmutablesLocation,
        immutable_files_range: &[ImmutableFileNumber],
    ) -> StdResult<Vec<(ImmutableFileNumber, FileDownloaderUri)>> {
        match immutable_files_location {
            ImmutablesLocation::CloudStorage { uri } => {
                let expand_variables = immutable_files_range
                    .iter()
                    .map(|immutable_file_number| {
                        HashMap::from([(
                            "immutable_file_number".to_string(),
                            format!("{:05}", immutable_file_number),
                        )])
                    })
                    .collect();
                let file_downloader_uris = uri
                    .expand_to_file_uris(expand_variables)?
                    .into_iter()
                    .map(FileDownloaderUri::FileUri);
                let immutable_files_range = immutable_files_range.iter().copied();

                Ok(immutable_files_range.zip(file_downloader_uris).collect())
            }
        }
    }

    /// Get the URI as a string
    pub fn as_str(&self) -> &str {
        match self {
            FileDownloaderUri::FileUri(file_uri) => file_uri.0.as_str(),
        }
    }
}

impl From<FileUri> for FileDownloaderUri {
    fn from(file_uri: FileUri) -> Self {
        Self::FileUri(file_uri)
    }
}

/// A file downloader
#[cfg_attr(test, mockall::automock)]
#[async_trait]
pub trait FileDownloader: Sync + Send {
    /// Download and unpack (if necessary) a file on the disk.
    ///
    /// The `download_id` is a unique identifier that allow
    /// [feedback receivers][crate::feedback::FeedbackReceiver] to track concurrent downloads.
    async fn download_unpack(
        &self,
        location: &FileDownloaderUri,
        target_dir: &Path,
        compression_algorithm: Option<CompressionAlgorithm>,
        download_id: &str,
    ) -> StdResult<()>;
}

#[cfg(test)]
mod tests {
    use mithril_common::entities::{MultiFilesUri, TemplateUri};

    use super::*;

    #[test]
    fn immutable_files_location_to_file_downloader_uris() {
        let immutable_files_location = ImmutablesLocation::CloudStorage {
            uri: MultiFilesUri::Template(TemplateUri(
                "http://whatever/{immutable_file_number}.tar.gz".to_string(),
            )),
        };
        let immutable_files_range: Vec<ImmutableFileNumber> = (1..=3).collect();

        let file_downloader_uris =
            FileDownloaderUri::expand_immutable_files_location_to_file_downloader_uris(
                &immutable_files_location,
                &immutable_files_range,
            )
            .unwrap();

        assert_eq!(
            file_downloader_uris,
            vec![
                (
                    1,
                    FileDownloaderUri::FileUri(FileUri("http://whatever/00001.tar.gz".to_string()))
                ),
                (
                    2,
                    FileDownloaderUri::FileUri(FileUri("http://whatever/00002.tar.gz".to_string()))
                ),
                (
                    3,
                    FileDownloaderUri::FileUri(FileUri("http://whatever/00003.tar.gz".to_string()))
                ),
            ]
        );
    }
}
