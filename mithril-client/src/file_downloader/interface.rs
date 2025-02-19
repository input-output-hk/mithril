use std::{collections::HashMap, path::Path};

use anyhow::anyhow;
use async_trait::async_trait;

use mithril_common::{
    entities::{
        AncillaryLocation, CompressionAlgorithm, DigestLocation, FileUri, ImmutableFileNumber,
        ImmutablesLocation,
    },
    StdError, StdResult,
};

use crate::feedback::{MithrilEvent, MithrilEventCardanoDatabase};

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
            ImmutablesLocation::Unknown => {
                Err(anyhow!("Unknown location type to download immutable"))
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

impl From<String> for FileDownloaderUri {
    fn from(location: String) -> Self {
        Self::FileUri(FileUri(location))
    }
}

impl From<FileUri> for FileDownloaderUri {
    fn from(file_uri: FileUri) -> Self {
        Self::FileUri(file_uri)
    }
}

impl TryFrom<AncillaryLocation> for FileDownloaderUri {
    type Error = StdError;

    fn try_from(location: AncillaryLocation) -> Result<Self, Self::Error> {
        match location {
            AncillaryLocation::CloudStorage { uri } => Ok(Self::FileUri(FileUri(uri))),
            AncillaryLocation::Unknown => {
                Err(anyhow!("Unknown location type to download ancillary"))
            }
        }
    }
}

impl TryFrom<DigestLocation> for FileDownloaderUri {
    type Error = StdError;

    fn try_from(location: DigestLocation) -> Result<Self, Self::Error> {
        match location {
            DigestLocation::CloudStorage { uri } | DigestLocation::Aggregator { uri } => {
                Ok(Self::FileUri(FileUri(uri)))
            }
            DigestLocation::Unknown => Err(anyhow!("Unknown location type to download digest")),
        }
    }
}

/// A download event
///
/// The `download_id` is a unique identifier that allow
/// [feedback receivers][crate::feedback::FeedbackReceiver] to track concurrent downloads.
#[derive(Debug, Clone)]
pub enum DownloadEvent {
    /// Immutable file download
    Immutable {
        /// Unique download identifier
        download_id: String,
        /// Immutable file number
        immutable_file_number: ImmutableFileNumber,
    },
    /// Ancillary file download
    Ancillary {
        /// Unique download identifier
        download_id: String,
    },
    /// Digest file download
    Digest {
        /// Unique download identifier
        download_id: String,
    },
    /// Full database download
    Full {
        /// Unique download identifier
        download_id: String,
    },
}

impl DownloadEvent {
    /// Get the unique download identifier
    pub fn download_id(&self) -> &str {
        match self {
            DownloadEvent::Immutable {
                immutable_file_number: _,
                download_id,
            } => download_id,
            DownloadEvent::Ancillary { download_id }
            | DownloadEvent::Digest { download_id }
            | DownloadEvent::Full { download_id } => download_id,
        }
    }

    /// Build a download started event
    pub fn build_download_progress_event(
        &self,
        downloaded_bytes: u64,
        total_bytes: u64,
    ) -> MithrilEvent {
        match self {
            DownloadEvent::Immutable {
                immutable_file_number,
                download_id,
            } => MithrilEvent::CardanoDatabase(
                MithrilEventCardanoDatabase::ImmutableDownloadProgress {
                    download_id: download_id.to_string(),
                    downloaded_bytes,
                    size: total_bytes,
                    immutable_file_number: *immutable_file_number,
                },
            ),
            DownloadEvent::Ancillary { download_id } => MithrilEvent::CardanoDatabase(
                MithrilEventCardanoDatabase::AncillaryDownloadProgress {
                    download_id: download_id.to_string(),
                    downloaded_bytes,
                    size: total_bytes,
                },
            ),
            DownloadEvent::Digest { download_id } => {
                MithrilEvent::CardanoDatabase(MithrilEventCardanoDatabase::DigestDownloadProgress {
                    download_id: download_id.to_string(),
                    downloaded_bytes,
                    size: total_bytes,
                })
            }
            DownloadEvent::Full { download_id } => MithrilEvent::SnapshotDownloadProgress {
                download_id: download_id.to_string(),
                downloaded_bytes,
                size: total_bytes,
            },
        }
    }
}

/// A file downloader
#[cfg_attr(test, mockall::automock)]
#[async_trait]
pub trait FileDownloader: Sync + Send {
    /// Download and unpack (if necessary) a file on the disk.
    ///
    async fn download_unpack(
        &self,
        location: &FileDownloaderUri,
        target_dir: &Path,
        compression_algorithm: Option<CompressionAlgorithm>,
        download_event_type: DownloadEvent,
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

    #[test]
    fn immutable_files_location_to_file_downloader_uris_return_error_when_location_is_unknown() {
        let immutable_files_location = ImmutablesLocation::Unknown;
        let immutable_files_range: Vec<ImmutableFileNumber> = (1..=1).collect();

        FileDownloaderUri::expand_immutable_files_location_to_file_downloader_uris(
            &immutable_files_location,
            &immutable_files_range,
        )
        .expect_err("expand_immutable_files_location_to_file_downloader_uris should fail");
    }

    #[test]
    fn download_event_type_builds_correct_event() {
        let download_event_type = DownloadEvent::Immutable {
            download_id: "download-123".to_string(),
            immutable_file_number: 123,
        };
        let event = download_event_type.build_download_progress_event(123, 1234);
        assert_eq!(
            MithrilEvent::CardanoDatabase(MithrilEventCardanoDatabase::ImmutableDownloadProgress {
                immutable_file_number: 123,
                download_id: "download-123".to_string(),
                downloaded_bytes: 123,
                size: 1234,
            }),
            event,
        );

        let download_event_type = DownloadEvent::Ancillary {
            download_id: "download-123".to_string(),
        };
        let event = download_event_type.build_download_progress_event(123, 1234);
        assert_eq!(
            MithrilEvent::CardanoDatabase(MithrilEventCardanoDatabase::AncillaryDownloadProgress {
                download_id: "download-123".to_string(),
                downloaded_bytes: 123,
                size: 1234,
            }),
            event,
        );

        let download_event_type = DownloadEvent::Digest {
            download_id: "download-123".to_string(),
        };
        let event = download_event_type.build_download_progress_event(123, 1234);
        assert_eq!(
            MithrilEvent::CardanoDatabase(MithrilEventCardanoDatabase::DigestDownloadProgress {
                download_id: "download-123".to_string(),
                downloaded_bytes: 123,
                size: 1234,
            }),
            event,
        );

        let download_event_type = DownloadEvent::Full {
            download_id: "download-123".to_string(),
        };
        let event = download_event_type.build_download_progress_event(123, 1234);
        assert_eq!(
            MithrilEvent::SnapshotDownloadProgress {
                download_id: "download-123".to_string(),
                downloaded_bytes: 123,
                size: 1234,
            },
            event,
        );
    }

    #[test]
    fn file_downloader_uri_from_ancillary_location() {
        let location = AncillaryLocation::CloudStorage {
            uri: "http://whatever/ancillary-1".to_string(),
        };
        let file_downloader_uri: FileDownloaderUri = location.try_into().unwrap();

        assert_eq!(
            FileDownloaderUri::FileUri(FileUri("http://whatever/ancillary-1".to_string())),
            file_downloader_uri
        );
    }
    #[test]
    fn file_downloader_uri_from_unknown_ancillary_location() {
        let location = AncillaryLocation::Unknown;
        let file_downloader_uri: StdResult<FileDownloaderUri> = location.try_into();

        file_downloader_uri.expect_err("try_into should fail on Unknown ancillary location");
    }

    #[test]
    fn file_downloader_uri_from_digest_location() {
        let location = DigestLocation::CloudStorage {
            uri: "http://whatever/digest-1".to_string(),
        };
        let file_downloader_uri: FileDownloaderUri = location.try_into().unwrap();

        assert_eq!(
            FileDownloaderUri::FileUri(FileUri("http://whatever/digest-1".to_string())),
            file_downloader_uri
        );
    }
    #[test]
    fn file_downloader_uri_from_unknown_digest_location() {
        let location = DigestLocation::Unknown;
        let file_downloader_uri: StdResult<FileDownloaderUri> = location.try_into();

        file_downloader_uri.expect_err("try_into should fail on Unknown digest location");
    }
}
