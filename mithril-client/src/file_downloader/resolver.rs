use std::{collections::HashMap, sync::Arc};

use mithril_common::entities::{
    DigestLocation, DigestLocationDiscriminants, ImmutablesLocation,
    ImmutablesLocationDiscriminants,
};

use super::FileDownloader;

/// A file downloader resolver
#[cfg_attr(test, mockall::automock)]
pub trait FileDownloaderResolver<L: Sync + Send>: Sync + Send {
    /// Resolve a file downloader for the given location.
    fn resolve(&self, location: &L) -> Option<Arc<dyn FileDownloader>>;
}

/// A file downloader resolver for immutable file locations
pub struct ImmutablesFileDownloaderResolver {
    file_downloaders: HashMap<ImmutablesLocationDiscriminants, Arc<dyn FileDownloader>>,
}

impl ImmutablesFileDownloaderResolver {
    /// Constructs a new `ImmutablesFileDownloaderResolver`.
    pub fn new(
        file_downloaders: Vec<(ImmutablesLocationDiscriminants, Arc<dyn FileDownloader>)>,
    ) -> Self {
        let file_downloaders = file_downloaders.into_iter().collect();

        Self { file_downloaders }
    }
}

impl FileDownloaderResolver<ImmutablesLocation> for ImmutablesFileDownloaderResolver {
    fn resolve(&self, location: &ImmutablesLocation) -> Option<Arc<dyn FileDownloader>> {
        self.file_downloaders.get(&location.into()).cloned()
    }
}

/// A file downloader resolver for digests file locations
pub struct DigestFileDownloaderResolver {
    file_downloaders: HashMap<DigestLocationDiscriminants, Arc<dyn FileDownloader>>,
}

impl DigestFileDownloaderResolver {
    /// Constructs a new `DigestFileDownloaderResolver`.
    pub fn new(
        file_downloaders: Vec<(DigestLocationDiscriminants, Arc<dyn FileDownloader>)>,
    ) -> Self {
        let file_downloaders = file_downloaders.into_iter().collect();

        Self { file_downloaders }
    }
}

impl FileDownloaderResolver<DigestLocation> for DigestFileDownloaderResolver {
    fn resolve(&self, location: &DigestLocation) -> Option<Arc<dyn FileDownloader>> {
        self.file_downloaders.get(&location.into()).cloned()
    }
}

#[cfg(test)]
mod tests {
    use std::path::Path;

    use mithril_common::entities::{FileUri, MultiFilesUri, TemplateUri};

    use crate::file_downloader::{FileDownloaderUri, MockFileDownloader};

    use super::*;

    #[tokio::test]
    async fn immutables_file_downloader_resolver() {
        let mut mock_file_downloader = MockFileDownloader::new();
        mock_file_downloader
            .expect_download_unpack()
            .times(1)
            .returning(|_, _, _, _| Ok(()));
        let resolver = ImmutablesFileDownloaderResolver::new(vec![(
            ImmutablesLocationDiscriminants::CloudStorage,
            Arc::new(mock_file_downloader),
        )]);

        let file_downloader = resolver
            .resolve(&ImmutablesLocation::CloudStorage {
                uri: MultiFilesUri::Template(TemplateUri(
                    "http://whatever/{immutable_file_number}.tar.gz".to_string(),
                )),
            })
            .unwrap();
        file_downloader
            .download_unpack(
                &FileDownloaderUri::FileUri(FileUri("http://whatever/1.tar.gz".to_string())),
                Path::new("."),
                None,
                "download_id",
            )
            .await
            .unwrap();
    }

    #[tokio::test]
    async fn digest_file_downloader_resolver() {
        let mock_file_downloader_aggregator = MockFileDownloader::new();
        let mut mock_file_downloader_cloud_storage = MockFileDownloader::new();
        mock_file_downloader_cloud_storage
            .expect_download_unpack()
            .times(1)
            .returning(|_, _, _, _| Ok(()));
        let resolver = DigestFileDownloaderResolver::new(vec![
            (
                DigestLocationDiscriminants::Aggregator,
                Arc::new(mock_file_downloader_aggregator),
            ),
            (
                DigestLocationDiscriminants::CloudStorage,
                Arc::new(mock_file_downloader_cloud_storage),
            ),
        ]);

        let file_downloader = resolver
            .resolve(&DigestLocation::CloudStorage {
                uri: "http://whatever/00001.tar.gz".to_string(),
            })
            .unwrap();
        file_downloader
            .download_unpack(
                &FileDownloaderUri::FileUri(FileUri("http://whatever/00001.tar.gz".to_string())),
                Path::new("."),
                None,
                "download_id",
            )
            .await
            .unwrap();
    }
}
