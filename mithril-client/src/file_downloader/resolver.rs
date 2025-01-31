use std::{collections::HashMap, sync::Arc};

use mithril_common::entities::{ImmutablesLocation, ImmutablesLocationDiscriminants};

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

#[cfg(test)]
mod tests {
    use std::path::Path;

    use mithril_common::entities::{FileUri, MultiFilesUri, TemplateUri};

    use crate::file_downloader::{FileDownloaderUri, MockFileDownloader};

    use super::*;

    #[tokio::test]
    async fn resolves_file_downloader() {
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
}
