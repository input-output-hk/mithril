use async_trait::async_trait;
use std::{fs, path::PathBuf};

use mithril_common::StdResult;

use crate::{EraMarker, EraReaderAdapter};

/// File adapter is intended to be used in a test environment
/// to simulate eras transitions.
pub struct FileAdapter {
    markers_file: PathBuf,
}

impl FileAdapter {
    /// File adapter factory
    pub fn new(markers_file: PathBuf) -> Self {
        Self { markers_file }
    }
}

#[async_trait]
impl EraReaderAdapter for FileAdapter {
    async fn read(&self) -> StdResult<Vec<EraMarker>> {
        Ok(serde_json::from_str(&fs::read_to_string(
            &self.markers_file,
        )?)?)
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::entities::{Epoch, SupportedEra};
    use mithril_common::test_utils::TempDir;

    use super::*;

    fn get_temp_dir(dir_name: &str) -> PathBuf {
        TempDir::create("era-adapter", dir_name)
    }

    #[tokio::test]
    async fn file_adapter_output() {
        let markers = vec![
            EraMarker::new("one", Some(Epoch(1))),
            EraMarker::new(&SupportedEra::dummy().to_string(), None),
            EraMarker::new(&SupportedEra::dummy().to_string(), Some(Epoch(10))),
        ];
        let temp_file = get_temp_dir("era_reader_file_adapter").join("markers.json");
        fs::write(
            &temp_file,
            serde_json::to_string(&markers).expect("serializing markers should not fail"),
        )
        .expect("writing markers to file should not fail");
        let adapter = FileAdapter::new(temp_file);

        assert_eq!(
            markers,
            adapter.read().await.expect("file adapter shall not fail reading")
        );
    }
}
