use async_trait::async_trait;
use std::sync::RwLock;

use crate::era::{EraMarker, EraReaderAdapter};
use crate::StdResult;

/// Dummy adapter is intended to be used in a test environment (end to end test)
/// to simulate not yet activated Eras.
#[derive(Default)]
pub struct DummyAdapter {
    markers: RwLock<Vec<EraMarker>>,
}

impl DummyAdapter {
    /// Create a new instance directly from markers
    pub fn from_markers(markers: Vec<EraMarker>) -> Self {
        let myself = Self::default();
        myself.set_markers(markers);

        myself
    }

    /// Tells what markers should be sent back by the adapter.
    pub fn set_markers(&self, markers: Vec<EraMarker>) {
        let mut my_markers = self.markers.write().unwrap();
        *my_markers = markers;
    }
}

#[async_trait]
impl EraReaderAdapter for DummyAdapter {
    async fn read(&self) -> StdResult<Vec<EraMarker>> {
        let markers = self.markers.read().unwrap();

        Ok((*markers.clone()).to_vec())
    }
}

#[cfg(test)]
mod tests {
    use crate::entities::Epoch;

    use super::super::super::SupportedEra;
    use super::*;

    #[tokio::test]
    async fn empty_dummy_adapter() {
        let adapter = DummyAdapter::default();

        assert!(adapter
            .read()
            .await
            .expect("dummy adapter shall not fail reading")
            .is_empty());
    }

    #[tokio::test]
    async fn dummy_adapter_output() {
        let markers = vec![
            EraMarker::new("one", Some(Epoch(1))),
            EraMarker::new(&SupportedEra::dummy().to_string(), None),
            EraMarker::new(&SupportedEra::dummy().to_string(), Some(Epoch(10))),
        ];

        let adapter = DummyAdapter::default();
        adapter.set_markers(markers.clone());

        assert_eq!(
            markers,
            adapter
                .read()
                .await
                .expect("dummy adapter shall not fail reading")
        );
    }
}
