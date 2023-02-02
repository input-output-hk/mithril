use std::error::Error;

use async_trait::async_trait;

use super::super::{EraMarker, EraReaderAdapter};

/// Dummy adapter is intended to be used in a test environment (end to end test)
/// to simulate not yet activated Eras.
#[derive(Default)]
pub struct DummyAdapter {
    markers: Vec<EraMarker>,
}

impl DummyAdapter {
    /// Tells what markers should be sent back by the adapter.
    pub fn set_markers(&mut self, markers: Vec<EraMarker>) {
        self.markers = markers;
    }
}

#[async_trait]
impl EraReaderAdapter for DummyAdapter {
    async fn read(&self) -> Result<Vec<EraMarker>, Box<dyn Error + Sync + Send>> {
        Ok(self.markers.clone())
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

        let mut adapter = DummyAdapter::default();
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
