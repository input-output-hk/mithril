use async_trait::async_trait;

use crate::entities::Epoch;

use super::super::{EraMarker, EraReaderAdapter, SupportedEra};

/// The goal of the bootstrap adapter is to advertise for the first existing Era
/// while it does not exist yet on any backend. This adapter is intended to be
/// removed once Eras are effectively written in a backend.
pub struct BootstrapAdapter;

#[async_trait]
impl EraReaderAdapter for BootstrapAdapter {
    async fn read(&self) -> Result<Vec<EraMarker>, Box<dyn std::error::Error + Sync + Send>> {
        Ok(vec![EraMarker::new(
            &SupportedEra::eras().first().unwrap().to_string(),
            Some(Epoch(1)),
        )])
    }
}

#[cfg(test)]
mod tests {
    use crate::era::SupportedEra;

    use super::*;

    #[tokio::test]
    async fn bootstrap_adapter() {
        let adapter = BootstrapAdapter;

        assert_eq!(
            vec![EraMarker::new(
                &SupportedEra::dummy().to_string(),
                Some(Epoch(1))
            )],
            adapter
                .read()
                .await
                .expect("bootstrap adapter shall never fail")
        );
    }
}
