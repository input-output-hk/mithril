use async_trait::async_trait;

use mithril_common::entities::{Epoch, SupportedEra};
use mithril_common::StdResult;

use crate::{EraMarker, EraReaderAdapter};

/// The goal of the bootstrap adapter is to advertise for the first existing Era
/// while it does not exist yet on any backend. This adapter is intended to be
/// removed once Eras are effectively written in a backend.
pub struct BootstrapAdapter;

#[async_trait]
impl EraReaderAdapter for BootstrapAdapter {
    async fn read(&self) -> StdResult<Vec<EraMarker>> {
        Ok(vec![EraMarker::new(
            &SupportedEra::eras().first().unwrap().to_string(),
            Some(Epoch(0)),
        )])
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn bootstrap_adapter() {
        let adapter = BootstrapAdapter;

        assert_eq!(
            vec![EraMarker::new(&SupportedEra::dummy().to_string(), Some(Epoch(0)))],
            adapter.read().await.expect("bootstrap adapter shall never fail")
        );
    }
}
