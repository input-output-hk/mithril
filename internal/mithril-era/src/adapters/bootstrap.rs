use async_trait::async_trait;

use mithril_common::StdResult;
use mithril_common::entities::{Epoch, SupportedEra};

use crate::{EraMarker, EraReaderAdapter};

/// The goal of the bootstrap adapter is to advertise for the first existing Era (test-only)
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
    use mithril_common::test::double::Dummy;

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
