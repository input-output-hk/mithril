use async_trait::async_trait;

use mithril_common::StdResult;
use mithril_common::entities::{Epoch, SupportedEra};

use crate::{EraMarker, EraReaderAdapter};

/// The goal of the bootstrap latest adapter is to advertise for the last existing Era (test-only)
pub struct BootstrapLatestAdapter;

#[async_trait]
impl EraReaderAdapter for BootstrapLatestAdapter {
    async fn read(&self) -> StdResult<Vec<EraMarker>> {
        Ok(vec![EraMarker::new(
            &SupportedEra::eras().last().unwrap().to_string(),
            Some(Epoch(0)),
        )])
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn bootstrap_latest_adapter() {
        let adapter = BootstrapLatestAdapter;

        assert_eq!(
            vec![EraMarker::new(
                &SupportedEra::eras().last().unwrap().to_string(),
                Some(Epoch(0))
            )],
            adapter
                .read()
                .await
                .expect("bootstrap latest adapter shall never fail")
        );
    }
}
