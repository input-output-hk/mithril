use async_trait::async_trait;

use mithril_common::StdResult;
use mithril_common::entities::{Epoch, SupportedEra};

use crate::{EraMarker, EraReaderAdapter};

/// The goal of the latest adapter is to advertise for the last existing Era (test-only)
pub struct LatestAdapter;

#[async_trait]
impl EraReaderAdapter for LatestAdapter {
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
    async fn latest_adapter() {
        let adapter = LatestAdapter;

        assert_eq!(
            vec![EraMarker::new(
                &SupportedEra::eras().last().unwrap().to_string(),
                Some(Epoch(0))
            )],
            adapter.read().await.expect("latest adapter shall never fail")
        );
    }
}
