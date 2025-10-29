use std::collections::VecDeque;

use tokio::sync::Mutex;

use mithril_common::StdResult;

use crate::{AggregatorDiscoverer, AggregatorEndpoint, MithrilNetwork};

type AggregatorListReturn = StdResult<Vec<AggregatorEndpoint>>;

/// A fake implementation of the [AggregatorDiscoverer] trait for testing purposes.
pub struct AggregatorDiscovererFake {
    results: Mutex<VecDeque<AggregatorListReturn>>,
}

impl AggregatorDiscovererFake {
    /// Creates a new `AggregatorDiscovererFake` instance with the provided results.
    pub fn new(results: Vec<AggregatorListReturn>) -> Self {
        Self {
            results: Mutex::new(VecDeque::from(results)),
        }
    }
}

#[async_trait::async_trait]
impl AggregatorDiscoverer for AggregatorDiscovererFake {
    async fn get_available_aggregators(
        &self,
        _network: MithrilNetwork,
    ) -> StdResult<Vec<AggregatorEndpoint>> {
        let mut results = self.results.lock().await;

        let endpoints = results.pop_front().ok_or_else(|| {
            anyhow::anyhow!("No more results available in AggregatorDiscovererFake")
        })??;

        Ok(endpoints)
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    #[tokio::test]
    async fn get_available_aggregators_success() {
        let consumer = AggregatorDiscovererFake::new(vec![
            Ok(vec![AggregatorEndpoint::new("test-1".to_string())]),
            Ok(vec![AggregatorEndpoint::new("test-2".to_string())]),
        ]);

        let messages = consumer
            .get_available_aggregators(MithrilNetwork::dummy())
            .await
            .unwrap();

        assert_eq!(
            vec![AggregatorEndpoint::new("test-1".to_string())],
            messages
        );
    }

    #[tokio::test]
    async fn consume_messages_failure() {
        let consumer = AggregatorDiscovererFake::new(vec![
            Err(anyhow::anyhow!("Test error")),
            Ok(vec![AggregatorEndpoint::new("test-2".to_string())]),
        ]);

        consumer
            .get_available_aggregators(MithrilNetwork::dummy())
            .await
            .expect_err("AggregatorDiscovererFake should return an error");
    }
}
