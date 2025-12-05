use std::sync::Arc;

use rand::{Rng, seq::SliceRandom};
use tokio::sync::Mutex;

use mithril_common::{StdResult, entities::MithrilNetwork};

use crate::{AggregatorDiscoverer, AggregatorEndpoint};

/// A discoverer that returns a random set of aggregators
pub struct ShuffleAggregatorDiscoverer<R: Rng + Send + Sized> {
    random_generator: Arc<Mutex<Box<R>>>,
    inner_discoverer: Arc<dyn AggregatorDiscoverer<AggregatorEndpoint>>,
}

impl<R: Rng + Send + Sized> ShuffleAggregatorDiscoverer<R> {
    /// Creates a new `ShuffleAggregatorDiscoverer` instance with the provided inner discoverer.
    pub fn new(
        inner_discoverer: Arc<dyn AggregatorDiscoverer<AggregatorEndpoint>>,
        random_generator: R,
    ) -> Self {
        Self {
            inner_discoverer,
            random_generator: Arc::new(Mutex::new(Box::new(random_generator))),
        }
    }
}

#[async_trait::async_trait]
impl<R: Rng + Send + Sized> AggregatorDiscoverer<AggregatorEndpoint>
    for ShuffleAggregatorDiscoverer<R>
{
    async fn get_available_aggregators(
        &self,
        network: MithrilNetwork,
    ) -> StdResult<Box<dyn Iterator<Item = AggregatorEndpoint>>> {
        let mut aggregators: Vec<AggregatorEndpoint> = self
            .inner_discoverer
            .get_available_aggregators(network)
            .await?
            .collect();
        let mut rng = self.random_generator.lock().await;
        aggregators.shuffle(&mut *rng);

        Ok(Box::new(aggregators.into_iter()))
    }
}

#[cfg(test)]
mod tests {
    use rand::{SeedableRng, rngs::StdRng};

    use crate::test::double::AggregatorDiscovererFake;

    use super::*;

    #[tokio::test]
    async fn shuffle_aggregator_discoverer() {
        let inner_discoverer = AggregatorDiscovererFake::new(vec![Ok(vec![
            AggregatorEndpoint::new("https://release-devnet-aggregator1".to_string()),
            AggregatorEndpoint::new("https://release-devnet-aggregator2".to_string()),
            AggregatorEndpoint::new("https://release-devnet-aggregator3".to_string()),
        ])]);
        let seed = [0u8; 32];
        let rng = StdRng::from_seed(seed);
        let discoverer = ShuffleAggregatorDiscoverer::new(Arc::new(inner_discoverer), rng);

        let aggregators = discoverer
            .get_available_aggregators(MithrilNetwork::new("release-devnet".into()))
            .await
            .unwrap();

        assert_eq!(
            vec![
                AggregatorEndpoint::new("https://release-devnet-aggregator3".into()),
                AggregatorEndpoint::new("https://release-devnet-aggregator2".into()),
                AggregatorEndpoint::new("https://release-devnet-aggregator1".into()),
            ],
            aggregators.collect::<Vec<_>>()
        );
    }
}
