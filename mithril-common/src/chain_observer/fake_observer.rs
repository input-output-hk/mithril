use async_trait::async_trait;

use crate::chain_observer::interface::*;
use crate::entities::*;
use crate::fake_data;

pub struct FakeObserver {}

impl FakeObserver {
    pub fn new() -> Self {
        Self {}
    }
}

impl Default for FakeObserver {
    fn default() -> Self {
        Self::new()
    }
}

#[async_trait]
impl ChainObserver for FakeObserver {
    /// Retrieve the current epoch of the Cardano network
    async fn get_current_epoch(&self) -> Result<Option<Epoch>, ChainObserverError> {
        let beacon = fake_data::beacon();
        Ok(Some(beacon.epoch))
    }

    /// Retrieve the current stake distribution of the Cardano network
    async fn get_current_stake_distribution(
        &self,
    ) -> Result<Option<StakeDistribution>, ChainObserverError> {
        let stakes = fake_data::signers_with_stakes(5);
        Ok(Some(
            stakes
                .iter()
                .map(|signer| (signer.party_id.clone() as PartyId, signer.stake as Stake))
                .collect::<StakeDistribution>(),
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_get_current_epoch() {
        let beacon = fake_data::beacon();
        let fake_observer = FakeObserver::new();
        let current_epoch = fake_observer.get_current_epoch().await;
        assert_eq!(Ok(Some(beacon.epoch)), current_epoch);
    }

    #[tokio::test]
    async fn test_get_current_stake_distribution() {
        let fake_observer = FakeObserver::new();
        let stake_distribution = fake_observer.get_current_stake_distribution().await;
        assert!(
            stake_distribution.unwrap().unwrap().len() > 0,
            "get current stake distribution should not fail and should not be empty"
        );
    }
}
