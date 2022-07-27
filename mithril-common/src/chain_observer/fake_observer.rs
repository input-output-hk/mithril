use async_trait::async_trait;

use crate::chain_observer::interface::*;
use crate::{entities::*, fake_data};

pub struct FakeObserver {
    pub signers: Vec<SignerWithStake>,
    pub current_beacon: Option<Beacon>,
}

impl FakeObserver {
    pub fn new() -> Self {
        Self {
            signers: vec![],
            current_beacon: None,
        }
    }

    pub fn next_epoch(&mut self) -> Option<Epoch> {
        self.current_beacon = self.current_beacon.as_ref().map(|beacon| Beacon {
            epoch: beacon.epoch + 1,
            ..beacon.clone()
        });

        self.current_beacon.as_ref().map(|beacon| beacon.epoch)
    }
}

impl Default for FakeObserver {
    fn default() -> Self {
        let mut observer = Self::new();
        observer.current_beacon = Some(fake_data::beacon());
        observer.signers = fake_data::signers_with_stakes(2);

        observer
    }
}

#[async_trait]
impl ChainObserver for FakeObserver {
    /// Retrieve the current epoch of the Cardano network
    async fn get_current_epoch(&self) -> Result<Option<Epoch>, ChainObserverError> {
        Ok(self.current_beacon.as_ref().map(|beacon| beacon.epoch))
    }

    /// Retrieve the current stake distribution of the Cardano network
    async fn get_current_stake_distribution(
        &self,
    ) -> Result<Option<StakeDistribution>, ChainObserverError> {
        Ok(Some(
            self.signers
                .iter()
                .map(|signer| (signer.party_id.clone() as PartyId, signer.stake as Stake))
                .collect::<StakeDistribution>(),
        ))
    }
}

#[cfg(test)]
mod tests {
    use crate::fake_data;

    use super::*;

    #[tokio::test]
    async fn test_get_current_epoch() {
        let beacon = fake_data::beacon();
        let mut fake_observer = FakeObserver::new();
        fake_observer.current_beacon = Some(beacon.clone());
        let current_epoch = fake_observer.get_current_epoch().await.unwrap();

        assert_eq!(Some(beacon.epoch), current_epoch);
    }

    #[tokio::test]
    async fn test_get_current_stake_distribution() {
        let mut fake_observer = FakeObserver::new();
        fake_observer.signers = fake_data::signers_with_stakes(2);
        let stake_distribution = fake_observer.get_current_stake_distribution().await;

        assert_eq!(
            2,
            stake_distribution.unwrap().unwrap().len(),
            "get current stake distribution should not fail and should not be empty"
        );
    }
}
