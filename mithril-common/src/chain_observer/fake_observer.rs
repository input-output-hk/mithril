use async_trait::async_trait;
use tokio::sync::RwLock;

use crate::chain_observer::interface::*;
use crate::crypto_helper::{KESPeriod, OpCert};
use crate::{entities::*, test_utils::fake_data};

/// A Fake [ChainObserver] for testing purpose using fixed data.
pub struct FakeObserver {
    /// A list of [SignerWithStake], used for [get_current_stake_distribution].
    ///
    /// [get_current_stake_distribution]: ChainObserver::get_current_stake_distribution
    pub signers: RwLock<Vec<SignerWithStake>>,

    /// A [Beacon], used by [get_current_epoch]
    ///
    /// [get_current_epoch]: ChainObserver::get_current_epoch
    pub current_beacon: RwLock<Option<Beacon>>,
}

impl FakeObserver {
    /// FakeObserver factory
    pub fn new(current_beacon: Option<Beacon>) -> Self {
        Self {
            signers: RwLock::new(vec![]),
            current_beacon: RwLock::new(current_beacon),
        }
    }

    /// Increase by one the epoch of the [current_beacon][`FakeObserver::current_beacon`].
    pub async fn next_epoch(&self) -> Option<Epoch> {
        let mut current_beacon = self.current_beacon.write().await;
        *current_beacon = current_beacon.as_ref().map(|beacon| Beacon {
            epoch: beacon.epoch + 1,
            ..beacon.clone()
        });

        current_beacon.as_ref().map(|b| b.epoch)
    }

    /// Set the signers that will used to compute the result of
    /// [get_current_stake_distribution][ChainObserver::get_current_stake_distribution].
    pub async fn set_signers(&self, new_signers: Vec<SignerWithStake>) {
        let mut signers = self.signers.write().await;
        *signers = new_signers;
    }
}

impl Default for FakeObserver {
    fn default() -> Self {
        let mut observer = Self::new(Some(fake_data::beacon()));
        observer.signers = RwLock::new(fake_data::signers_with_stakes(2));

        observer
    }
}

#[async_trait]
impl ChainObserver for FakeObserver {
    async fn get_current_epoch(&self) -> Result<Option<Epoch>, ChainObserverError> {
        Ok(self
            .current_beacon
            .read()
            .await
            .as_ref()
            .map(|beacon| beacon.epoch))
    }

    async fn get_current_stake_distribution(
        &self,
    ) -> Result<Option<StakeDistribution>, ChainObserverError> {
        Ok(Some(
            self.signers
                .read()
                .await
                .iter()
                .map(|signer| (signer.party_id.clone() as PartyId, signer.stake as Stake))
                .collect::<StakeDistribution>(),
        ))
    }

    async fn get_current_kes_period(
        &self,
        _opcert: &OpCert,
    ) -> Result<Option<KESPeriod>, ChainObserverError> {
        Ok(Some(0))
    }
}

#[cfg(test)]
mod tests {
    use crate::test_utils::fake_data;

    use super::*;

    #[tokio::test]
    async fn test_get_current_epoch() {
        let beacon = fake_data::beacon();
        let fake_observer = FakeObserver::new(Some(beacon.clone()));
        let current_epoch = fake_observer.get_current_epoch().await.unwrap();

        assert_eq!(Some(beacon.epoch), current_epoch);
    }

    #[tokio::test]
    async fn test_get_current_stake_distribution() {
        let fake_observer = FakeObserver::new(None);
        fake_observer
            .set_signers(fake_data::signers_with_stakes(2))
            .await;
        let stake_distribution = fake_observer.get_current_stake_distribution().await;

        assert_eq!(
            2,
            stake_distribution.unwrap().unwrap().len(),
            "get current stake distribution should not fail and should not be empty"
        );
    }
}
