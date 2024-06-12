use async_trait::async_trait;
use tokio::sync::RwLock;

use crate::chain_observer::interface::*;
use crate::chain_observer::{ChainAddress, TxDatum};
use crate::crypto_helper::{KESPeriod, OpCert};
use crate::{entities::*, test_utils::fake_data};

/// A Fake [ChainObserver] for testing purpose using fixed data.
pub struct FakeObserver {
    /// A list of [SignerWithStake], used for [get_current_stake_distribution].
    ///
    /// [get_current_stake_distribution]: ChainObserver::get_current_stake_distribution
    pub signers: RwLock<Vec<SignerWithStake>>,

    /// A [TimePoint], used by [get_current_epoch]
    ///
    /// [get_current_epoch]: ChainObserver::get_current_epoch
    pub current_time_point: RwLock<Option<TimePoint>>,

    /// A list of [TxDatum], used by [get_current_datums]
    ///
    /// [get_current_datums]: ChainObserver::get_current_datums
    pub datums: RwLock<Vec<TxDatum>>,
}

impl FakeObserver {
    /// FakeObserver factory
    pub fn new(current_time_point: Option<TimePoint>) -> Self {
        Self {
            signers: RwLock::new(vec![]),
            current_time_point: RwLock::new(current_time_point.clone()),
            datums: RwLock::new(vec![]),
        }
    }

    /// Increase by one the epoch of the [current_time_point][`FakeObserver::current_time_point`].
    pub async fn next_epoch(&self) -> Option<Epoch> {
        let mut current_time_point = self.current_time_point.write().await;
        *current_time_point = current_time_point.as_ref().map(|time_point| TimePoint {
            epoch: time_point.epoch + 1,
            ..time_point.clone()
        });

        current_time_point.as_ref().map(|b| b.epoch)
    }

    /// Increase the block number of the [current_time_point][`FakeObserver::current_time_point`] by
    /// the given increment.
    pub async fn increase_block_number(&self, increment: BlockNumber) -> Option<BlockNumber> {
        self.change_block_number(|actual_block_number| actual_block_number + increment)
            .await
    }

    /// Decrease the block number of the [current_time_point][`FakeObserver::current_time_point`] by
    /// the given decrement.
    pub async fn decrease_block_number(&self, decrement: BlockNumber) -> Option<BlockNumber> {
        self.change_block_number(|actual_block_number| actual_block_number - decrement)
            .await
    }

    async fn change_block_number(
        &self,
        change_to_apply: impl Fn(u64) -> u64,
    ) -> Option<BlockNumber> {
        let mut current_time_point = self.current_time_point.write().await;

        *current_time_point = current_time_point.as_ref().map(|time_point| TimePoint {
            chain_point: ChainPoint {
                block_number: change_to_apply(time_point.chain_point.block_number),
                ..time_point.chain_point.clone()
            },
            ..time_point.clone()
        });

        current_time_point
            .as_ref()
            .map(|b| b.chain_point.block_number)
    }

    /// Set the signers that will use to compute the result of
    /// [get_current_stake_distribution][ChainObserver::get_current_stake_distribution].
    pub async fn set_signers(&self, new_signers: Vec<SignerWithStake>) {
        let mut signers = self.signers.write().await;
        *signers = new_signers;
    }

    /// Set the time point
    pub async fn set_current_time_point(&self, new_current_time_point: Option<TimePoint>) {
        let mut current_time_point = self.current_time_point.write().await;
        *current_time_point = new_current_time_point;
    }

    /// Set the datums that will use to compute the result of
    /// [get_current_datums][ChainObserver::get_current_datums].
    pub async fn set_datums(&self, new_datums: Vec<TxDatum>) {
        let mut datums = self.datums.write().await;
        *datums = new_datums;
    }
}

impl Default for FakeObserver {
    fn default() -> Self {
        let mut observer = Self::new(Some(TimePoint::dummy()));
        observer.signers = RwLock::new(fake_data::signers_with_stakes(2));

        observer
    }
}

#[async_trait]
impl ChainObserver for FakeObserver {
    async fn get_current_datums(
        &self,
        _address: &ChainAddress,
    ) -> Result<Vec<TxDatum>, ChainObserverError> {
        let datums = self.datums.read().await;
        Ok(datums.to_vec())
    }

    async fn get_current_epoch(&self) -> Result<Option<Epoch>, ChainObserverError> {
        Ok(self
            .current_time_point
            .read()
            .await
            .as_ref()
            .map(|time_point| time_point.epoch))
    }

    async fn get_current_chain_point(&self) -> Result<Option<ChainPoint>, ChainObserverError> {
        Ok(self
            .current_time_point
            .read()
            .await
            .as_ref()
            .map(|time_point| time_point.chain_point.clone()))
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
        let time_point = TimePoint::dummy();
        let fake_observer = FakeObserver::new(Some(time_point.clone()));
        let current_epoch = fake_observer.get_current_epoch().await.unwrap();

        assert_eq!(Some(time_point.epoch), current_epoch);
    }

    #[tokio::test]
    async fn test_get_current_chain_point() {
        let fake_observer = FakeObserver::new(None);
        fake_observer
            .set_current_time_point(Some(TimePoint::dummy()))
            .await;
        let chain_point = fake_observer.get_current_chain_point().await.unwrap();

        assert_eq!(
            Some(TimePoint::dummy().chain_point),
            chain_point,
            "get current chain point should not fail"
        );
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

    #[tokio::test]
    async fn test_get_current_datums() {
        let fake_address = "addr_test_123456".to_string();
        let fake_datums = vec![
            TxDatum("tx_datum_1".to_string()),
            TxDatum("tx_datum_2".to_string()),
        ];
        let fake_observer = FakeObserver::new(None);
        fake_observer.set_datums(fake_datums.clone()).await;
        let datums = fake_observer
            .get_current_datums(&fake_address)
            .await
            .expect("get_current_datums should not fail");

        assert_eq!(fake_datums, datums);
    }

    #[tokio::test]
    async fn test_increase_block_number() {
        let fake_observer = FakeObserver::new(None);
        fake_observer
            .set_current_time_point(Some(TimePoint::dummy()))
            .await;
        fake_observer.increase_block_number(375).await;

        let chain_point = fake_observer.get_current_chain_point().await.unwrap();
        assert_eq!(
            Some(ChainPoint {
                block_number: TimePoint::dummy().chain_point.block_number + 375,
                ..TimePoint::dummy().chain_point
            }),
            chain_point,
            "get current chain point should not fail"
        );
    }

    #[tokio::test]
    async fn test_decrease_block_number() {
        let fake_observer = FakeObserver::new(None);
        fake_observer
            .set_current_time_point(Some(TimePoint {
                chain_point: ChainPoint {
                    block_number: 1000,
                    ..TimePoint::dummy().chain_point
                },
                ..TimePoint::dummy()
            }))
            .await;
        fake_observer.decrease_block_number(800).await;

        let chain_point = fake_observer.get_current_chain_point().await.unwrap();
        assert_eq!(
            Some(ChainPoint {
                block_number: 200,
                ..TimePoint::dummy().chain_point
            }),
            chain_point,
            "get current chain point should not fail"
        );
    }
}
