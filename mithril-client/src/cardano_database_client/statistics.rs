use std::sync::Arc;

use crate::{MithrilResult, cardano_database_client::CardanoDatabaseAggregatorRequest};

pub struct InternalStatisticsSender {
    pub(super) aggregator_requester: Arc<dyn CardanoDatabaseAggregatorRequest>,
}

impl InternalStatisticsSender {
    /// Constructs a new `InternalStaticticsSender`.
    pub fn new(aggregator_requester: Arc<dyn CardanoDatabaseAggregatorRequest>) -> Self {
        Self {
            aggregator_requester,
        }
    }

    /// Increments the aggregator Cardano database snapshot download statistics
    pub async fn add_statistics(
        &self,
        full_restoration: bool,
        include_ancillary: bool,
        number_of_immutable_files_restored: u64,
    ) -> MithrilResult<()> {
        if include_ancillary {
            self.aggregator_requester
                .increment_ancillary_downloaded_statistic()
                .await?;
        };

        if full_restoration {
            self.aggregator_requester
                .increment_cardano_database_complete_restoration_statistic()
                .await?;
        } else {
            self.aggregator_requester
                .increment_cardano_database_partial_restoration_statistic()
                .await?;
        };

        if number_of_immutable_files_restored > 0 {
            self.aggregator_requester
                .increment_immutables_snapshot_restored_statistic(
                    number_of_immutable_files_restored as u32,
                )
                .await?;
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use mockall::predicate::eq;

    use crate::cardano_database_client::CardanoDatabaseClientDependencyInjector;

    #[tokio::test]
    async fn add_statistics_with_ancillary() {
        let include_ancillary = true;
        let client = CardanoDatabaseClientDependencyInjector::new()
            .with_aggregator_requester_mock_config(|requester| {
                requester
                    .expect_increment_ancillary_downloaded_statistic()
                    .times(1)
                    .return_once(|| Ok(()));

                requester
                    .expect_increment_cardano_database_partial_restoration_statistic()
                    .returning(|| Ok(()));
                requester
                    .expect_increment_immutables_snapshot_restored_statistic()
                    .returning(|_| Ok(()));
            })
            .build_cardano_database_client();

        client.add_statistics(false, include_ancillary, 99).await.unwrap();
    }

    #[tokio::test]
    async fn add_statistics_without_ancillary() {
        let include_ancillary = false;
        let client = CardanoDatabaseClientDependencyInjector::new()
            .with_aggregator_requester_mock_config(|requester| {
                requester.expect_increment_ancillary_downloaded_statistic().never();

                requester
                    .expect_increment_cardano_database_partial_restoration_statistic()
                    .returning(|| Ok(()));
                requester
                    .expect_increment_immutables_snapshot_restored_statistic()
                    .returning(|_| Ok(()));
            })
            .build_cardano_database_client();

        client.add_statistics(false, include_ancillary, 99).await.unwrap();
    }

    #[tokio::test]
    async fn add_statistics_with_full_restoration() {
        let full_restoration = true;
        let client = CardanoDatabaseClientDependencyInjector::new()
            .with_aggregator_requester_mock_config(|requester| {
                requester
                    .expect_increment_cardano_database_complete_restoration_statistic()
                    .returning(|| Ok(()))
                    .times(1);
                requester
                    .expect_increment_cardano_database_partial_restoration_statistic()
                    .never();

                requester
                    .expect_increment_ancillary_downloaded_statistic()
                    .returning(|| Ok(()));
                requester
                    .expect_increment_immutables_snapshot_restored_statistic()
                    .returning(|_| Ok(()));
            })
            .build_cardano_database_client();

        client.add_statistics(full_restoration, false, 99).await.unwrap();
    }

    #[tokio::test]
    async fn add_statistics_with_partial_restoration() {
        let full_restoration = false;
        let client = CardanoDatabaseClientDependencyInjector::new()
            .with_aggregator_requester_mock_config(|requester| {
                requester
                    .expect_increment_cardano_database_partial_restoration_statistic()
                    .returning(|| Ok(()))
                    .times(1);
                requester
                    .expect_increment_cardano_database_complete_restoration_statistic()
                    .never();

                requester
                    .expect_increment_immutables_snapshot_restored_statistic()
                    .returning(|_| Ok(()));
            })
            .build_cardano_database_client();

        client.add_statistics(full_restoration, false, 99).await.unwrap();
    }

    #[tokio::test]
    async fn add_statistics_increments_immutable_files_restored() {
        let immutable_files_restored = 123456;
        let client = CardanoDatabaseClientDependencyInjector::new()
            .with_aggregator_requester_mock_config(|requester| {
                requester
                    .expect_increment_immutables_snapshot_restored_statistic()
                    .times(1)
                    .with(eq(immutable_files_restored as u32))
                    .returning(|_| Ok(()));

                requester
                    .expect_increment_cardano_database_partial_restoration_statistic()
                    .returning(|| Ok(()));
            })
            .build_cardano_database_client();

        client
            .add_statistics(false, false, immutable_files_restored)
            .await
            .unwrap();
    }

    #[tokio::test]
    async fn add_statistics_does_not_increment_immutable_files_restored_when_none_restored() {
        let immutable_files_restored = 0;
        let client = CardanoDatabaseClientDependencyInjector::new()
            .with_aggregator_requester_mock_config(|requester| {
                requester
                    .expect_increment_immutables_snapshot_restored_statistic()
                    .never()
                    .returning(|_| Ok(()));

                requester
                    .expect_increment_cardano_database_partial_restoration_statistic()
                    .returning(|| Ok(()));
            })
            .build_cardano_database_client();

        client
            .add_statistics(false, false, immutable_files_restored)
            .await
            .unwrap();
    }
}
