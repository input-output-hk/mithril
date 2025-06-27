use std::sync::Arc;

use crate::{
    MithrilResult,
    aggregator_client::{AggregatorClient, AggregatorRequest},
};

pub struct InternalStatisticsSender {
    pub(super) aggregator_client: Arc<dyn AggregatorClient>,
}

impl InternalStatisticsSender {
    /// Constructs a new `InternalStaticticsSender`.
    pub fn new(aggregator_client: Arc<dyn AggregatorClient>) -> Self {
        Self { aggregator_client }
    }

    /// Increments the aggregator Cardano database snapshot download statistics
    pub async fn add_statistics(
        &self,
        full_restoration: bool,
        include_ancillary: bool,
        number_of_immutable_files_restored: u64,
    ) -> MithrilResult<()> {
        if include_ancillary {
            self.aggregator_client
                .post_content(AggregatorRequest::IncrementCardanoDatabaseAncillaryStatistic)
                .await?;
        };

        let restoration_request = if full_restoration {
            AggregatorRequest::IncrementCardanoDatabaseCompleteRestorationStatistic
        } else {
            AggregatorRequest::IncrementCardanoDatabasePartialRestorationStatistic
        };

        self.aggregator_client.post_content(restoration_request).await?;

        self.aggregator_client
            .post_content(
                AggregatorRequest::IncrementCardanoDatabaseImmutablesRestoredStatistic {
                    number_of_immutables: number_of_immutable_files_restored,
                },
            )
            .await?;

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use mockall::predicate::{self, eq};

    use crate::{
        aggregator_client::AggregatorRequest,
        cardano_database_client::CardanoDatabaseClientDependencyInjector,
    };

    #[tokio::test]
    async fn add_statistics_with_ancillary() {
        let include_ancillary = true;
        let client = CardanoDatabaseClientDependencyInjector::new()
            .with_aggregator_client_mock_config(|http_client| {
                http_client
                    .expect_post_content()
                    .with(eq(
                        AggregatorRequest::IncrementCardanoDatabaseAncillaryStatistic,
                    ))
                    .times(1)
                    .return_once(|_| Ok("whatever".to_string()));

                http_client
                    .expect_post_content()
                    .returning(|_| Ok("whatever".to_string()));
            })
            .build_cardano_database_client();

        client.add_statistics(false, include_ancillary, 99).await.unwrap();
    }

    #[tokio::test]
    async fn add_statistics_without_ancillary() {
        let include_ancillary = false;
        let client = CardanoDatabaseClientDependencyInjector::new()
            .with_aggregator_client_mock_config(|http_client| {
                http_client
                    .expect_post_content()
                    .with(eq(
                        AggregatorRequest::IncrementCardanoDatabaseAncillaryStatistic,
                    ))
                    .never();

                http_client
                    .expect_post_content()
                    .returning(|_| Ok("whatever".to_string()));
            })
            .build_cardano_database_client();

        client.add_statistics(false, include_ancillary, 99).await.unwrap();
    }

    #[tokio::test]
    async fn add_statistics_with_full_restoration() {
        let full_restoration = true;
        let client = CardanoDatabaseClientDependencyInjector::new()
            .with_aggregator_client_mock_config(|http_client| {
                http_client
                    .expect_post_content()
                    .with(eq(
                        AggregatorRequest::IncrementCardanoDatabaseCompleteRestorationStatistic,
                    ))
                    .times(1)
                    .return_once(|_| Ok("whatever".to_string()));

                http_client
                    .expect_post_content()
                    .with(eq(
                        AggregatorRequest::IncrementCardanoDatabasePartialRestorationStatistic,
                    ))
                    .never();

                http_client
                    .expect_post_content()
                    .returning(|_| Ok("whatever".to_string()));
            })
            .build_cardano_database_client();

        client.add_statistics(full_restoration, false, 99).await.unwrap();
    }

    #[tokio::test]
    async fn add_statistics_with_partial_restoration() {
        let full_restoration = false;
        let client = CardanoDatabaseClientDependencyInjector::new()
            .with_aggregator_client_mock_config(|http_client| {
                http_client
                    .expect_post_content()
                    .with(eq(
                        AggregatorRequest::IncrementCardanoDatabasePartialRestorationStatistic,
                    ))
                    .times(1)
                    .return_once(|_| Ok("whatever".to_string()));

                http_client
                    .expect_post_content()
                    .with(eq(
                        AggregatorRequest::IncrementCardanoDatabaseCompleteRestorationStatistic,
                    ))
                    .never();

                http_client
                    .expect_post_content()
                    .returning(|_| Ok("whatever".to_string()));
            })
            .build_cardano_database_client();

        client.add_statistics(full_restoration, false, 99).await.unwrap();
    }

    #[tokio::test]
    async fn add_statistics_increments_immutable_files_restored() {
        let immutable_files_restored = 123456;
        let client = CardanoDatabaseClientDependencyInjector::new()
            .with_aggregator_client_mock_config(|http_client| {
                http_client
                    .expect_post_content()
                    .with(eq(
                        AggregatorRequest::IncrementCardanoDatabaseImmutablesRestoredStatistic {
                            number_of_immutables: immutable_files_restored,
                        },
                    ))
                    .times(1)
                    .return_once(|_| Ok("whatever".to_string()));

                http_client
                    .expect_post_content()
                    .returning(|_| Ok("whatever".to_string()));
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
            .with_aggregator_client_mock_config(|http_client| {
                http_client
                    .expect_post_content()
                    .with(predicate::function(|req| matches!(req, AggregatorRequest::IncrementCardanoDatabaseImmutablesRestoredStatistic { .. })))
                    .never();

                http_client
                    .expect_post_content()
                    .returning(|_| Ok("whatever".to_string()));
            })
            .build_cardano_database_client();

        client
            .add_statistics(false, false, immutable_files_restored)
            .await
            .unwrap();
    }
}
