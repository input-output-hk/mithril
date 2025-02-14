use mithril_common::entities::CardanoDatabaseSnapshot;

use crate::{aggregator_client::AggregatorRequest, MithrilResult};

use super::CardanoDatabaseClient;

impl CardanoDatabaseClient {
    /// Increments the aggregator Cardano database snapshot download statistics
    pub async fn add_statistics(
        &self,
        cardano_database_snapshot: &CardanoDatabaseSnapshot,
        full_restoration: bool,
        include_ancillary: bool,
    ) -> MithrilResult<()> {
        // let _response = self
        //     .aggregator_client
        //     .post_content(AggregatorRequest::IncrementSnapshotStatistic {
        //         snapshot: serde_json::to_string(cardano_database_snapshot)?,
        //     })
        //     .await?;

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::aggregator_client::{AggregatorClient, AggregatorClientError};
    use async_trait::async_trait;
    use mockall::{mock, predicate::eq};

    use super::*;

    mock! {
        pub AggregatorClientImpl { }

        #[async_trait]
        impl AggregatorClient for AggregatorClientImpl {
            async fn get_content(
                &self,
                request: AggregatorRequest,
            ) -> Result<String, AggregatorClientError>;

            async fn post_content(
                &self,
                request: AggregatorRequest,
            ) -> Result<String, AggregatorClientError>;
        }
    }

    // Variants:
    // AggregatorRequest::IncrementCardanoDatabaseSnapshotImmutableFilesRestoredStatistic
    // AggregatorRequest::IncrementCardanoDatabaseSnapshotAncillaryFilesRestoredStatistic
    // AggregatorRequest::IncrementCardanoDatabaseSnapshotCompleteRestorationStatistic
    // AggregatorRequest::IncrementCardanoDatabaseSnapshotPartialRestorationStatistic

    // Cases
    // `add_statics` calls post_content with IncrementCardanoDatabaseSnapshotCompleteRestorationStatistic with ImmutableFileRange::Full (=> full_restoration is true)
    // `add_statics` calls post_content with IncrementCardanoDatabaseSnapshotPartialRestorationStatistic with others
    // `add_statics` always calls post_content with IncrementCardanoDatabaseSnapshotImmutableFilesRestoredStatistic
    // `add_statics` calls post_content with IncrementCardanoDatabaseSnapshotAncillaryFilesRestoredStatistic if include_ancillary is true

    #[tokio::test]
    async fn add_statistics_with_ancillary() {
        let aggregator_client = MockAggregatorClientImpl::new();
        aggregator_client
            .expect_post_content()
            .with(eq(
                AggregatorRequest::IncrementCardanoDatabaseSnapshotAncillaryFilesRestoredStatistic,
            ))
            .returning(|_| Ok("whatever".to_string()));

        let cardano_database_client = CardanoDatabaseClient::new(aggregator_client);

        // cardano_database_client
        //     .add_statistics(&snapshot, true, true)
        //     .await
        //     .unwrap();
    }
}
