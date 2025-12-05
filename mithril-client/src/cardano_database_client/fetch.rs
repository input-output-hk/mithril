use std::sync::Arc;

use mithril_common::entities::EpochSpecifier;

use crate::{
    CardanoDatabaseSnapshot, CardanoDatabaseSnapshotListItem, MithrilResult,
    cardano_database_client::CardanoDatabaseAggregatorRequest, common::Epoch,
};

pub struct InternalArtifactRetriever {
    pub(super) aggregator_requester: Arc<dyn CardanoDatabaseAggregatorRequest>,
}

impl InternalArtifactRetriever {
    /// Constructs a new `InternalArtifactRetriever`
    pub fn new(aggregator_requester: Arc<dyn CardanoDatabaseAggregatorRequest>) -> Self {
        Self {
            aggregator_requester,
        }
    }

    /// Fetch a list of signed CardanoDatabase
    pub async fn list(&self) -> MithrilResult<Vec<CardanoDatabaseSnapshotListItem>> {
        self.aggregator_requester.list_latest().await
    }

    /// Fetch a list of signed CardanoDatabase for a given epoch
    pub async fn list_by_epoch(
        &self,
        epoch: Epoch,
    ) -> MithrilResult<Vec<CardanoDatabaseSnapshotListItem>> {
        self.aggregator_requester
            .list_by_epoch(EpochSpecifier::Number(epoch))
            .await
    }

    /// Fetch a list of signed CardanoDatabase for the latest epoch
    pub async fn list_for_latest_epoch(
        &self,
    ) -> MithrilResult<Vec<CardanoDatabaseSnapshotListItem>> {
        self.aggregator_requester.list_by_epoch(EpochSpecifier::Latest).await
    }

    /// Fetch a list of signed CardanoDatabase for the latest epoch minus the given offset
    pub async fn list_for_latest_epoch_with_offset(
        &self,
        offset: u64,
    ) -> MithrilResult<Vec<CardanoDatabaseSnapshotListItem>> {
        self.aggregator_requester
            .list_by_epoch(EpochSpecifier::LatestMinusOffset(offset))
            .await
    }

    /// Get the given Cardano database data by hash.
    pub async fn get(&self, hash: &str) -> MithrilResult<Option<CardanoDatabaseSnapshot>> {
        self.aggregator_requester.get_by_hash(hash).await
    }
}

#[cfg(test)]
mod tests {

    use anyhow::anyhow;
    use chrono::{DateTime, Utc};
    use mockall::predicate::eq;

    use mithril_common::entities::{CardanoDbBeacon, Epoch};

    use crate::cardano_database_client::CardanoDatabaseClientDependencyInjector;
    use crate::common::test::Dummy;

    use super::*;

    fn fake_messages() -> Vec<CardanoDatabaseSnapshotListItem> {
        vec![
            CardanoDatabaseSnapshotListItem {
                hash: "hash-123".to_string(),
                merkle_root: "mkroot-123".to_string(),
                beacon: CardanoDbBeacon {
                    epoch: Epoch(1),
                    immutable_file_number: 123,
                },
                certificate_hash: "cert-hash-123".to_string(),
                total_db_size_uncompressed: 800796318,
                created_at: DateTime::parse_from_rfc3339("2025-01-19T13:43:05.618857482Z")
                    .unwrap()
                    .with_timezone(&Utc),
                cardano_node_version: "0.0.1".to_string(),
            },
            CardanoDatabaseSnapshotListItem {
                hash: "hash-456".to_string(),
                merkle_root: "mkroot-456".to_string(),
                beacon: CardanoDbBeacon {
                    epoch: Epoch(2),
                    immutable_file_number: 456,
                },
                certificate_hash: "cert-hash-456".to_string(),
                total_db_size_uncompressed: 2960713808,
                created_at: DateTime::parse_from_rfc3339("2025-01-27T15:22:05.618857482Z")
                    .unwrap()
                    .with_timezone(&Utc),
                cardano_node_version: "0.0.1".to_string(),
            },
        ]
    }

    mod list {

        use super::*;

        #[tokio::test]
        async fn list_cardano_database_snapshots_returns_messages() {
            let message = fake_messages();
            let client = CardanoDatabaseClientDependencyInjector::new()
                .with_aggregator_requester_mock_config(|requester| {
                    requester.expect_list_latest().return_once(move || Ok(message));
                })
                .build_cardano_database_client();

            let messages = client.list().await.unwrap();

            assert_eq!(2, messages.len());
            assert_eq!("hash-123".to_string(), messages[0].hash);
            assert_eq!("hash-456".to_string(), messages[1].hash);
        }

        #[tokio::test]
        async fn list_cardano_database_snapshots_by_epoch_returns_messages() {
            let message = fake_messages();
            let client = CardanoDatabaseClientDependencyInjector::new()
                .with_aggregator_requester_mock_config(|requester| {
                    requester
                        .expect_list_by_epoch()
                        .with(eq(EpochSpecifier::Number(Epoch(4))))
                        .return_once(move |_| Ok(message));
                })
                .build_cardano_database_client();

            let messages = client.list_by_epoch(Epoch(4)).await.unwrap();

            assert_eq!(2, messages.len());
            assert_eq!("hash-123".to_string(), messages[0].hash);
            assert_eq!("hash-456".to_string(), messages[1].hash);
        }

        #[tokio::test]
        async fn list_cardano_database_snapshots_for_latest_epoch_returns_messages() {
            let message = fake_messages();
            let client = CardanoDatabaseClientDependencyInjector::new()
                .with_aggregator_requester_mock_config(|requester| {
                    requester
                        .expect_list_by_epoch()
                        .with(eq(EpochSpecifier::Latest))
                        .return_once(move |_| Ok(message));
                })
                .build_cardano_database_client();

            let messages = client.list_for_latest_epoch().await.unwrap();

            assert_eq!(2, messages.len());
            assert_eq!("hash-123".to_string(), messages[0].hash);
            assert_eq!("hash-456".to_string(), messages[1].hash);
        }

        #[tokio::test]
        async fn list_cardano_database_snapshots_for_latest_epoch_with_offset_returns_messages() {
            let message = fake_messages();
            let client = CardanoDatabaseClientDependencyInjector::new()
                .with_aggregator_requester_mock_config(|requester| {
                    requester
                        .expect_list_by_epoch()
                        .with(eq(EpochSpecifier::LatestMinusOffset(42)))
                        .return_once(move |_| Ok(message));
                })
                .build_cardano_database_client();

            let messages = client.list_for_latest_epoch_with_offset(42).await.unwrap();

            assert_eq!(2, messages.len());
            assert_eq!("hash-123".to_string(), messages[0].hash);
            assert_eq!("hash-456".to_string(), messages[1].hash);
        }
    }

    mod get {
        use super::*;

        #[tokio::test]
        async fn get_cardano_database_snapshot_returns_message() {
            let expected_cardano_database_snapshot = CardanoDatabaseSnapshot {
                hash: "hash-123".to_string(),
                ..CardanoDatabaseSnapshot::dummy()
            };
            let message = expected_cardano_database_snapshot.clone();
            let client = CardanoDatabaseClientDependencyInjector::new()
                .with_aggregator_requester_mock_config(|requester| {
                    requester
                        .expect_get_by_hash()
                        .with(eq("hash-123".to_string()))
                        .return_once(move |_| Ok(Some(message)));
                })
                .build_cardano_database_client();

            let cardano_database = client
                .get("hash-123")
                .await
                .unwrap()
                .expect("should return a Cardano database");

            assert_eq!(expected_cardano_database_snapshot, cardano_database);
        }

        #[tokio::test]
        async fn get_cardano_database_snapshot_returns_error() {
            let client = CardanoDatabaseClientDependencyInjector::new()
                .with_aggregator_requester_mock_config(|requester| {
                    requester
                        .expect_get_by_hash()
                        .return_once(move |_| Err(anyhow!("error")));
                })
                .build_cardano_database_client();

            client
                .get("hash-123")
                .await
                .expect_err("Get Cardano database should return an error");
        }
    }
}
