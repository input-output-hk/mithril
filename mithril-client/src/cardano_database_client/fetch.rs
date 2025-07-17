use std::sync::Arc;

use anyhow::Context;
use serde::de::DeserializeOwned;

use crate::{
    CardanoDatabaseSnapshot, CardanoDatabaseSnapshotListItem, MithrilResult,
    aggregator_client::{AggregatorClient, AggregatorClientError, AggregatorRequest},
};

pub struct InternalArtifactRetriever {
    pub(super) aggregator_client: Arc<dyn AggregatorClient>,
}

impl InternalArtifactRetriever {
    /// Constructs a new `InternalArtifactRetriever`
    pub fn new(aggregator_client: Arc<dyn AggregatorClient>) -> Self {
        Self { aggregator_client }
    }

    /// Fetch a list of signed CardanoDatabase
    pub async fn list(&self) -> MithrilResult<Vec<CardanoDatabaseSnapshotListItem>> {
        let response = self
            .aggregator_client
            .get_content(AggregatorRequest::ListCardanoDatabaseSnapshots)
            .await
            .with_context(|| "CardanoDatabase client can not get the artifact list")?;
        let items = serde_json::from_str::<Vec<CardanoDatabaseSnapshotListItem>>(&response)
            .with_context(|| "CardanoDatabase client can not deserialize artifact list")?;

        Ok(items)
    }

    /// Get the given Cardano database data by hash.
    pub async fn get(&self, hash: &str) -> MithrilResult<Option<CardanoDatabaseSnapshot>> {
        self.fetch_with_aggregator_request(AggregatorRequest::GetCardanoDatabaseSnapshot {
            hash: hash.to_string(),
        })
        .await
    }

    /// Fetch the given Cardano database data with an aggregator request.
    /// If it cannot be found, a None is returned.
    async fn fetch_with_aggregator_request<T: DeserializeOwned>(
        &self,
        request: AggregatorRequest,
    ) -> MithrilResult<Option<T>> {
        match self.aggregator_client.get_content(request).await {
            Ok(content) => {
                let result = serde_json::from_str(&content)
                    .with_context(|| "CardanoDatabase client can not deserialize artifact")?;

                Ok(Some(result))
            }
            Err(AggregatorClientError::RemoteServerLogical(_)) => Ok(None),
            Err(e) => Err(e.into()),
        }
    }
}

#[cfg(test)]
mod tests {

    use anyhow::anyhow;
    use chrono::{DateTime, Utc};
    use mockall::predicate::eq;

    use mithril_common::entities::{CardanoDbBeacon, Epoch};
    use mithril_common::test_utils::double::Dummy;

    use crate::cardano_database_client::CardanoDatabaseClientDependencyInjector;

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
                .with_aggregator_client_mock_config(|http_client| {
                    http_client
                        .expect_get_content()
                        .with(eq(AggregatorRequest::ListCardanoDatabaseSnapshots))
                        .return_once(move |_| Ok(serde_json::to_string(&message).unwrap()));
                })
                .build_cardano_database_client();

            let messages = client.list().await.unwrap();

            assert_eq!(2, messages.len());
            assert_eq!("hash-123".to_string(), messages[0].hash);
            assert_eq!("hash-456".to_string(), messages[1].hash);
        }

        #[tokio::test]
        async fn list_cardano_database_snapshots_returns_error_when_invalid_json_structure_in_response()
         {
            let client = CardanoDatabaseClientDependencyInjector::new()
                .with_aggregator_client_mock_config(|http_client| {
                    http_client
                        .expect_get_content()
                        .return_once(move |_| Ok("invalid json structure".to_string()));
                })
                .build_cardano_database_client();

            client
                .list()
                .await
                .expect_err("List Cardano databases should return an error");
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
                .with_aggregator_client_mock_config(|http_client| {
                    http_client
                        .expect_get_content()
                        .with(eq(AggregatorRequest::GetCardanoDatabaseSnapshot {
                            hash: "hash-123".to_string(),
                        }))
                        .return_once(move |_| Ok(serde_json::to_string(&message).unwrap()));
                })
                .build_cardano_database_client();

            let cardano_database = client
                .get("hash-123")
                .await
                .unwrap()
                .expect("This test returns a Cardano database");

            assert_eq!(expected_cardano_database_snapshot, cardano_database);
        }

        #[tokio::test]
        async fn get_cardano_database_snapshot_returns_error_when_invalid_json_structure_in_response()
         {
            let client = CardanoDatabaseClientDependencyInjector::new()
                .with_aggregator_client_mock_config(|http_client| {
                    http_client
                        .expect_get_content()
                        .return_once(move |_| Ok("invalid json structure".to_string()));
                })
                .build_cardano_database_client();

            client
                .get("hash-123")
                .await
                .expect_err("Get Cardano database should return an error");
        }

        #[tokio::test]
        async fn get_cardano_database_snapshot_returns_none_when_not_found_or_remote_server_logical_error()
         {
            let client = CardanoDatabaseClientDependencyInjector::new()
                .with_aggregator_client_mock_config(|http_client| {
                    http_client.expect_get_content().return_once(move |_| {
                        Err(AggregatorClientError::RemoteServerLogical(anyhow!(
                            "not found"
                        )))
                    });
                })
                .build_cardano_database_client();

            let result = client.get("hash-123").await.unwrap();

            assert!(result.is_none());
        }

        #[tokio::test]
        async fn get_cardano_database_snapshot_returns_error() {
            let client = CardanoDatabaseClientDependencyInjector::new()
                .with_aggregator_client_mock_config(|http_client| {
                    http_client.expect_get_content().return_once(move |_| {
                        Err(AggregatorClientError::SubsystemError(anyhow!("error")))
                    });
                })
                .build_cardano_database_client();

            client
                .get("hash-123")
                .await
                .expect_err("Get Cardano database should return an error");
        }
    }
}
