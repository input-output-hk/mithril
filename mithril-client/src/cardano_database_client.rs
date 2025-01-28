//! A client to retrieve Cardano databases data from an Aggregator.
//!
//! In order to do so it defines a [CardanoDatabaseClient] which exposes the following features:
//!  - [get][CardanoDatabaseClient::get]: get a Cardano database data from its hash
//!  - [list][CardanoDatabaseClient::list]: get the list of available Cardano database
//!
//! # Get a Cardano database
//!
//! To get a Cardano database using the [ClientBuilder][crate::client::ClientBuilder].
//!
//! ```no_run
//! # async fn run() -> mithril_client::MithrilResult<()> {
//! use mithril_client::ClientBuilder;
//!
//! let client = ClientBuilder::aggregator("YOUR_AGGREGATOR_ENDPOINT", "YOUR_GENESIS_VERIFICATION_KEY").build()?;
//! let cardano_database = client.cardano_database().get("CARDANO_DATABASE_HASH").await?.unwrap();
//!
//! println!(
//!     "Cardano database hash={}, merkle_root={}, immutable_file_number={:?}",
//!     cardano_database.hash,
//!     cardano_database.merkle_root,
//!     cardano_database.beacon.immutable_file_number
//! );
//! #    Ok(())
//! # }
//! ```
//!
//! # List available Cardano databases
//!
//! To list available Cardano databases using the [ClientBuilder][crate::client::ClientBuilder].
//!
//! ```no_run
//! # async fn run() -> mithril_client::MithrilResult<()> {
//! use mithril_client::ClientBuilder;
//!
//! let client = ClientBuilder::aggregator("YOUR_AGGREGATOR_ENDPOINT", "YOUR_GENESIS_VERIFICATION_KEY").build()?;
//! let cardano_databases = client.cardano_database().list().await?;
//!
//! for cardano_database in cardano_databases {
//!     println!("Cardano database hash={}, immutable_file_number={}", cardano_database.hash, cardano_database.beacon.immutable_file_number);
//! }
//! #    Ok(())
//! # }
//! ```

#[cfg(feature = "fs")]
use anyhow::anyhow;
use anyhow::Context;
#[cfg(feature = "fs")]
use std::ops::RangeInclusive;
use std::sync::Arc;

#[cfg(feature = "fs")]
use mithril_common::{entities::ImmutableFileNumber, StdResult};

use crate::aggregator_client::{AggregatorClient, AggregatorClientError, AggregatorRequest};
use crate::{CardanoDatabaseSnapshot, CardanoDatabaseSnapshotListItem, MithrilResult};

cfg_fs! {
    /// Immutable file range representation
    #[derive(Debug)]
    pub enum ImmutableFileRange {
        /// From the first (included) to the last immutable file number (included)
        Full,

        /// From a specific immutable file number (included) to the last immutable file number (included)
        From(ImmutableFileNumber),

        /// From a specific immutable file number (included) to another specific immutable file number (included)
        Range(ImmutableFileNumber, ImmutableFileNumber),

        /// From the first immutable file number (included) up to a specific immutable file number (included)
        UpTo(ImmutableFileNumber),
    }

    impl ImmutableFileRange {
        /// Returns the range of immutable file numbers
        pub fn to_range_inclusive(
            &self,
            last_immutable_file_number: ImmutableFileNumber,
        ) -> StdResult<RangeInclusive<ImmutableFileNumber>> {
            // TODO: first immutable file is 0 on devnet and 1 otherwise. To be handled properly
            let first_immutable_file_number = 0 as ImmutableFileNumber;
            let full_range = first_immutable_file_number..=last_immutable_file_number;

            match self {
                ImmutableFileRange::Full => Ok(full_range),
                ImmutableFileRange::From(from) if full_range.contains(from) => {
                    Ok(*from..=last_immutable_file_number)
                }
                ImmutableFileRange::Range(from, to)
                    if full_range.contains(from)
                        && full_range.contains(to)
                        && !(*from..=*to).is_empty() =>
                {
                    Ok(*from..=*to)
                }
                ImmutableFileRange::UpTo(to) if full_range.contains(to) => {
                    Ok(first_immutable_file_number..=*to)
                }
                _ => Err(anyhow!("Invalid immutable file range: {self:?}")),
            }
        }
    }

    /// Options for downloading and unpacking a Cardano database
    #[derive(Debug,Default)]
    pub struct DownloadUnpackOptions {
        /// Allow overriding the destination directory
        pub allow_override: bool,

        /// Include ancillary files in the download
        pub include_ancillary: bool
    }
}

/// HTTP client for CardanoDatabase API from the Aggregator
pub struct CardanoDatabaseClient {
    aggregator_client: Arc<dyn AggregatorClient>,
}

impl CardanoDatabaseClient {
    /// Constructs a new `CardanoDatabase`.
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
    async fn fetch_with_aggregator_request(
        &self,
        request: AggregatorRequest,
    ) -> MithrilResult<Option<CardanoDatabaseSnapshot>> {
        match self.aggregator_client.get_content(request).await {
            Ok(content) => {
                let cardano_database: CardanoDatabaseSnapshot = serde_json::from_str(&content)
                    .with_context(|| "CardanoDatabase client can not deserialize artifact")?;

                Ok(Some(cardano_database))
            }
            Err(AggregatorClientError::RemoteServerLogical(_)) => Ok(None),
            Err(e) => Err(e.into()),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    mod cardano_database_client {
        use anyhow::anyhow;
        use chrono::{DateTime, Utc};
        use mithril_common::entities::{CardanoDbBeacon, CompressionAlgorithm, Epoch};
        use mockall::predicate::eq;

        use crate::aggregator_client::MockAggregatorHTTPClient;

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
                    compression_algorithm: CompressionAlgorithm::default(),
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
                    compression_algorithm: CompressionAlgorithm::default(),
                    cardano_node_version: "0.0.1".to_string(),
                },
            ]
        }

        #[tokio::test]
        async fn list_cardano_database_snapshots_returns_messages() {
            let message = fake_messages();
            let mut http_client = MockAggregatorHTTPClient::new();
            http_client
                .expect_get_content()
                .with(eq(AggregatorRequest::ListCardanoDatabaseSnapshots))
                .return_once(move |_| Ok(serde_json::to_string(&message).unwrap()));
            let client = CardanoDatabaseClient::new(Arc::new(http_client));

            let messages = client.list().await.unwrap();

            assert_eq!(2, messages.len());
            assert_eq!("hash-123".to_string(), messages[0].hash);
            assert_eq!("hash-456".to_string(), messages[1].hash);
        }

        #[tokio::test]
        async fn list_cardano_database_snapshots_returns_error_when_invalid_json_structure_in_response(
        ) {
            let mut http_client = MockAggregatorHTTPClient::new();
            http_client
                .expect_get_content()
                .return_once(move |_| Ok("invalid json structure".to_string()));
            let client = CardanoDatabaseClient::new(Arc::new(http_client));

            client
                .list()
                .await
                .expect_err("List Cardano databases should return an error");
        }

        #[tokio::test]
        async fn get_cardano_database_snapshot_returns_message() {
            let expected_cardano_database_snapshot = CardanoDatabaseSnapshot {
                hash: "hash-123".to_string(),
                ..CardanoDatabaseSnapshot::dummy()
            };
            let message = expected_cardano_database_snapshot.clone();
            let mut http_client = MockAggregatorHTTPClient::new();
            http_client
                .expect_get_content()
                .with(eq(AggregatorRequest::GetCardanoDatabaseSnapshot {
                    hash: "hash-123".to_string(),
                }))
                .return_once(move |_| Ok(serde_json::to_string(&message).unwrap()));
            let client = CardanoDatabaseClient::new(Arc::new(http_client));

            let cardano_database = client
                .get("hash-123")
                .await
                .unwrap()
                .expect("This test returns a Cardano database");

            assert_eq!(expected_cardano_database_snapshot, cardano_database);
        }

        #[tokio::test]
        async fn get_cardano_database_snapshot_returns_error_when_invalid_json_structure_in_response(
        ) {
            let mut http_client = MockAggregatorHTTPClient::new();
            http_client
                .expect_get_content()
                .return_once(move |_| Ok("invalid json structure".to_string()));
            let client = CardanoDatabaseClient::new(Arc::new(http_client));

            client
                .get("hash-123")
                .await
                .expect_err("Get Cardano database should return an error");
        }

        #[tokio::test]
        async fn get_cardano_database_snapshot_returns_none_when_not_found_or_remote_server_logical_error(
        ) {
            let mut http_client = MockAggregatorHTTPClient::new();
            http_client.expect_get_content().return_once(move |_| {
                Err(AggregatorClientError::RemoteServerLogical(anyhow!(
                    "not found"
                )))
            });
            let client = CardanoDatabaseClient::new(Arc::new(http_client));

            let result = client.get("hash-123").await.unwrap();

            assert!(result.is_none());
        }

        #[tokio::test]
        async fn get_cardano_database_snapshot_returns_error() {
            let mut http_client = MockAggregatorHTTPClient::new();
            http_client
                .expect_get_content()
                .return_once(move |_| Err(AggregatorClientError::SubsystemError(anyhow!("error"))));
            let client = CardanoDatabaseClient::new(Arc::new(http_client));

            client
                .get("hash-123")
                .await
                .expect_err("Get Cardano database should return an error");
        }
    }

    cfg_fs! {
        mod immutable_file_range {
            use super::*;

            #[test]
            fn to_range_inclusive_with_full() {
                let immutable_file_range = ImmutableFileRange::Full;
                let last_immutable_file_number = 10;

                let result = immutable_file_range.to_range_inclusive(last_immutable_file_number).unwrap();
                assert_eq!(0..=10, result);
            }

            #[test]
            fn to_range_inclusive_with_from() {
                let immutable_file_range = ImmutableFileRange::From(5);

                let last_immutable_file_number = 10;
                let result = immutable_file_range.to_range_inclusive(last_immutable_file_number).unwrap();
                assert_eq!(5..=10, result);

                let last_immutable_file_number = 3;
                immutable_file_range.to_range_inclusive(last_immutable_file_number).expect_err("conversion to range inlusive should fail");
            }

            #[test]
            fn to_range_inclusive_with_range() {
                let immutable_file_range = ImmutableFileRange::Range(5, 8);

                let last_immutable_file_number = 10;
                let result = immutable_file_range.to_range_inclusive(last_immutable_file_number).unwrap();
                assert_eq!(5..=8, result);

                let last_immutable_file_number = 7;
                immutable_file_range.to_range_inclusive(last_immutable_file_number).expect_err("conversion to range inlusive should fail");

                let immutable_file_range = ImmutableFileRange::Range(10, 8);
                immutable_file_range.to_range_inclusive(last_immutable_file_number).expect_err("conversion to range inlusive should fail");

            }

            #[test]
            fn to_range_inclusive_with_up_to() {
                let immutable_file_range = ImmutableFileRange::UpTo(8);

                let last_immutable_file_number = 10;
                let result = immutable_file_range.to_range_inclusive(last_immutable_file_number).unwrap();
                assert_eq!(0..=8, result);

                let last_immutable_file_number = 7;
                immutable_file_range.to_range_inclusive(last_immutable_file_number).expect_err("conversion to range inlusive should fail");
            }
        }
    }
}
