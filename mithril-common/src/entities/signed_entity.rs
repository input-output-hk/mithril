use chrono::{DateTime, Utc};

use crate::signable_builder::Artifact;
#[cfg(any(test, feature = "test_tools"))]
use crate::test_utils::fake_data;

#[cfg(any(test, feature = "test_tools"))]
use super::{CardanoDbBeacon, Epoch};
use super::{CardanoTransactionsSnapshot, MithrilStakeDistribution, SignedEntityType, Snapshot};

/// Aggregate for signed entity
#[derive(Debug, Clone)]
pub struct SignedEntity<T>
where
    T: Artifact,
{
    /// Signed entity id.
    pub signed_entity_id: String,

    /// Signed entity type.
    pub signed_entity_type: SignedEntityType,

    /// Certificate id for this signed entity.
    pub certificate_id: String,

    /// Artifact
    pub artifact: T,

    /// Date and time when the signed_entity was created
    pub created_at: DateTime<Utc>,
}

impl SignedEntity<Snapshot> {
    cfg_test_tools! {
        /// Create a dummy [SignedEntity] for [Snapshot] entity
        pub fn dummy() -> Self {
            SignedEntity {
                signed_entity_id: "snapshot-id-123".to_string(),
                signed_entity_type: SignedEntityType::CardanoImmutableFilesFull(CardanoDbBeacon::default()),
                certificate_id: "certificate-hash-123".to_string(),
                artifact: fake_data::snapshots(1)[0].to_owned(),
                created_at: DateTime::parse_from_rfc3339("2023-01-19T13:43:05.618857482Z")
                    .unwrap()
                    .with_timezone(&Utc),
            }
        }
    }
}

impl SignedEntity<MithrilStakeDistribution> {
    cfg_test_tools! {
        /// Create a dummy [SignedEntity] for [MithrilStakeDistribution] entity
        pub fn dummy() -> Self {
            SignedEntity {
                signed_entity_id: "mithril-stake-distribution-id-123".to_string(),
                signed_entity_type: SignedEntityType::MithrilStakeDistribution(Epoch(1)),
                certificate_id: "certificate-hash-123".to_string(),
                artifact: fake_data::mithril_stake_distributions(1)[0].to_owned(),
                created_at: DateTime::parse_from_rfc3339("2023-01-19T13:43:05.618857482Z")
                    .unwrap()
                    .with_timezone(&Utc),
            }
        }
    }
}

impl SignedEntity<CardanoTransactionsSnapshot> {
    cfg_test_tools! {
        /// Create a dummy [SignedEntity] for [CardanoTransactionsSnapshot] entity
        pub fn dummy() -> Self {
            let block_number = 50;
            SignedEntity {
                signed_entity_id: "snapshot-id-123".to_string(),
                signed_entity_type: SignedEntityType::CardanoTransactions(Epoch(5), block_number),
                certificate_id: "certificate-hash-123".to_string(),
                artifact: CardanoTransactionsSnapshot::new("mkroot123".to_string(), block_number),
                created_at: DateTime::parse_from_rfc3339("2023-01-19T13:43:05.618857482Z")
                    .unwrap()
                    .with_timezone(&Utc),
            }
        }
    }
}
