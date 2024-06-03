use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};

use mithril_common::crypto_helper::ProtocolParameters;
use mithril_common::entities::{BlockNumber, Epoch, SignedEntity, SignedEntityType, Snapshot};
use mithril_common::messages::{
    CardanoTransactionSnapshotListItemMessage, CardanoTransactionSnapshotMessage,
    MithrilStakeDistributionListItemMessage, MithrilStakeDistributionMessage,
    SignerWithStakeMessagePart, SnapshotListItemMessage, SnapshotMessage,
};
use mithril_common::signable_builder::Artifact;
use mithril_common::StdError;
use mithril_persistence::database::Hydrator;
use mithril_persistence::sqlite::{HydrationError, Projection, SqLiteEntity};

/// SignedEntity record is the representation of a stored signed_entity.
#[derive(Debug, PartialEq, Clone)]
pub struct SignedEntityRecord {
    /// Signed entity id.
    pub signed_entity_id: String,

    /// Signed entity type.
    pub signed_entity_type: SignedEntityType,

    /// Certificate id for this signed entity.
    pub certificate_id: String,

    /// Raw artifact (in JSON format).
    pub artifact: String,

    /// Date and time when the signed_entity was created
    pub created_at: DateTime<Utc>,
}

#[cfg(test)]
impl SignedEntityRecord {
    pub(crate) fn from_snapshot(
        snapshot: Snapshot,
        certificate_id: String,
        created_at: DateTime<Utc>,
    ) -> Self {
        let entity = serde_json::to_string(&snapshot).unwrap();

        SignedEntityRecord {
            signed_entity_id: snapshot.digest,
            signed_entity_type: SignedEntityType::CardanoImmutableFilesFull(snapshot.beacon),
            certificate_id,
            artifact: entity,
            created_at,
        }
    }

    pub(crate) fn fake_records(number_if_records: usize) -> Vec<SignedEntityRecord> {
        use mithril_common::test_utils::fake_data;

        let snapshots = fake_data::snapshots(number_if_records as u64);
        (0..number_if_records)
            .map(|idx| {
                let snapshot = snapshots.get(idx).unwrap().to_owned();
                let entity = serde_json::to_string(&snapshot).unwrap();
                SignedEntityRecord {
                    signed_entity_id: snapshot.digest,
                    signed_entity_type: SignedEntityType::CardanoImmutableFilesFull(
                        snapshot.beacon,
                    ),
                    certificate_id: format!("certificate-{idx}"),
                    artifact: entity,
                    created_at: DateTime::parse_from_rfc3339("2023-01-19T13:43:05.618857482Z")
                        .unwrap()
                        .with_timezone(&Utc),
                }
            })
            .collect()
    }
}

impl From<SignedEntityRecord> for Snapshot {
    fn from(other: SignedEntityRecord) -> Snapshot {
        serde_json::from_str(&other.artifact).unwrap()
    }
}

impl<T> TryFrom<SignedEntityRecord> for SignedEntity<T>
where
    for<'a> T: Artifact + Serialize + Deserialize<'a>,
{
    type Error = serde_json::error::Error;

    fn try_from(other: SignedEntityRecord) -> Result<SignedEntity<T>, Self::Error> {
        let signed_entity = SignedEntity {
            signed_entity_id: other.signed_entity_id,
            signed_entity_type: other.signed_entity_type,
            created_at: other.created_at,
            certificate_id: other.certificate_id,
            artifact: serde_json::from_str::<T>(&other.artifact)?,
        };

        Ok(signed_entity)
    }
}

impl TryFrom<SignedEntityRecord> for SnapshotMessage {
    type Error = StdError;

    fn try_from(value: SignedEntityRecord) -> Result<Self, Self::Error> {
        let artifact = serde_json::from_str::<Snapshot>(&value.artifact)?;
        let snapshot_message = SnapshotMessage {
            digest: artifact.digest,
            beacon: artifact.beacon,
            certificate_hash: value.certificate_id,
            size: artifact.size,
            created_at: value.created_at,
            locations: artifact.locations,
            compression_algorithm: Some(artifact.compression_algorithm),
            cardano_node_version: Some(artifact.cardano_node_version),
        };

        Ok(snapshot_message)
    }
}

impl TryFrom<SignedEntityRecord> for MithrilStakeDistributionMessage {
    type Error = StdError;

    fn try_from(value: SignedEntityRecord) -> Result<Self, Self::Error> {
        #[derive(Deserialize)]
        struct TmpMithrilStakeDistribution {
            epoch: Epoch,
            signers_with_stake: Vec<SignerWithStakeMessagePart>,
            hash: String,
            protocol_parameters: ProtocolParameters,
        }
        let artifact = serde_json::from_str::<TmpMithrilStakeDistribution>(&value.artifact)?;
        let mithtril_stake_distribution_message = MithrilStakeDistributionMessage {
            epoch: artifact.epoch,
            signers_with_stake: artifact.signers_with_stake,
            hash: artifact.hash,
            certificate_hash: value.certificate_id,
            created_at: value.created_at,
            protocol_parameters: artifact.protocol_parameters.into(),
        };

        Ok(mithtril_stake_distribution_message)
    }
}

impl TryFrom<SignedEntityRecord> for MithrilStakeDistributionListItemMessage {
    type Error = StdError;

    fn try_from(value: SignedEntityRecord) -> Result<Self, Self::Error> {
        #[derive(Deserialize)]
        struct TmpMithrilStakeDistribution {
            epoch: Epoch,
            hash: String,
        }
        let artifact = serde_json::from_str::<TmpMithrilStakeDistribution>(&value.artifact)?;
        let message = MithrilStakeDistributionListItemMessage {
            epoch: artifact.epoch,
            hash: artifact.hash,
            certificate_hash: value.certificate_id,
            created_at: value.created_at,
        };

        Ok(message)
    }
}

impl TryFrom<SignedEntityRecord> for CardanoTransactionSnapshotMessage {
    type Error = StdError;

    fn try_from(value: SignedEntityRecord) -> Result<Self, Self::Error> {
        #[derive(Deserialize)]
        struct TmpCardanoTransaction {
            merkle_root: String,
            block_number: BlockNumber,
            hash: String,
        }
        let artifact = serde_json::from_str::<TmpCardanoTransaction>(&value.artifact)?;
        let cardano_transaction_message = CardanoTransactionSnapshotMessage {
            merkle_root: artifact.merkle_root,
            epoch: value.signed_entity_type.get_epoch(),
            block_number: artifact.block_number,
            hash: artifact.hash,
            certificate_hash: value.certificate_id,
            created_at: value.created_at,
        };

        Ok(cardano_transaction_message)
    }
}

impl TryFrom<SignedEntityRecord> for CardanoTransactionSnapshotListItemMessage {
    type Error = StdError;

    fn try_from(value: SignedEntityRecord) -> Result<Self, Self::Error> {
        #[derive(Deserialize)]
        struct TmpCardanoTransaction {
            merkle_root: String,
            block_number: BlockNumber,
            hash: String,
        }
        let artifact = serde_json::from_str::<TmpCardanoTransaction>(&value.artifact)?;
        let message = CardanoTransactionSnapshotListItemMessage {
            merkle_root: artifact.merkle_root,
            epoch: value.signed_entity_type.get_epoch(),
            block_number: artifact.block_number,
            hash: artifact.hash,
            certificate_hash: value.certificate_id,
            created_at: value.created_at,
        };

        Ok(message)
    }
}

impl TryFrom<SignedEntityRecord> for SnapshotListItemMessage {
    type Error = StdError;

    fn try_from(value: SignedEntityRecord) -> Result<Self, Self::Error> {
        let artifact = serde_json::from_str::<Snapshot>(&value.artifact)?;
        let message = SnapshotListItemMessage {
            digest: artifact.digest,
            beacon: artifact.beacon,
            certificate_hash: value.certificate_id,
            size: artifact.size,
            created_at: value.created_at,
            locations: artifact.locations,
            compression_algorithm: Some(artifact.compression_algorithm),
            cardano_node_version: Some(artifact.cardano_node_version),
        };

        Ok(message)
    }
}

impl SqLiteEntity for SignedEntityRecord {
    fn hydrate(row: sqlite::Row) -> Result<Self, HydrationError>
    where
        Self: Sized,
    {
        let signed_entity_id = row.read::<&str, _>(0).to_string();
        let signed_entity_type_id_int = row.read::<i64, _>(1);
        let certificate_id = row.read::<&str, _>(2).to_string();
        let beacon_str = Hydrator::read_signed_entity_beacon_column(&row, 3);
        let artifact_str = row.read::<&str, _>(4).to_string();
        let created_at = row.read::<&str, _>(5);

        let signed_entity_record = Self {
            signed_entity_id,
            signed_entity_type: Hydrator::hydrate_signed_entity_type(
                signed_entity_type_id_int.try_into().map_err(|e| {
                    HydrationError::InvalidData(format!(
                        "Could not cast i64 ({signed_entity_type_id_int}) to u64. Error: '{e}'"
                    ))
                })?,
                &beacon_str,
            )?,
            certificate_id,
            artifact: artifact_str,
            created_at: DateTime::parse_from_rfc3339(created_at)
                .map_err(|e| {
                    HydrationError::InvalidData(format!(
                        "Could not turn string '{created_at}' to rfc3339 Datetime. Error: {e}"
                    ))
                })?
                .with_timezone(&Utc),
        };

        Ok(signed_entity_record)
    }

    fn get_projection() -> Projection {
        Projection::from(&[
            (
                "signed_entity_id",
                "{:signed_entity:}.signed_entity_id",
                "text",
            ),
            (
                "signed_entity_type_id",
                "{:signed_entity:}.signed_entity_type_id",
                "integer",
            ),
            ("certificate_id", "{:signed_entity:}.certificate_id", "text"),
            ("beacon", "{:signed_entity:}.beacon", "text"),
            ("artifact", "{:signed_entity:}.artifact", "text"),
            ("created_at", "{:signed_entity:}.created_at", "text"),
        ])
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::test_utils::fake_data;

    use super::*;

    #[test]
    fn test_convert_signed_entity() {
        let snapshots = fake_data::snapshots(1);
        let snapshot = snapshots.first().unwrap().to_owned();
        let snapshot_expected = snapshot.clone();

        let signed_entity: SignedEntityRecord = SignedEntityRecord::from_snapshot(
            snapshot,
            "certificate-1".to_string(),
            DateTime::parse_from_rfc3339("2023-01-19T13:43:05.618857482Z")
                .unwrap()
                .with_timezone(&Utc),
        );
        let snapshot: Snapshot = signed_entity.into();
        assert_eq!(snapshot_expected, snapshot);
    }
}
