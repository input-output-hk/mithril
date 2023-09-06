use mithril_common::entities::{SignedEntity, Snapshot};
use mithril_common::messages::{SnapshotListItemMessage, SnapshotListMessage, ToMessageAdapter};

/// Adapter to convert a list of [Snapshot] to [SnapshotListMessage] instances
pub struct ToSnapshotListMessageAdapter;

impl ToMessageAdapter<Vec<SignedEntity<Snapshot>>, SnapshotListMessage>
    for ToSnapshotListMessageAdapter
{
    /// Method to trigger the conversion
    fn adapt(snapshots: Vec<SignedEntity<Snapshot>>) -> SnapshotListMessage {
        snapshots
            .into_iter()
            .map(|entity| SnapshotListItemMessage {
                digest: entity.artifact.digest,
                beacon: entity.artifact.beacon,
                certificate_hash: entity.certificate_id,
                size: entity.artifact.size,
                created_at: entity.created_at,
                locations: entity.artifact.locations,
                compression_algorithm: Some(entity.artifact.compression_algorithm),
                cardano_node_version: Some(entity.artifact.cardano_node_version),
            })
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use chrono::{DateTime, Utc};
    use mithril_common::{
        entities::{Beacon, SignedEntityType},
        test_utils::fake_data,
    };

    use super::*;

    #[test]
    fn adapt_ok() {
        let mut snapshot = fake_data::snapshots(1)[0].to_owned();
        snapshot.digest = "digest123".to_string();
        let signed_entity = SignedEntity {
            signed_entity_id: "signed-entity-id-123".to_string(),
            signed_entity_type: SignedEntityType::CardanoImmutableFilesFull(Beacon::default()),
            certificate_id: "certificate-hash-123".to_string(),
            artifact: snapshot,
            created_at: DateTime::<Utc>::default(),
        };
        let snapshot_list_message = ToSnapshotListMessageAdapter::adapt(vec![signed_entity]);

        assert_eq!("digest123".to_string(), snapshot_list_message[0].digest);
    }
}
