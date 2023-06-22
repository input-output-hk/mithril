use mithril_common::entities::{SignedEntity, Snapshot};
use mithril_common::messages::{MessageAdapter, SnapshotMessage};

/// Adapter to convert [Snapshot] to [SnapshotMessage] instances
pub struct ToSnapshotMessageAdapter;

impl MessageAdapter<SignedEntity<Snapshot>, SnapshotMessage> for ToSnapshotMessageAdapter {
    /// Method to trigger the conversion
    fn adapt(signed_entity: SignedEntity<Snapshot>) -> SnapshotMessage {
        SnapshotMessage {
            digest: signed_entity.artifact.digest,
            beacon: signed_entity.artifact.beacon,
            certificate_hash: signed_entity.certificate_id,
            size: signed_entity.artifact.size,
            created_at: signed_entity.artifact.created_at,
            locations: signed_entity.artifact.locations,
        }
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
        let snapshot_message = ToSnapshotMessageAdapter::adapt(signed_entity);

        assert_eq!("digest123".to_string(), snapshot_message.digest);
    }
}
