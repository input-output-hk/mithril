use mithril_common::{
    entities::{SignedEntity, SignedEntityType, Snapshot},
    messages::{FromMessageAdapter, SnapshotMessage},
};

/// Adapter to convert [SnapshotMessage] to [`SignedEntity<Snapshot>`] instances
pub struct FromSnapshotMessageAdapter;

impl FromMessageAdapter<SnapshotMessage, SignedEntity<Snapshot>> for FromSnapshotMessageAdapter {
    /// Method to trigger the conversion
    fn adapt(snapshot_message: SnapshotMessage) -> SignedEntity<Snapshot> {
        let snapshot = Snapshot {
            digest: snapshot_message.digest.clone(),
            beacon: snapshot_message.beacon.clone(),
            size: snapshot_message.size,
            locations: snapshot_message.locations,
            compression_algorithm: snapshot_message.compression_algorithm.unwrap_or_default(),
        };

        SignedEntity {
            signed_entity_id: snapshot_message.digest,
            signed_entity_type: SignedEntityType::CardanoImmutableFilesFull(
                snapshot_message.beacon,
            ),
            certificate_id: snapshot_message.certificate_hash,
            artifact: snapshot,
            created_at: snapshot_message.created_at,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn adapt_ok() {
        let snapshot_message = SnapshotMessage {
            digest: "digest123".to_string(),
            ..Default::default()
        };
        let snapshot_item = FromSnapshotMessageAdapter::adapt(snapshot_message);

        assert_eq!("digest123".to_string(), snapshot_item.artifact.digest);
    }
}
