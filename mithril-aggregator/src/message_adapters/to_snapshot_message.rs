use mithril_common::entities::{SignedEntity, Snapshot};
use mithril_common::messages::{SnapshotMessage, ToMessageAdapter};

/// Adapter to convert [Snapshot] to [SnapshotMessage] instances
pub struct ToSnapshotMessageAdapter;

impl ToMessageAdapter<SignedEntity<Snapshot>, SnapshotMessage> for ToSnapshotMessageAdapter {
    /// Method to trigger the conversion
    fn adapt(signed_entity: SignedEntity<Snapshot>) -> SnapshotMessage {
        SnapshotMessage {
            digest: signed_entity.artifact.digest,
            beacon: signed_entity.artifact.beacon,
            certificate_hash: signed_entity.certificate_id,
            size: signed_entity.artifact.size,
            created_at: signed_entity.created_at,
            locations: signed_entity.artifact.locations,
            compression_algorithm: Some(signed_entity.artifact.compression_algorithm),
            cardano_node_version: Some(signed_entity.artifact.cardano_node_version),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn adapt_ok() {
        let signed_entity = SignedEntity::<Snapshot>::dummy();
        let expected_digest = signed_entity.artifact.digest.clone().to_string();

        let snapshot_message = ToSnapshotMessageAdapter::adapt(signed_entity);

        assert_eq!(expected_digest, snapshot_message.digest);
    }
}
