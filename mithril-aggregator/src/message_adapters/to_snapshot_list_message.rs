use mithril_common::entities::{SignedEntity, Snapshot};
use mithril_common::messages::{SnapshotListItemMessage, SnapshotListMessage, ToMessageAdapter};

/// Adapter to convert a list of [Snapshot] to [SnapshotListMessage] instances
#[allow(dead_code)]
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
    use super::*;

    #[test]
    fn adapt_ok() {
        let signed_entity = SignedEntity::<Snapshot>::dummy();
        let expected_digest = signed_entity.artifact.digest.clone().to_string();

        let snapshot_list_message = ToSnapshotListMessageAdapter::adapt(vec![signed_entity]);

        assert_eq!(expected_digest, snapshot_list_message[0].digest);
    }
}
