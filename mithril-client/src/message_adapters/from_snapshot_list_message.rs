use mithril_common::entities::{SignedEntity, SignedEntityType, Snapshot};
use mithril_common::messages::SnapshotListMessage;

/// Adapter to convert [SnapshotListMessage] to SnapshotList instances
pub struct FromSnapshotListMessageAdapter;

impl FromSnapshotListMessageAdapter {
    /// Method to trigger the conversion
    pub fn adapt(snapshot_list_message: SnapshotListMessage) -> Vec<SignedEntity<Snapshot>> {
        snapshot_list_message
            .into_iter()
            .map(|snapshot_list_item_message| {
                let snapshot = Snapshot {
                    digest: snapshot_list_item_message.digest.clone(),
                    beacon: snapshot_list_item_message.beacon.clone(),
                    size: snapshot_list_item_message.size,
                    locations: snapshot_list_item_message.locations,
                };
                SignedEntity {
                    signed_entity_id: snapshot_list_item_message.digest,
                    signed_entity_type: SignedEntityType::CardanoImmutableFilesFull(
                        snapshot_list_item_message.beacon,
                    ),
                    certificate_id: snapshot_list_item_message.certificate_hash,
                    artifact: snapshot,
                    created_at: snapshot_list_item_message.created_at,
                }
            })
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::messages::SnapshotListItemMessage;

    use super::*;

    #[test]
    fn adapt_ok() {
        let snapshot_list_message: SnapshotListMessage = vec![SnapshotListItemMessage::dummy()];
        let snapshot_list = FromSnapshotListMessageAdapter::adapt(snapshot_list_message.clone());

        assert_eq!(
            snapshot_list_message[0].digest,
            snapshot_list[0].signed_entity_id
        );
    }
}
