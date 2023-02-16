use mithril_common::entities::Snapshot;
use mithril_common::messages::SnapshotListMessage;

/// Adapter to convert [SnapshotListMessage] to SnapshotList instances
pub struct FromSnapshotListMessageAdapter;

impl FromSnapshotListMessageAdapter {
    /// Method to trigger the conversion
    pub fn adapt(snapshot_list_message: SnapshotListMessage) -> Vec<Snapshot> {
        snapshot_list_message
            .into_iter()
            .map(|snapshot_list_item_message| Snapshot {
                digest: snapshot_list_item_message.digest,
                beacon: snapshot_list_item_message.beacon,
                certificate_hash: snapshot_list_item_message.certificate_hash,
                size: snapshot_list_item_message.size,
                created_at: snapshot_list_item_message.created_at,
                locations: snapshot_list_item_message.locations,
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

        assert_eq!(snapshot_list_message[0].digest, snapshot_list[0].digest);
    }
}
