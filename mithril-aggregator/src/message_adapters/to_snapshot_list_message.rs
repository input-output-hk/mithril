use mithril_common::entities::Snapshot;
use mithril_common::messages::{SnapshotListItemMessage, SnapshotListMessage};

/// Adapter to convert a list of [Snapshot] to [SnapshotListMessage] instances
pub struct ToSnapshotListMessageAdapter;

impl ToSnapshotListMessageAdapter {
    /// Method to trigger the conversion
    pub fn adapt(snapshots: Vec<Snapshot>) -> SnapshotListMessage {
        snapshots
            .into_iter()
            .map(|snapshot| SnapshotListItemMessage {
                digest: snapshot.digest,
                beacon: snapshot.beacon,
                certificate_hash: snapshot.certificate_hash,
                size: snapshot.size,
                created_at: snapshot.created_at,
                locations: snapshot.locations,
            })
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::test_utils::fake_data;

    use super::*;

    #[test]
    fn adapt_ok() {
        let mut snapshot = fake_data::snapshots(1)[0].to_owned();
        snapshot.digest = "digest123".to_string();
        let snapshot_list_message = ToSnapshotListMessageAdapter::adapt(vec![snapshot]);

        assert_eq!("digest123".to_string(), snapshot_list_message[0].digest);
    }
}
