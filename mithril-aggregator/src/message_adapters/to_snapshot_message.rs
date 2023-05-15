use mithril_common::entities::Snapshot;
use mithril_common::messages::{MessageAdapter, SnapshotMessage};

/// Adapter to convert [Snapshot] to [SnapshotMessage] instances
pub struct ToSnapshotMessageAdapter;

impl MessageAdapter<Snapshot, SnapshotMessage> for ToSnapshotMessageAdapter {
    /// Method to trigger the conversion
    fn adapt(snapshot: Snapshot) -> SnapshotMessage {
        SnapshotMessage {
            digest: snapshot.digest,
            beacon: snapshot.beacon,
            certificate_hash: snapshot.certificate_hash,
            size: snapshot.size,
            created_at: snapshot.created_at,
            locations: snapshot.locations,
        }
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
        let snapshot_message = ToSnapshotMessageAdapter::adapt(snapshot);

        assert_eq!("digest123".to_string(), snapshot_message.digest);
    }
}
