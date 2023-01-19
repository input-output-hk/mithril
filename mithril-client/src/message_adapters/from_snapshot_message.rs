use mithril_common::entities::Snapshot;
use mithril_common::messages::SnapshotMessage;

/// Adapter to convert [SnapshotMessage] to [Snapshot] instances
pub struct FromSnapshotMessageAdapter;

impl FromSnapshotMessageAdapter {
    /// Method to trigger the conversion
    pub fn adapt(snapshot_message: SnapshotMessage) -> Snapshot {
        Snapshot {
            digest: snapshot_message.digest,
            beacon: snapshot_message.beacon,
            certificate_hash: snapshot_message.certificate_hash,
            size: snapshot_message.size,
            created_at: snapshot_message.created_at,
            locations: snapshot_message.locations,
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
        let snapshot = FromSnapshotMessageAdapter::adapt(snapshot_message);

        assert_eq!("digest123".to_string(), snapshot.digest);
    }
}
