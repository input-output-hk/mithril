use mithril_common::entities::Snapshot;
use mithril_common::messages::{SnapshotDownloadMessage, ToMessageAdapter};

/// Adapter to convert [Snapshot] to [SnapshotDownloadMessage] instances
pub struct ToSnapshotDownloadMessageAdapter;

impl ToMessageAdapter<&Snapshot, SnapshotDownloadMessage> for ToSnapshotDownloadMessageAdapter {
    /// Method to trigger the conversion
    fn adapt(snapshot: &Snapshot) -> SnapshotDownloadMessage {
        SnapshotDownloadMessage {
            digest: snapshot.digest.clone(),
            beacon: snapshot.beacon.clone(),
            size: snapshot.size,
            locations: snapshot.locations.clone(),
            compression_algorithm: snapshot.compression_algorithm,
            cardano_node_version: snapshot.cardano_node_version.clone(),
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
        let snapshot_download_message = ToSnapshotDownloadMessageAdapter::adapt(&snapshot);

        assert_eq!("digest123".to_string(), snapshot_download_message.digest);
    }
}
