use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};

use crate::entities::CardanoDbBeacon;

/// Message structure of a Cardano database snapshot list
pub type CardanoNodeLedgerStateSnapshotListMessage =
    Vec<CardanoNodeLedgerStateSnapshotListItemMessage>;

/// Message structure of a Cardano database snapshot list item
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct CardanoNodeLedgerStateSnapshotListItemMessage {
    /// Hash of the Cardano node ledger state snapshot.
    pub hash: String,

    /// Mithril beacon on the Cardano chain
    pub beacon: CardanoDbBeacon,

    /// Version of the Cardano node used to create the snapshot.
    pub cardano_node_version: String,

    /// Certificate hash for the Cardano node ledger state snapshot.
    pub certificate_hash: String,

    /// Date and time at which the snapshot was created
    pub created_at: DateTime<Utc>,
}

#[cfg(test)]
mod tests {
    use crate::entities::Epoch;

    use super::*;

    const CURRENT_JSON: &str = r#"
    [
        {
            "hash": "d4071d518a3ace0f6c04a9c0745b9e9560e3e2af1b373bafc4e0398423e9abfb",
            "beacon": {
                "epoch": 123,
                "immutable_file_number": 2345
            },
            "certificate_hash": "f6c01b373bafc4e039844071d5da3ace4a9c0745b9e9560e3e2af01823e9abfb",
            "cardano_node_version": "0.0.1",
            "created_at": "2023-01-19T13:43:05.618857482Z"
        }
    ]"#;

    fn golden_current_message() -> CardanoNodeLedgerStateSnapshotListMessage {
        vec![CardanoNodeLedgerStateSnapshotListItemMessage {
            hash: "d4071d518a3ace0f6c04a9c0745b9e9560e3e2af1b373bafc4e0398423e9abfb".to_string(),
            beacon: CardanoDbBeacon {
                epoch: Epoch(123),
                immutable_file_number: 2345,
            },
            certificate_hash: "f6c01b373bafc4e039844071d5da3ace4a9c0745b9e9560e3e2af01823e9abfb"
                .to_string(),
            created_at: DateTime::parse_from_rfc3339("2023-01-19T13:43:05.618857482Z")
                .unwrap()
                .with_timezone(&Utc),
            cardano_node_version: "0.0.1".to_string(),
        }]
    }

    #[test]
    fn test_current_json_deserialized_into_current_message() {
        let json = CURRENT_JSON;
        let message: CardanoNodeLedgerStateSnapshotListMessage = serde_json::from_str(json).expect(
            "This JSON is expected to be successfully parsed into a CardanoNodeLedgerStateSnapshotListMessage instance.",
        );

        assert_eq!(golden_current_message(), message);
    }
}
