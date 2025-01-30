use serde::{Deserialize, Serialize};

/// Message structure of an ancillary files restoration
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct CardanoDatabaseAncillaryFilesRestoredMessage {
    /// Number of ancillary files restored.
    pub nb_ancillary_files: u32,
}

impl CardanoDatabaseAncillaryFilesRestoredMessage {
    /// Return a dummy test entity (test-only).
    pub fn dummy() -> Self {
        Self {
            nb_ancillary_files: 34,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const CURRENT_JSON: &str = r#"{
        "nb_ancillary_files": 62
    }"#;

    fn golden_message_current() -> CardanoDatabaseAncillaryFilesRestoredMessage {
        CardanoDatabaseAncillaryFilesRestoredMessage {
            nb_ancillary_files: 62,
        }
    }

    #[test]
    fn test_current_json_deserialized_into_current_message() {
        let json = CURRENT_JSON;
        let message: CardanoDatabaseAncillaryFilesRestoredMessage =
            serde_json::from_str(json).unwrap();

        assert_eq!(golden_message_current(), message);
    }
}
