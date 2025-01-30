use serde::{Deserialize, Serialize};

/// Message structure of a complete Cardano database restoration
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct CardanoDatabaseCompleteRestorationMessage {
    /// Number of complete restoration.
    pub nb_complete_restoration: u32,
}

impl CardanoDatabaseCompleteRestorationMessage {
    /// Return a dummy test entity (test-only).
    pub fn dummy() -> Self {
        Self {
            nb_complete_restoration: 34,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const CURRENT_JSON: &str = r#"{
        "nb_complete_restoration": 62
    }"#;

    fn golden_message_current() -> CardanoDatabaseCompleteRestorationMessage {
        CardanoDatabaseCompleteRestorationMessage {
            nb_complete_restoration: 62,
        }
    }

    #[test]
    fn test_current_json_deserialized_into_current_message() {
        let json = CURRENT_JSON;
        let message: CardanoDatabaseCompleteRestorationMessage =
            serde_json::from_str(json).unwrap();

        assert_eq!(golden_message_current(), message);
    }
}
