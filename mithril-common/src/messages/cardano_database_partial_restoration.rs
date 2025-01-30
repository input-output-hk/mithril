use serde::{Deserialize, Serialize};

/// Message structure of a partial Cardano database restoration
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct CardanoDatabasePartialRestorationMessage {
    /// Number of partial restoration.
    pub nb_partial_restoration: u32,
}

impl CardanoDatabasePartialRestorationMessage {
    /// Return a dummy test entity (test-only).
    pub fn dummy() -> Self {
        Self {
            nb_partial_restoration: 34,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const CURRENT_JSON: &str = r#"{
        "nb_partial_restoration": 62
    }"#;

    fn golden_message_current() -> CardanoDatabasePartialRestorationMessage {
        CardanoDatabasePartialRestorationMessage {
            nb_partial_restoration: 62,
        }
    }

    #[test]
    fn test_current_json_deserialized_into_current_message() {
        let json = CURRENT_JSON;
        let message: CardanoDatabasePartialRestorationMessage = serde_json::from_str(json).unwrap();

        assert_eq!(golden_message_current(), message);
    }
}
