use serde::{Deserialize, Serialize};

use crate::entities::{HexEncodedDigest, ImmutableFileName};

/// Message structure of a Cardano database digests list
pub type CardanoDatabaseDigestListMessage = Vec<CardanoDatabaseDigestListItemMessage>;

/// Message structure of a Cardano database digest list item
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct CardanoDatabaseDigestListItemMessage {
    /// Immutable file name
    pub immutable_file_name: ImmutableFileName,

    /// Digest of an immutable file
    pub digest: HexEncodedDigest,
}

impl CardanoDatabaseDigestListItemMessage {
    /// Return a dummy test entity (test-only).
    pub fn dummy() -> Self {
        Self {
            immutable_file_name: "06685.chunk".to_string(),
            digest: "0af556ab2620dd9363bf76963a231abe8948a500ea6be31b131d87907ab09b1e".to_string(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const CURRENT_JSON: &str = r#"
    [
        {
            "immutable_file_name": "06685.chunk",
            "digest": "0af556ab2620dd9363bf76963a231abe8948a500ea6be31b131d87907ab09b1e"
        }
    ]"#;

    fn golden_current_message() -> CardanoDatabaseDigestListMessage {
        vec![CardanoDatabaseDigestListItemMessage {
            immutable_file_name: "06685.chunk".to_string(),
            digest: "0af556ab2620dd9363bf76963a231abe8948a500ea6be31b131d87907ab09b1e".to_string(),
        }]
    }

    #[test]
    fn test_current_json_deserialized_into_current_message() {
        let json = CURRENT_JSON;
        let message: CardanoDatabaseDigestListMessage = serde_json::from_str(json).expect(
            "This JSON is expected to be successfully parsed into a CardanoDatabaseDigestListMessage instance.",
        );

        assert_eq!(golden_current_message(), message);
    }
}
