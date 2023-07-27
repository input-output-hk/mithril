use crate::crypto_helper::ProtocolKey;
use mithril_stm::stm::StmVerificationKeyPoP;

/// Wrapper of [MithrilStm:StmVerificationKeyPoP](type@mithril_stm::stm::StmVerificationKeyPoP)
/// to add serialization utilities.
pub type ProtocolSignerVerificationKey = ProtocolKey<StmVerificationKeyPoP>;

impl ProtocolSignerVerificationKey {
    /// Output the key's bytes in memory
    pub fn to_bytes(&self) -> [u8; 192] {
        self.key().to_bytes()
    }
}

impl From<ProtocolKey<StmVerificationKeyPoP>> for StmVerificationKeyPoP {
    fn from(value: ProtocolKey<StmVerificationKeyPoP>) -> Self {
        *value.key()
    }
}

impl From<StmVerificationKeyPoP> for ProtocolKey<StmVerificationKeyPoP> {
    fn from(value: StmVerificationKeyPoP) -> Self {
        Self::new(value)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use serde::{Deserialize, Serialize};

    const VERIFICATION_KEY: &str = "7b22766b223a5b3134352c32332c3135382c31322c3138332c3230392c33322c3134302c33372c3132342c3136362c3231352c3136302c3231352c3235302c3133342c3135342c3235302c3234312c3230362c3139342c3232322c382c35392c33332c392c35382c322c3235312c31302c33322c3135352c3232372c3134332c3232362c35372c3135312c37342c3139392c3131372c37352c3136382c3134302c34362c3233392c3134352c37322c31362c32312c3138312c3139332c3134362c38362c3231332c3230342c3139332c3232332c32352c3135372c33342c33332c3232372c35312c3132362c3132362c3135362c36342c3232302c3139392c3231332c31362c34352c3131302c3234332c33352c3134382c37312c3231382c3132342c3132332c31362c3132312c3135322c31382c32362c3231322c3231342c3230312c3139302c3137342c3131352c39372c3234392c3235342c3131362c3234335d2c22706f70223a5b3138332c3134352c3133392c3234322c3132302c3136302c35362c3131382c3234322c3230342c39312c38392c32312c3138342c382c34372c3231332c3130352c36332c3135302c32312c3231372c352c382c3231392c3138382c3131342c3230352c3136362c31362c3234302c3234302c3231342c31362c3230342c3231382c3139332c3138312c32342c35362c34352c39392c3234342c38312c32352c35322c3232342c36372c3136382c3136392c3130392c3132322c38372c34392c3137302c3138312c3135312c31352c3235322c3139352c3231312c3233342c3139352c34392c39312c31392c35312c3234312c33332c35382c3134302c3235322c3234322c362c342c34302c32312c3136372c3234392c3235312c33362c38372c36302c39362c36392c3135322c3231302c39382c3136352c352c362c34312c39362c3233352c37352c3138335d7d";

    #[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
    struct Container {
        key: ProtocolSignerVerificationKey,
    }

    #[test]
    fn serializing_directly_does_not_change_the_string() {
        let key: ProtocolSignerVerificationKey = VERIFICATION_KEY.try_into().unwrap();
        let serialized = serde_json::to_string(&key).expect("Serialization should not fail");

        // Note: in json strings are enclosed in quotes
        assert_eq!(format!("\"{VERIFICATION_KEY}\""), serialized);
    }

    #[test]
    fn serialize_deserialize_are_the_same_object() {
        let container = Container {
            key: VERIFICATION_KEY.try_into().unwrap(),
        };
        let serialized = serde_json::to_string(&container).expect("Serialization should not fail");
        let deserialized: Container = serde_json::from_str(&serialized).unwrap();

        assert_eq!(container, deserialized);
    }

    #[test]
    fn can_serialize_a_struct_containing_a_verification_key() {
        let container = Container {
            key: VERIFICATION_KEY.try_into().unwrap(),
        };
        let expected = format!(r#"{{"key":"{VERIFICATION_KEY}"}}"#);

        let serialized = serde_json::to_string(&container).expect("Serialization should not fail");
        assert_eq!(expected, serialized);
    }

    #[test]
    fn can_deserialize_a_struct_containing_a_verification_key() {
        let expected = Container {
            key: VERIFICATION_KEY.try_into().unwrap(),
        };
        let serialized = format!(r#"{{"key":"{VERIFICATION_KEY}"}}"#);

        let deserialized: Container =
            serde_json::from_str(&serialized).expect("Deserialization should not fail");
        assert_eq!(expected, deserialized);
    }
}
