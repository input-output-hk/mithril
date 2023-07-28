use serde::{Deserialize, Serialize};

use crate::{
    crypto_helper::KESPeriod,
    entities::{
        Beacon, HexEncodedOpCert, HexEncodedVerificationKey, HexEncodedVerificationKeySignature,
        PartyId, ProtocolParameters, SignedEntityType,
    },
    test_utils::fake_keys,
};

/// Structure to transport [crate::entities::CertificatePending] data.
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct CertificatePendingMessage {
    /// Current Beacon
    pub beacon: Beacon,

    /// Signed entity type
    #[serde(rename = "entity_type")]
    pub signed_entity_type: SignedEntityType,

    /// Current Protocol parameters
    #[serde(rename = "protocol")]
    pub protocol_parameters: ProtocolParameters,

    /// Next Protocol parameters
    #[serde(rename = "next_protocol")]
    pub next_protocol_parameters: ProtocolParameters,

    /// Current Signers
    pub signers: Vec<SignerMessage>,

    /// Signers that will be able to sign on the next epoch
    pub next_signers: Vec<SignerMessage>,
}

impl CertificatePendingMessage {
    /// Provide a dummy instance for test.
    pub fn dummy() -> Self {
        Self {
            beacon: Beacon::default(),
            signed_entity_type: SignedEntityType::dummy(),
            protocol_parameters: ProtocolParameters {
                k: 5,
                m: 100,
                phi_f: 0.65,
            },
            next_protocol_parameters: ProtocolParameters {
                k: 50,
                m: 1000,
                phi_f: 0.65,
            },
            signers: [SignerMessage::dummy()].to_vec(),
            next_signers: [SignerMessage::dummy()].to_vec(),
        }
    }
}

// todo: move to message_parts
/// Signer Message
#[derive(Clone, Debug, PartialEq, Eq, Default, Serialize, Deserialize)]
pub struct SignerMessage {
    /// The unique identifier of the signer
    // TODO: Should be removed once the signer certification is fully deployed
    pub party_id: PartyId,

    /// The public key used to authenticate signer signature
    pub verification_key: HexEncodedVerificationKey,

    /// The encoded signer 'Mithril verification key' signature (signed by the
    /// Cardano node KES secret key).
    // TODO: Option should be removed once the signer certification is fully
    //       deployed.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub verification_key_signature: Option<HexEncodedVerificationKeySignature>,

    /// The encoded operational certificate of stake pool operator attached to
    /// the signer node.
    // TODO: Option should be removed once the signer certification is fully
    //       deployed.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub operational_certificate: Option<HexEncodedOpCert>,

    /// The KES period used to compute the verification key signature
    // TODO: This KES period should not be used as is and should probably be
    //       within an allowed range of KES periods for the epoch.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub kes_period: Option<KESPeriod>,
}

impl SignerMessage {
    /// Return a dummy test entity (test-only).
    pub fn dummy() -> Self {
        Self {
            party_id: "pool1m8crhnqj5k2kyszf5j2scshupystyxc887zdfrpzh6ty6eun4fx".to_string(),
            verification_key: fake_keys::signer_verification_key()[0].to_string(),
            verification_key_signature: Some(fake_keys::signer_verification_key_signature()[0].to_string()),
            operational_certificate: Some("5b5b5b3230332c3130392c34302c32382c3235312c39342c35322c32342c3231322c3131362c3134392c38302c3138332c3136322c312c36322c352c3133332c35372c3230342c31352c3137322c3134372c38362c3132352c35392c31322c3235332c3130312c3138342c32332c31355d2c322c3132382c5b3133382c3131302c3139322c35302c38362c332c3136382c33342c3137322c31392c39312c3133392c3139302c3134302c31382c3137372c33312c34362c3132322c3130362c3233342c3137372c3130382c3232352c3230372c342c302c35392c3233372c3133352c3130342c39382c3133332c3133312c32392c3231322c3137312c3139342c3234342c3139312c3137392c3131392c34322c37352c3135302c36312c3232362c3132312c35342c3232332c3139332c3133382c3139302c32372c3138322c3135322c35362c32312c3136302c3230372c33352c3233372c3130322c31325d5d2c5b3230372c31322c3136382c3139302c34362c3131362c3139362c3133332c3139362c3233312c3132342c3235302c3134372c33372c3137352c3231312c3234372c3139382c3134302c3133392c3234362c3130342c3132342c3232372c34392c352c3235332c3232382c3130372c39332c3133362c3134345d5d".to_string()),
            kes_period: Some(6),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::entities::Epoch;

    use super::*;

    fn golden_message() -> CertificatePendingMessage {
        let beacon = Beacon {
            network: "preview".to_string(),
            epoch: Epoch(86),
            immutable_file_number: 1728,
        };
        CertificatePendingMessage {
            beacon: beacon.clone(),
            signed_entity_type: SignedEntityType::CardanoImmutableFilesFull(beacon),
            protocol_parameters: ProtocolParameters {
                k: 5,
                m: 100,
                phi_f: 0.65,
            },
            next_protocol_parameters: ProtocolParameters {
                k: 50,
                m: 1000,
                phi_f: 0.65,
            },
            signers: vec![
                SignerMessage {
                    party_id: "123".to_string(),
                    verification_key: "7b22766b223a5b3134332c3136312c3235352c34382c37382c35372c3230342c3232302c32352c3232312c3136342c3235322c3234382c31342c35362c3132362c3138362c3133352c3232382c3138382c3134352c3138312c35322c3230302c39372c39392c3231332c34362c302c3139392c3139332c38392c3138372c38382c32392c3133352c3137332c3234342c38362c33362c38332c35342c36372c3136342c362c3133372c39342c37322c362c3130352c3132382c3132382c39332c34382c3137362c31312c342c3234362c3133382c34382c3138302c3133332c39302c3134322c3139322c32342c3139332c3131312c3134322c33312c37362c3131312c3131302c3233342c3135332c39302c3230382c3139322c33312c3132342c39352c3130322c34392c3135382c39392c35322c3232302c3136352c39342c3235312c36382c36392c3132312c31362c3232342c3139345d2c22706f70223a5b3136382c35302c3233332c3139332c31352c3133362c36352c37322c3132332c3134382c3132392c3137362c33382c3139382c3230392c34372c32382c3230342c3137362c3134342c35372c3235312c34322c32382c36362c37362c38392c39372c3135382c36332c35342c3139382c3139342c3137362c3133352c3232312c31342c3138352c3139372c3232352c3230322c39382c3234332c37342c3233332c3232352c3134332c3135312c3134372c3137372c3137302c3131372c36362c3136352c36362c36322c33332c3231362c3233322c37352c36382c3131342c3139352c32322c3130302c36352c34342c3139382c342c3136362c3130322c3233332c3235332c3234302c35392c3137352c36302c3131372c3134322c3131342c3134302c3132322c31372c38372c3131302c3138372c312c31372c31302c3139352c3135342c31332c3234392c38362c35342c3232365d7d".to_string(),
                    verification_key_signature: None,
                    operational_certificate: None,
                    kes_period: None
                }
            ],
            next_signers: vec![
                SignerMessage {
                    party_id: "123".to_string(),
                    verification_key: "7b22766b223a5b3134332c3136312c3235352c34382c37382c35372c3230342c3232302c32352c3232312c3136342c3235322c3234382c31342c35362c3132362c3138362c3133352c3232382c3138382c3134352c3138312c35322c3230302c39372c39392c3231332c34362c302c3139392c3139332c38392c3138372c38382c32392c3133352c3137332c3234342c38362c33362c38332c35342c36372c3136342c362c3133372c39342c37322c362c3130352c3132382c3132382c39332c34382c3137362c31312c342c3234362c3133382c34382c3138302c3133332c39302c3134322c3139322c32342c3139332c3131312c3134322c33312c37362c3131312c3131302c3233342c3135332c39302c3230382c3139322c33312c3132342c39352c3130322c34392c3135382c39392c35322c3232302c3136352c39342c3235312c36382c36392c3132312c31362c3232342c3139345d2c22706f70223a5b3136382c35302c3233332c3139332c31352c3133362c36352c37322c3132332c3134382c3132392c3137362c33382c3139382c3230392c34372c32382c3230342c3137362c3134342c35372c3235312c34322c32382c36362c37362c38392c39372c3135382c36332c35342c3139382c3139342c3137362c3133352c3232312c31342c3138352c3139372c3232352c3230322c39382c3234332c37342c3233332c3232352c3134332c3135312c3134372c3137372c3137302c3131372c36362c3136352c36362c36322c33332c3231362c3233322c37352c36382c3131342c3139352c32322c3130302c36352c34342c3139382c342c3136362c3130322c3233332c3235332c3234302c35392c3137352c36302c3131372c3134322c3131342c3134302c3132322c31372c38372c3131302c3138372c312c31372c31302c3139352c3135342c31332c3234392c38362c35342c3232365d7d".to_string(),
                    verification_key_signature: None,
                    operational_certificate: None,
                    kes_period: None
                }
            ],
        }
    }

    #[test]
    fn test_v1() {
        let json = r#"{
            "beacon": {
                "network": "preview",
                "epoch": 86,
                "immutable_file_number": 1728
            },
            "entity_type": {
                "CardanoImmutableFilesFull": {
                    "network": "preview",
                    "epoch": 86,
                    "immutable_file_number": 1728
                }
            },
            "protocol": {
                "k": 5,
                "m": 100,
                "phi_f": 0.65
            },
            "next_protocol": {
                "k": 50,
                "m": 1000,
                "phi_f": 0.65
            },
            "signers": [
                {
                    "party_id": "123",
                    "verification_key": "7b22766b223a5b3134332c3136312c3235352c34382c37382c35372c3230342c3232302c32352c3232312c3136342c3235322c3234382c31342c35362c3132362c3138362c3133352c3232382c3138382c3134352c3138312c35322c3230302c39372c39392c3231332c34362c302c3139392c3139332c38392c3138372c38382c32392c3133352c3137332c3234342c38362c33362c38332c35342c36372c3136342c362c3133372c39342c37322c362c3130352c3132382c3132382c39332c34382c3137362c31312c342c3234362c3133382c34382c3138302c3133332c39302c3134322c3139322c32342c3139332c3131312c3134322c33312c37362c3131312c3131302c3233342c3135332c39302c3230382c3139322c33312c3132342c39352c3130322c34392c3135382c39392c35322c3232302c3136352c39342c3235312c36382c36392c3132312c31362c3232342c3139345d2c22706f70223a5b3136382c35302c3233332c3139332c31352c3133362c36352c37322c3132332c3134382c3132392c3137362c33382c3139382c3230392c34372c32382c3230342c3137362c3134342c35372c3235312c34322c32382c36362c37362c38392c39372c3135382c36332c35342c3139382c3139342c3137362c3133352c3232312c31342c3138352c3139372c3232352c3230322c39382c3234332c37342c3233332c3232352c3134332c3135312c3134372c3137372c3137302c3131372c36362c3136352c36362c36322c33332c3231362c3233322c37352c36382c3131342c3139352c32322c3130302c36352c34342c3139382c342c3136362c3130322c3233332c3235332c3234302c35392c3137352c36302c3131372c3134322c3131342c3134302c3132322c31372c38372c3131302c3138372c312c31372c31302c3139352c3135342c31332c3234392c38362c35342c3232365d7d"
                }
            ],
            "next_signers": [
                {
                    "party_id": "123",
                    "verification_key": "7b22766b223a5b3134332c3136312c3235352c34382c37382c35372c3230342c3232302c32352c3232312c3136342c3235322c3234382c31342c35362c3132362c3138362c3133352c3232382c3138382c3134352c3138312c35322c3230302c39372c39392c3231332c34362c302c3139392c3139332c38392c3138372c38382c32392c3133352c3137332c3234342c38362c33362c38332c35342c36372c3136342c362c3133372c39342c37322c362c3130352c3132382c3132382c39332c34382c3137362c31312c342c3234362c3133382c34382c3138302c3133332c39302c3134322c3139322c32342c3139332c3131312c3134322c33312c37362c3131312c3131302c3233342c3135332c39302c3230382c3139322c33312c3132342c39352c3130322c34392c3135382c39392c35322c3232302c3136352c39342c3235312c36382c36392c3132312c31362c3232342c3139345d2c22706f70223a5b3136382c35302c3233332c3139332c31352c3133362c36352c37322c3132332c3134382c3132392c3137362c33382c3139382c3230392c34372c32382c3230342c3137362c3134342c35372c3235312c34322c32382c36362c37362c38392c39372c3135382c36332c35342c3139382c3139342c3137362c3133352c3232312c31342c3138352c3139372c3232352c3230322c39382c3234332c37342c3233332c3232352c3134332c3135312c3134372c3137372c3137302c3131372c36362c3136352c36362c36322c33332c3231362c3233322c37352c36382c3131342c3139352c32322c3130302c36352c34342c3139382c342c3136362c3130322c3233332c3235332c3234302c35392c3137352c36302c3131372c3134322c3131342c3134302c3132322c31372c38372c3131302c3138372c312c31372c31302c3139352c3135342c31332c3234392c38362c35342c3232365d7d"
                }
            ]
        }"#;
        let message: CertificatePendingMessage = serde_json::from_str(json).unwrap();

        assert_eq!(golden_message(), message);
    }
}
