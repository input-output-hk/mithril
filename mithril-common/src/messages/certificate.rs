use anyhow::{anyhow, Context};
use serde::{Deserialize, Serialize};
use std::fmt::{Debug, Formatter};

#[cfg(any(test, feature = "test_tools"))]
use crate::entities::ProtocolMessagePartKey;
use crate::entities::{
    CardanoDbBeacon, Certificate, CertificateMetadata, CertificateSignature, ProtocolMessage,
    SignedEntityType,
};
use crate::messages::CertificateMetadataMessagePart;

#[cfg(any(test, feature = "test_tools"))]
use crate::test_utils::fake_keys;
use crate::StdError;

/// Message structure of a certificate
#[derive(Clone, PartialEq, Serialize, Deserialize)]
pub struct CertificateMessage {
    /// Hash of the current certificate
    /// Computed from the other fields of the certificate
    /// aka H(Cp,n))
    pub hash: String,

    /// Hash of the previous certificate in the chain
    /// This is either the hash of the first certificate of the epoch in the chain
    /// Or the first certificate of the previous epoch in the chain (if the certificate is the first of its epoch)
    /// aka H(FC(n))
    pub previous_hash: String,

    /// Mithril beacon on the Cardano chain
    /// aka BEACON(p,n)
    pub beacon: CardanoDbBeacon,

    /// Certificate metadata
    /// aka METADATA(p,n)
    pub metadata: CertificateMetadataMessagePart,

    /// Structured message that is used to create the signed message
    /// aka MSG(p,n) U AVK(n-1)
    pub protocol_message: ProtocolMessage,

    /// Message that is signed by the signers
    /// aka H(MSG(p,n) || AVK(n-1))
    pub signed_message: String,

    /// Aggregate verification key
    /// The AVK used to sign during the current epoch
    /// aka AVK(n-2)
    pub aggregate_verification_key: String,

    /// The signed entity type of the message.
    ///
    /// Only available if the message is a MultiSignature.
    pub signed_entity_type: Option<SignedEntityType>,

    /// STM multi signature created from a quorum of single signatures from the signers
    /// aka MULTI_SIG(H(MSG(p,n) || AVK(n-1)))
    pub multi_signature: String,

    /// Genesis signature created from the original stake distribution
    /// aka GENESIS_SIG(AVK(-1))
    pub genesis_signature: String,
}

impl CertificateMessage {
    cfg_test_tools! {
        /// Return a dummy test entity (test-only).
        pub fn dummy() -> Self {
            let mut protocol_message = ProtocolMessage::new();
            protocol_message.set_message_part(
                ProtocolMessagePartKey::SnapshotDigest,
                "snapshot-digest-123".to_string(),
            );
            protocol_message.set_message_part(
                ProtocolMessagePartKey::NextAggregateVerificationKey,
                fake_keys::aggregate_verification_key()[1].to_owned(),
            );
            let beacon = CardanoDbBeacon::new("testnet".to_string(), 10, 100);

            Self {
                hash: "hash".to_string(),
                previous_hash: "previous_hash".to_string(),
                beacon: beacon.clone(),
                metadata: CertificateMetadataMessagePart::dummy(),
                protocol_message: protocol_message.clone(),
                signed_message: "signed_message".to_string(),
                aggregate_verification_key: fake_keys::aggregate_verification_key()[0].to_owned(),
                signed_entity_type: Some(SignedEntityType::MithrilStakeDistribution(beacon.epoch)),
                multi_signature: fake_keys::multi_signature()[0].to_owned(),
                genesis_signature: String::new(),
            }
        }
    }

    /// Check that the certificate signed message match the given protocol message.
    pub fn match_message(&self, message: &ProtocolMessage) -> bool {
        message.compute_hash() == self.signed_message
    }
}

impl Debug for CertificateMessage {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let should_be_exhaustive = f.alternate();
        let mut debug = f.debug_struct("Certificate");
        debug
            .field("hash", &self.hash)
            .field("previous_hash", &self.previous_hash)
            .field("beacon", &format_args!("{:?}", self.beacon))
            .field(
                "signed_entity_type",
                &format_args!("{:?}", self.signed_entity_type),
            )
            .field("metadata", &format_args!("{:?}", self.metadata))
            .field(
                "protocol_message",
                &format_args!("{:?}", self.protocol_message),
            )
            .field("signed_message", &self.signed_message);

        match should_be_exhaustive {
            true => debug
                .field(
                    "aggregate_verification_key",
                    &self.aggregate_verification_key,
                )
                .field("multi_signature", &self.multi_signature)
                .field("genesis_signature", &self.genesis_signature)
                .finish(),
            false => debug.finish_non_exhaustive(),
        }
    }
}

impl TryFrom<CertificateMessage> for Certificate {
    type Error = StdError;

    fn try_from(certificate_message: CertificateMessage) -> Result<Self, Self::Error> {
        let metadata = CertificateMetadata {
            network: certificate_message.beacon.network,
            immutable_file_number: certificate_message.beacon.immutable_file_number,
            protocol_version: certificate_message.metadata.protocol_version,
            protocol_parameters: certificate_message.metadata.protocol_parameters,
            initiated_at: certificate_message.metadata.initiated_at,
            sealed_at: certificate_message.metadata.sealed_at,
            signers: certificate_message.metadata.signers,
        };

        let certificate = Certificate {
            hash: certificate_message.hash,
            previous_hash: certificate_message.previous_hash,
            epoch: certificate_message.beacon.epoch,
            metadata,
            protocol_message: certificate_message.protocol_message,
            signed_message: certificate_message.signed_message,
            aggregate_verification_key: certificate_message
                .aggregate_verification_key
                .try_into()
                .with_context(|| {
                "Can not convert message to certificate: can not decode the aggregate verification key"
            })?,
            signature: if certificate_message.genesis_signature.is_empty() {
                //bbb// Instead of an error should we lie on the signed entity by constructing a
                // "CardanoImmutableFilesFull" signed entity.
                CertificateSignature::MultiSignature(
                    certificate_message.signed_entity_type.ok_or(
                        anyhow!("Can not convert message to certificate: missing signed entity type for a multi-signature Certificate")
                    )?,
                    certificate_message
                        .multi_signature
                        .try_into()
                        .with_context(|| {
                            "Can not convert message to certificate: can not decode the multi-signature"
                        })?,
                )
            } else {
                CertificateSignature::GenesisSignature(
                    certificate_message
                        .genesis_signature
                        .try_into()
                        .with_context(|| {
                            "Can not convert message to certificate: can not decode the genesis signature"
                        })?,
                )
            },
        };

        Ok(certificate)
    }
}

impl TryFrom<Certificate> for CertificateMessage {
    type Error = StdError;

    fn try_from(certificate: Certificate) -> Result<Self, Self::Error> {
        let metadata = CertificateMetadataMessagePart {
            protocol_version: certificate.metadata.protocol_version,
            protocol_parameters: certificate.metadata.protocol_parameters,
            initiated_at: certificate.metadata.initiated_at,
            sealed_at: certificate.metadata.sealed_at,
            signers: certificate.metadata.signers,
        };

        let (signed_entity_type, multi_signature, genesis_signature) = match certificate.signature {
            CertificateSignature::GenesisSignature(signature) => {
                (None, String::new(), signature.to_bytes_hex())
            }
            CertificateSignature::MultiSignature(signed_entity_type, signature) => (
                Some(signed_entity_type),
                signature.to_json_hex().with_context(|| {
                    "Can not convert certificate to message: can not encode the multi-signature"
                })?,
                String::new(),
            ),
        };
        let beacon = CardanoDbBeacon::new(
            certificate.metadata.network,
            *certificate.epoch,
            certificate.metadata.immutable_file_number,
        );

        let message = CertificateMessage {
            hash: certificate.hash,
            previous_hash: certificate.previous_hash,
            beacon,
            metadata,
            protocol_message: certificate.protocol_message,
            signed_message: certificate.signed_message,
            aggregate_verification_key: certificate
                .aggregate_verification_key
                .to_json_hex()
                .with_context(|| {
                    "Can not convert certificate to message: can not encode aggregate verification key"
                })?,
            signed_entity_type,
            multi_signature,
            genesis_signature,
        };

        Ok(message)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::entities::{ProtocolParameters, StakeDistributionParty};
    use chrono::{DateTime, Utc};

    fn golden_message() -> CertificateMessage {
        let mut protocol_message = ProtocolMessage::new();
        protocol_message.set_message_part(
            ProtocolMessagePartKey::SnapshotDigest,
            "snapshot-digest-123".to_string(),
        );
        protocol_message.set_message_part(
            ProtocolMessagePartKey::NextAggregateVerificationKey,
            "next-avk-123".to_string(),
        );
        CertificateMessage {
            hash: "hash".to_string(),
            previous_hash: "previous_hash".to_string(),
            beacon: CardanoDbBeacon::new("testnet".to_string(), 10, 100),
            metadata: CertificateMetadataMessagePart {
                protocol_version: "0.1.0".to_string(),
                protocol_parameters: ProtocolParameters::new(1000, 100, 0.123),
                initiated_at: DateTime::parse_from_rfc3339("2024-02-12T13:11:47Z")
                    .unwrap()
                    .with_timezone(&Utc),
                sealed_at: DateTime::parse_from_rfc3339("2024-02-12T13:12:57Z")
                    .unwrap()
                    .with_timezone(&Utc),
                signers: vec![
                    StakeDistributionParty {
                        party_id: "1".to_string(),
                        stake: 10,
                    },
                    StakeDistributionParty {
                        party_id: "2".to_string(),
                        stake: 20,
                    },
                ],
            },
            protocol_message: protocol_message.clone(),
            signed_message: "signed_message".to_string(),
            aggregate_verification_key: "aggregate_verification_key".to_string(),
            signed_entity_type: None,
            multi_signature: "multi_signature".to_string(),
            genesis_signature: "genesis_signature".to_string(),
        }
    }

    // Test the backward compatibility with possible future upgrades.
    #[test]
    fn test_v1() {
        let json = r#"{
            "hash": "hash",
            "previous_hash": "previous_hash",
            "beacon": {
                "network": "testnet",
                "epoch": 10,
                "immutable_file_number": 100
            },
            "metadata": {
                "version": "0.1.0",
                "parameters": {
                    "k": 1000,
                    "m": 100,
                    "phi_f": 0.123
                },
            "initiated_at": "2024-02-12T13:11:47Z",
            "sealed_at": "2024-02-12T13:12:57Z",
                "signers": [
                    {
                        "party_id": "1",
                        "verification_key": "7b22766b223a5b3134332c3136312c3235352c34382c37382c35372c3230342c3232302c32352c3232312c3136342c3235322c3234382c31342c35362c3132362c3138362c3133352c3232382c3138382c3134352c3138312c35322c3230302c39372c39392c3231332c34362c302c3139392c3139332c38392c3138372c38382c32392c3133352c3137332c3234342c38362c33362c38332c35342c36372c3136342c362c3133372c39342c37322c362c3130352c3132382c3132382c39332c34382c3137362c31312c342c3234362c3133382c34382c3138302c3133332c39302c3134322c3139322c32342c3139332c3131312c3134322c33312c37362c3131312c3131302c3233342c3135332c39302c3230382c3139322c33312c3132342c39352c3130322c34392c3135382c39392c35322c3232302c3136352c39342c3235312c36382c36392c3132312c31362c3232342c3139345d2c22706f70223a5b3136382c35302c3233332c3139332c31352c3133362c36352c37322c3132332c3134382c3132392c3137362c33382c3139382c3230392c34372c32382c3230342c3137362c3134342c35372c3235312c34322c32382c36362c37362c38392c39372c3135382c36332c35342c3139382c3139342c3137362c3133352c3232312c31342c3138352c3139372c3232352c3230322c39382c3234332c37342c3233332c3232352c3134332c3135312c3134372c3137372c3137302c3131372c36362c3136352c36362c36322c33332c3231362c3233322c37352c36382c3131342c3139352c32322c3130302c36352c34342c3139382c342c3136362c3130322c3233332c3235332c3234302c35392c3137352c36302c3131372c3134322c3131342c3134302c3132322c31372c38372c3131302c3138372c312c31372c31302c3139352c3135342c31332c3234392c38362c35342c3232365d7d",
                        "stake": 10
                    },
                    {
                        "party_id": "2",
                        "verification_key": "7b22766b223a5b3134352c35362c3137352c33322c3132322c3138372c3231342c3232362c3235312c3134382c38382c392c312c3130332c3135392c3134362c38302c3136362c3130372c3234332c3235312c3233362c34312c32382c3131312c3132382c3230372c3136342c3133322c3134372c3232382c38332c3234362c3232382c3137302c36382c38392c37382c36302c32382c3132332c3133302c38382c3233342c33382c39372c34322c36352c312c3130302c35332c31382c37382c3133312c382c36312c3132322c3133312c3233382c38342c3233332c3232332c3135342c3131382c3131382c37332c32382c32372c3130312c37382c38302c3233332c3132332c3230362c3232302c3137342c3133342c3230352c37312c3131302c3131322c3138302c39372c39382c302c3131332c36392c3134352c3233312c3136382c34332c3137332c3137322c35362c3130342c3230385d2c22706f70223a5b3133372c3231342c37352c37352c3134342c3136312c3133372c37392c39342c3134302c3138312c34372c33312c38312c3231332c33312c3137312c3231362c32342c3137342c37382c3234382c3133302c37352c3235352c31312c3134352c3132342c36312c38302c3139302c32372c3231362c3130352c3130362c3234382c39312c3134332c3230342c3130322c3230332c3136322c37362c3130372c31352c35322c36312c38322c3134362c3133302c3132342c37342c382c33342c3136342c3138372c3230332c38322c36342c3130382c3139312c3138352c3138382c37372c3132322c352c3234362c3235352c3130322c3131392c3234372c3139392c3131372c36372c3234312c3134332c32392c3136382c36372c39342c3135312c37382c3132392c3133312c33302c3130312c3137332c31302c36392c36382c3137352c39382c33372c3233392c3139342c32395d7d",
                        "stake": 20
                    }
                ]
            },
            "protocol_message": {
                "message_parts": {
                    "snapshot_digest": "snapshot-digest-123",
                    "next_aggregate_verification_key": "next-avk-123"
                }
            },
            "signed_message": "signed_message",
            "aggregate_verification_key": "aggregate_verification_key",
            "multi_signature": "multi_signature",
            "genesis_signature": "genesis_signature"
        }"#;
        let message: CertificateMessage = serde_json::from_str(json).expect(
            "This JSON is expected to be successfully parsed into a CertificateMessage instance.",
        );

        assert_eq!(golden_message(), message);
    }
}
