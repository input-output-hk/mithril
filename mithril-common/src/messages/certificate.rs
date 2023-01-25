use either::Either;
use serde::{Deserialize, Serialize};

use crate::entities::{Beacon, CertificateMetadata, ProtocolMessage, ProtocolMessageThales};

#[cfg(any(test, feature = "test_only"))]
use crate::entities::{ProtocolMessagePartKey, ProtocolParameters, SignerWithStake};

/// Message structure of a certificate
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
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
    pub beacon: Beacon,

    /// Certificate metadata
    /// aka METADATA(p,n)
    pub metadata: CertificateMetadata,

    /// Structured message that is used to created the signed message
    /// aka MSG(p,n) U AVK(n-1)
    #[serde(with = "either::serde_untagged")]
    pub protocol_message: Either<ProtocolMessage, ProtocolMessageThales>,

    /// Message that is signed by the signers
    /// aka H(MSG(p,n) || AVK(n-1))
    pub signed_message: String,

    /// Aggregate verification key
    /// The AVK used to sign during the current epoch
    /// aka AVK(n-2)
    pub aggregate_verification_key: String,

    /// STM multi signature created from a quorum of single signatures from the signers
    /// aka MULTI_SIG(H(MSG(p,n) || AVK(n-1)))
    pub multi_signature: String,

    /// Genesis signature created from the original stake distribution
    /// aka GENESIS_SIG(AVK(-1))
    pub genesis_signature: String,
}

impl CertificateMessage {
    #[cfg(any(test, feature = "test_only"))]
    /// Return a dummy test entity (test-only).
    pub fn dummy() -> Self {
        let mut protocol_message = ProtocolMessage::new();
        protocol_message.set_message_part(
            ProtocolMessagePartKey::SnapshotDigest,
            "snapshot-digest-123".to_string(),
        );
        protocol_message.set_message_part(
            ProtocolMessagePartKey::NextAggregateVerificationKey,
            "next-avk-123".to_string(),
        );
        Self {
            hash: "hash".to_string(),
            previous_hash: "previous_hash".to_string(),
            beacon: Beacon::new("testnet".to_string(), 10, 100),
            metadata: CertificateMetadata::new(
                "0.1.0".to_string(),
                ProtocolParameters::new(1000, 100, 0.123),
                "initiated_at".to_string(),
                "sealed_at".to_string(),
                vec![
                    SignerWithStake::new(
                        "1".to_string(),
                        "verification-key-123".to_string(),
                        None,
                        None,
                        None,
                        10,
                    ),
                    SignerWithStake::new(
                        "2".to_string(),
                        "verification-key-456".to_string(),
                        None,
                        None,
                        None,
                        20,
                    ),
                ],
            ),
            protocol_message: Either::Left(protocol_message.clone()),
            signed_message: "signed_message".to_string(),
            aggregate_verification_key: "aggregate_verification_key".to_string(),
            multi_signature: "multi_signature".to_string(),
            genesis_signature: "genesis_signature".to_string(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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
            beacon: Beacon::new("testnet".to_string(), 10, 100),
            metadata: CertificateMetadata::new(
                "0.1.0".to_string(),
                ProtocolParameters::new(1000, 100, 0.123),
                "initiated_at".to_string(),
                "sealed_at".to_string(),
                vec![
                    SignerWithStake::new(
                        "1".to_string(),
                        "verification-key-123".to_string(),
                        None,
                        None,
                        None,
                        10,
                    ),
                    SignerWithStake::new(
                        "2".to_string(),
                        "verification-key-456".to_string(),
                        None,
                        None,
                        None,
                        20,
                    ),
                ],
            ),
            protocol_message: Either::Left(protocol_message.clone()),
            signed_message: "signed_message".to_string(),
            aggregate_verification_key: "aggregate_verification_key".to_string(),
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
                "initiated_at": "initiated_at",
                "sealed_at": "sealed_at",
                "signers": [
                    {
                        "party_id": "1",
                        "verification_key": "verification-key-123",
                        "stake": 10
                    },
                    {
                        "party_id": "2",
                        "verification_key": "verification-key-456",
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
            "This JSON is expected to be succesfully parsed into a CertificateMessage instance.",
        );

        assert_eq!(golden_message(), message);
    }
}
