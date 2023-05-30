use serde::{Deserialize, Serialize};

use crate::entities::Beacon;

use crate::entities::ProtocolMessage;
use crate::entities::ProtocolMessagePartKey;
use crate::entities::ProtocolParameters;
use crate::entities::ProtocolVersion;

/// Message structure of a certificate list
pub type CertificateListMessage = Vec<CertificateListItemMessage>;

/// CertificateListItemMessage represents the metadata associated to a CertificateListItemMessage
#[derive(Clone, Debug, PartialEq, Default, Serialize, Deserialize)]
pub struct CertificateListItemMessageMetadata {
    /// Protocol Version (semver)
    /// Useful to achieve backward compatibility of the certificates (including of the multi signature)
    /// part of METADATA(p,n)
    #[serde(rename = "version")]
    pub protocol_version: ProtocolVersion,

    /// Protocol parameters
    /// part of METADATA(p,n)
    #[serde(rename = "parameters")]
    pub protocol_parameters: ProtocolParameters,

    /// Date and time when the certificate was initiated
    /// Represents the time at which the single signatures registration is opened
    /// part of METADATA(p,n)
    pub initiated_at: String,

    /// Date and time when the certificate was sealed
    /// Represents the time at which the quorum of single signatures was reached so that they were aggregated into a multi signature
    /// part of METADATA(p,n)
    pub sealed_at: String,

    /// The number of signers that contributed to the certificate
    /// part of METADATA(p,n)
    pub total_signers: usize,
}

/// Message structure of a certificate list item
#[derive(Clone, Debug, PartialEq, Default, Serialize, Deserialize)]
pub struct CertificateListItemMessage {
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
    pub metadata: CertificateListItemMessageMetadata,

    /// Structured message that is used to created the signed message
    /// aka MSG(p,n) U AVK(n-1)
    pub protocol_message: ProtocolMessage,

    /// Message that is signed by the signers
    /// aka H(MSG(p,n) || AVK(n-1))
    pub signed_message: String,

    /// Aggregate verification key
    /// The AVK used to sign during the current epoch
    /// aka AVK(n-2)
    pub aggregate_verification_key: String,
}

impl CertificateListItemMessage {
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
            metadata: CertificateListItemMessageMetadata {
                protocol_version: "0.1.0".to_string(),
                protocol_parameters: ProtocolParameters::new(1000, 100, 0.123),
                initiated_at: "initiated_at".to_string(),
                sealed_at: "sealed_at".to_string(),
                total_signers: 2,
            },
            protocol_message: protocol_message.clone(),
            signed_message: "signed_message".to_string(),
            aggregate_verification_key: "aggregate_verification_key".to_string(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn golden_message() -> CertificateListMessage {
        let mut protocol_message = ProtocolMessage::new();
        protocol_message.set_message_part(
            ProtocolMessagePartKey::SnapshotDigest,
            "snapshot-digest-123".to_string(),
        );
        protocol_message.set_message_part(
            ProtocolMessagePartKey::NextAggregateVerificationKey,
            "next-avk-123".to_string(),
        );
        vec![CertificateListItemMessage {
            hash: "hash".to_string(),
            previous_hash: "previous_hash".to_string(),
            beacon: Beacon::new("testnet".to_string(), 10, 100),
            metadata: CertificateListItemMessageMetadata {
                protocol_version: "0.1.0".to_string(),
                protocol_parameters: ProtocolParameters::new(1000, 100, 0.123),
                initiated_at: "initiated_at".to_string(),
                sealed_at: "sealed_at".to_string(),
                total_signers: 2,
            },
            protocol_message: protocol_message.clone(),
            signed_message: "signed_message".to_string(),
            aggregate_verification_key: "aggregate_verification_key".to_string(),
        }]
    }

    // Test the retro compatibility with possible future upgrades.
    #[test]
    fn test_v1() {
        let json = r#"[{
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
    "total_signers": 2
},
"protocol_message": {
    "message_parts": {
        "snapshot_digest": "snapshot-digest-123",
        "next_aggregate_verification_key": "next-avk-123"
    }
},
"signed_message": "signed_message",
"aggregate_verification_key": "aggregate_verification_key"
}]"#;

        let message: CertificateListMessage = serde_json::from_str(json).expect(
            "This JSON is expected to be succesfully parsed into a CertificateListMessage instance.",
        );

        assert_eq!(golden_message(), message);
    }
}
