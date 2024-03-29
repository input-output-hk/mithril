use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::fmt::{Debug, Formatter};

use crate::entities::{
    CardanoDbBeacon, Epoch, ProtocolMessage, ProtocolMessagePartKey, ProtocolParameters,
    ProtocolVersion, SignedEntityType,
};

/// Message structure of a certificate list
pub type CertificateListMessage = Vec<CertificateListItemMessage>;

/// CertificateListItemMessage represents the metadata associated to a CertificateListItemMessage
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct CertificateListItemMessageMetadata {
    /// Cardano network
    /// part of METADATA(p,n)
    pub network: String,

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
    pub initiated_at: DateTime<Utc>,

    /// Date and time when the certificate was sealed
    /// Represents the time at which the quorum of single signatures was reached so that they were aggregated into a multi signature
    /// part of METADATA(p,n)
    pub sealed_at: DateTime<Utc>,

    /// The number of signers that contributed to the certificate
    /// part of METADATA(p,n)
    pub total_signers: usize,
}

/// Message structure of a certificate list item
#[derive(Clone, PartialEq, Serialize, Deserialize)]
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

    /// Epoch of the Cardano chain
    pub epoch: Epoch,

    /// The signed entity type of the message.
    /// aka BEACON(p,n)
    pub signed_entity_type: SignedEntityType,

    /// Mithril beacon on the Cardano chain
    #[deprecated(since = "0.3.25", note = "use epoch and/or signed_entity_type instead")]
    pub beacon: CardanoDbBeacon,

    /// Certificate metadata
    /// aka METADATA(p,n)
    pub metadata: CertificateListItemMessageMetadata,

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
        let epoch = Epoch(10);

        #[allow(deprecated)]
        Self {
            hash: "hash".to_string(),
            previous_hash: "previous_hash".to_string(),
            epoch,
            signed_entity_type: SignedEntityType::MithrilStakeDistribution(epoch),
            beacon: CardanoDbBeacon::new("testnet", *epoch, 100),
            metadata: CertificateListItemMessageMetadata {
                network: "testnet".to_string(),
                protocol_version: "0.1.0".to_string(),
                protocol_parameters: ProtocolParameters::new(1000, 100, 0.123),
                initiated_at: DateTime::parse_from_rfc3339("2024-02-12T13:11:47Z")
                    .unwrap()
                    .with_timezone(&Utc),
                sealed_at: DateTime::parse_from_rfc3339("2024-02-12T13:12:57Z")
                    .unwrap()
                    .with_timezone(&Utc),
                total_signers: 2,
            },
            protocol_message: protocol_message.clone(),
            signed_message: "signed_message".to_string(),
            aggregate_verification_key: "aggregate_verification_key".to_string(),
        }
    }
}

impl Debug for CertificateListItemMessage {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let should_be_exhaustive = f.alternate();
        let mut debug = f.debug_struct("Certificate");
        debug
            .field("hash", &self.hash)
            .field("previous_hash", &self.previous_hash)
            .field("epoch", &format_args!("{:?}", self.epoch))
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
                .finish(),
            false => debug.finish_non_exhaustive(),
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
        let epoch = Epoch(10);

        vec![
            #[allow(deprecated)]
            CertificateListItemMessage {
                hash: "hash".to_string(),
                previous_hash: "previous_hash".to_string(),
                epoch,
                signed_entity_type: SignedEntityType::MithrilStakeDistribution(epoch),
                beacon: CardanoDbBeacon::new("testnet", *epoch, 100),
                metadata: CertificateListItemMessageMetadata {
                    network: "testnet".to_string(),
                    protocol_version: "0.1.0".to_string(),
                    protocol_parameters: ProtocolParameters::new(1000, 100, 0.123),
                    initiated_at: DateTime::parse_from_rfc3339("2024-02-12T13:11:47Z")
                        .unwrap()
                        .with_timezone(&Utc),
                    sealed_at: DateTime::parse_from_rfc3339("2024-02-12T13:12:57Z")
                        .unwrap()
                        .with_timezone(&Utc),
                    total_signers: 2,
                },
                protocol_message: protocol_message.clone(),
                signed_message: "signed_message".to_string(),
                aggregate_verification_key: "aggregate_verification_key".to_string(),
            },
        ]
    }

    // Test the retro compatibility with possible future upgrades.
    #[test]
    fn test_v1() {
        let json = r#"[{
            "hash": "hash",
            "previous_hash": "previous_hash",
            "epoch": 10,
            "signed_entity_type": { "MithrilStakeDistribution": 10 },
            "beacon": {
                "network": "testnet",
                "epoch": 10,
                "immutable_file_number": 100
            },
            "metadata": {
                "network": "testnet",
                "version": "0.1.0",
                "parameters": {
                    "k": 1000,
                    "m": 100,
                    "phi_f": 0.123
                },
                "initiated_at": "2024-02-12T13:11:47Z",
                "sealed_at": "2024-02-12T13:12:57Z",
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
