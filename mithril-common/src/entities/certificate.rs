use crate::crypto_helper::{
    ProtocolAggregateVerificationKey, ProtocolGenesisSignature, ProtocolMultiSignature,
};
use crate::entities::{CertificateMetadata, Epoch, ProtocolMessage, SignedEntityType};
use std::fmt::{Debug, Formatter};

use sha2::{Digest, Sha256};

/// The signature of a [Certificate]
#[derive(Clone, Debug)]
pub enum CertificateSignature {
    /// Genesis signature created from the original stake distribution
    /// aka GENESIS_SIG(AVK(-1))
    GenesisSignature(ProtocolGenesisSignature),

    /// STM multi signature created from a quorum of single signatures from the signers
    /// aka (BEACON(p,n), MULTI_SIG(H(MSG(p,n) || AVK(n-1))))
    MultiSignature(SignedEntityType, ProtocolMultiSignature),
}

/// Certificate represents a Mithril certificate embedding a Mithril STM multisignature
#[derive(Clone)]
pub struct Certificate {
    /// Hash of the current certificate
    /// Computed from the other fields of the certificate
    /// aka H(Cp,n))
    pub hash: String,

    /// Hash of the previous certificate in the chain
    /// This is either the hash of the first certificate of the epoch in the chain
    /// Or the first certificate of the previous epoch in the chain (if the certificate is the first of its epoch)
    /// aka H(FC(n))
    pub previous_hash: String,

    /// Cardano chain epoch number
    pub epoch: Epoch,

    /// Certificate metadata
    /// aka METADATA(p,n)
    pub metadata: CertificateMetadata,

    /// Structured message that is used to create the signed message
    /// aka MSG(p,n) U AVK(n-1)
    pub protocol_message: ProtocolMessage,

    /// Message that is signed by the signers
    /// aka H(MSG(p,n) || AVK(n-1))
    pub signed_message: String,

    /// Aggregate verification key
    /// The AVK used to sign during the current epoch
    /// aka AVK(n-2)
    pub aggregate_verification_key: ProtocolAggregateVerificationKey,

    /// Certificate signature
    pub signature: CertificateSignature,
}

impl Certificate {
    /// Certificate factory
    pub fn new<T: Into<String>>(
        previous_hash: T,
        epoch: Epoch,
        metadata: CertificateMetadata,
        protocol_message: ProtocolMessage,
        aggregate_verification_key: ProtocolAggregateVerificationKey,
        signature: CertificateSignature,
    ) -> Certificate {
        let signed_message = protocol_message.compute_hash();
        let mut certificate = Certificate {
            hash: "".to_string(),
            previous_hash: previous_hash.into(),
            epoch,
            metadata,
            protocol_message,
            signed_message,
            aggregate_verification_key,
            signature,
        };
        certificate.hash = certificate.compute_hash();
        certificate
    }

    /// Computes the hash of a Certificate
    pub fn compute_hash(&self) -> String {
        let mut hasher = Sha256::new();
        hasher.update(self.previous_hash.as_bytes());
        hasher.update(self.epoch.to_be_bytes());
        hasher.update(self.metadata.compute_hash().as_bytes());
        hasher.update(self.protocol_message.compute_hash().as_bytes());
        hasher.update(self.signed_message.as_bytes());
        hasher.update(
            self.aggregate_verification_key
                .to_json_hex()
                .unwrap()
                .as_bytes(),
        );
        match &self.signature {
            CertificateSignature::GenesisSignature(signature) => {
                hasher.update(signature.to_bytes_hex());
            }
            CertificateSignature::MultiSignature(signed_entity_type, signature) => {
                signed_entity_type.feed_hash(&mut hasher);
                hasher.update(signature.to_json_hex().unwrap());
            }
        };
        hex::encode(hasher.finalize())
    }

    /// Tell if the certificate is a genesis certificate
    pub fn is_genesis(&self) -> bool {
        matches!(self.signature, CertificateSignature::GenesisSignature(_))
    }

    /// Return true if the certificate is chaining into itself (meaning that its hash and previous
    /// hash are equal).
    pub fn is_chaining_to_itself(&self) -> bool {
        self.hash == self.previous_hash
    }

    /// Check that the certificate signed message match the given protocol message.
    pub fn match_message(&self, message: &ProtocolMessage) -> bool {
        message.compute_hash() == self.signed_message
    }

    /// Get the certificate signed entity type.
    pub fn signed_entity_type(&self) -> SignedEntityType {
        match &self.signature {
            CertificateSignature::GenesisSignature(_) => SignedEntityType::genesis(self.epoch),
            CertificateSignature::MultiSignature(entity_type, _) => entity_type.clone(),
        }
    }
}

impl PartialEq for Certificate {
    fn eq(&self, other: &Self) -> bool {
        self.epoch.eq(&other.epoch) && self.hash.eq(&other.hash)
    }
}

impl Debug for Certificate {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let should_be_exhaustive = f.alternate();
        let mut debug = f.debug_struct("Certificate");
        debug
            .field("hash", &self.hash)
            .field("previous_hash", &self.previous_hash)
            .field("epoch", &format_args!("{:?}", self.epoch))
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
                    &format_args!("{:?}", self.aggregate_verification_key),
                )
                .field("signature", &format_args!("{:?}", self.signature))
                .finish(),
            false => debug.finish_non_exhaustive(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::entities::SignedEntityType::CardanoStakeDistribution;
    use crate::{
        entities::{
            certificate_metadata::StakeDistributionParty, ProtocolMessagePartKey,
            ProtocolParameters,
        },
        test_utils::fake_keys,
    };
    use chrono::{DateTime, Duration, Utc};

    fn get_parties() -> Vec<StakeDistributionParty> {
        vec![
            StakeDistributionParty {
                party_id: "1".to_string(),
                stake: 10,
            },
            StakeDistributionParty {
                party_id: "2".to_string(),
                stake: 20,
            },
        ]
    }

    fn get_protocol_message() -> ProtocolMessage {
        let mut protocol_message = ProtocolMessage::new();
        protocol_message.set_message_part(
            ProtocolMessagePartKey::SnapshotDigest,
            "snapshot-digest-123".to_string(),
        );
        protocol_message.set_message_part(
            ProtocolMessagePartKey::NextAggregateVerificationKey,
            fake_keys::aggregate_verification_key()[1].to_owned(),
        );

        protocol_message
    }

    #[test]
    fn test_certificate_compute_hash() {
        const HASH_EXPECTED: &str =
            "5e341ceeb91cc9957fca96d586e0ff2c66a8d6ec6b90bf84929c060c717094da";

        let initiated_at = DateTime::parse_from_rfc3339("2024-02-12T13:11:47.0123043Z")
            .unwrap()
            .with_timezone(&Utc);
        let sealed_at = initiated_at + Duration::try_seconds(100).unwrap();
        let signed_entity_type = SignedEntityType::MithrilStakeDistribution(Epoch(10));

        let certificate = Certificate::new(
            "previous_hash".to_string(),
            Epoch(10),
            CertificateMetadata::new(
                "testnet",
                "0.1.0",
                ProtocolParameters::new(1000, 100, 0.123),
                initiated_at,
                sealed_at,
                get_parties(),
            ),
            get_protocol_message(),
            fake_keys::aggregate_verification_key()[0]
                .try_into()
                .unwrap(),
            CertificateSignature::MultiSignature(
                signed_entity_type.clone(),
                fake_keys::multi_signature()[0].try_into().unwrap(),
            ),
        );

        assert_eq!(HASH_EXPECTED, certificate.compute_hash());

        assert_ne!(
            HASH_EXPECTED,
            Certificate {
                previous_hash: "previous_hash-modified".to_string(),
                ..certificate.clone()
            }
            .compute_hash(),
        );

        assert_ne!(
            HASH_EXPECTED,
            Certificate {
                epoch: certificate.epoch + 10,
                ..certificate.clone()
            }
            .compute_hash(),
        );

        assert_ne!(
            HASH_EXPECTED,
            Certificate {
                metadata: CertificateMetadata {
                    protocol_version: "0.1.0-modified".to_string(),
                    ..certificate.metadata.clone()
                },
                ..certificate.clone()
            }
            .compute_hash(),
        );

        assert_ne!(
            HASH_EXPECTED,
            Certificate {
                protocol_message: {
                    let mut protocol_message_modified = certificate.protocol_message.clone();
                    protocol_message_modified.set_message_part(
                        ProtocolMessagePartKey::NextAggregateVerificationKey,
                        fake_keys::aggregate_verification_key()[2].into(),
                    );

                    protocol_message_modified
                },
                ..certificate.clone()
            }
            .compute_hash(),
        );

        assert_ne!(
            HASH_EXPECTED,
            Certificate {
                aggregate_verification_key: fake_keys::aggregate_verification_key()[2]
                    .try_into()
                    .unwrap(),
                ..certificate.clone()
            }
            .compute_hash(),
        );

        assert_ne!(
            HASH_EXPECTED,
            Certificate {
                signature: CertificateSignature::MultiSignature(
                    CardanoStakeDistribution(Epoch(100)),
                    fake_keys::multi_signature()[0].try_into().unwrap()
                ),
                ..certificate.clone()
            }
            .compute_hash(),
        );

        assert_ne!(
            HASH_EXPECTED,
            Certificate {
                signature: CertificateSignature::MultiSignature(
                    signed_entity_type,
                    fake_keys::multi_signature()[1].try_into().unwrap()
                ),
                ..certificate.clone()
            }
            .compute_hash(),
        );
    }

    #[test]
    fn test_genesis_certificate_compute_hash() {
        const HASH_EXPECTED: &str =
            "6160fca853402c0ea89a0a9ceb5d97462ffd81c558c53feef01dcc0827f5bd19";

        let initiated_at = DateTime::parse_from_rfc3339("2024-02-12T13:11:47.0123043Z")
            .unwrap()
            .with_timezone(&Utc);
        let sealed_at = initiated_at + Duration::try_seconds(100).unwrap();

        let genesis_certificate = Certificate::new(
            "previous_hash",
            Epoch(10),
            CertificateMetadata::new(
                "testnet",
                "0.1.0".to_string(),
                ProtocolParameters::new(1000, 100, 0.123),
                initiated_at,
                sealed_at,
                get_parties(),
            ),
            get_protocol_message(),
            fake_keys::aggregate_verification_key()[1]
                .try_into()
                .unwrap(),
            CertificateSignature::GenesisSignature(
                fake_keys::genesis_signature()[0].try_into().unwrap(),
            ),
        );

        assert_eq!(HASH_EXPECTED, genesis_certificate.compute_hash());

        assert_ne!(
            HASH_EXPECTED,
            Certificate {
                signature: CertificateSignature::GenesisSignature(
                    fake_keys::genesis_signature()[1].try_into().unwrap()
                ),
                ..genesis_certificate.clone()
            }
            .compute_hash(),
        );
    }
}
