use crate::entities::{HexEncodedKey, PartyId, Stake};

use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};

/// Signer represents a signing participant in the network
#[derive(Clone, Debug, PartialEq, Eq, Default, Serialize, Deserialize)]
pub struct Signer {
    /// The unique identifier of the signer
    // TODO: Should be removed once the signer certification is fully deployed
    pub party_id: PartyId,

    /// The public key used to authenticate signer signature
    pub verification_key: HexEncodedKey,

    /// The encoded signer 'Mithril verification key' signature (signed by the Cardano node KES secret key)
    // TODO: Option should be removed once the signer certification is fully deployed
    #[serde(
        rename = "verification_key_signature",
        skip_serializing_if = "Option::is_none"
    )]
    pub verification_key_signature: Option<HexEncodedKey>,

    /// The encoded operational certificate of stake pool operator attached to the signer node
    // TODO: Option should be removed once the signer certification is fully deployed
    #[serde(
        rename = "operational_certificate",
        skip_serializing_if = "Option::is_none"
    )]
    pub operational_certificate: Option<HexEncodedKey>,
}

impl Signer {
    /// Signer factory
    pub fn new(
        party_id: PartyId,
        verification_key: String,
        verification_key_signature: Option<HexEncodedKey>,
        operational_certificate: Option<HexEncodedKey>,
    ) -> Signer {
        Signer {
            party_id,
            verification_key,
            verification_key_signature,
            operational_certificate,
        }
    }

    /// Computes the hash of Signer
    pub fn compute_hash(&self) -> String {
        let mut hasher = Sha256::new();
        hasher.update(self.party_id.as_bytes());
        hasher.update(self.verification_key.as_bytes());
        if let Some(verification_key_signature) = &self.verification_key_signature {
            hasher.update(verification_key_signature.as_bytes());
        }
        if let Some(operational_certificate) = &self.operational_certificate {
            hasher.update(operational_certificate.as_bytes());
        }
        hex::encode(hasher.finalize())
    }
}

/// Signer represents a signing party in the network (including its stakes)
#[derive(Clone, Debug, PartialEq, Eq, Default, Serialize, Deserialize)]
pub struct SignerWithStake {
    /// The unique identifier of the signer
    // TODO: Should be removed once the signer certification is fully deployed
    pub party_id: PartyId,

    /// The public key used to authenticate signer signature
    pub verification_key: HexEncodedKey,

    /// The encoded signer 'Mithril verification key' signature (signed by the Cardano node KES secret key)
    // TODO: Option should be removed once the signer certification is fully deployed
    #[serde(
        rename = "verification_key_signature",
        skip_serializing_if = "Option::is_none"
    )]
    pub verification_key_signature: Option<HexEncodedKey>,

    /// The encoded operational certificate of stake pool operator attached to the signer node
    // TODO: Option should be removed once the signer certification is fully deployed
    #[serde(
        rename = "operational_certificate",
        skip_serializing_if = "Option::is_none"
    )]
    pub operational_certificate: Option<HexEncodedKey>,

    /// The signer stake
    pub stake: Stake,
}

impl SignerWithStake {
    /// SignerWithStake factory
    pub fn new(
        party_id: PartyId,
        verification_key: HexEncodedKey,
        stake: Stake,
    ) -> SignerWithStake {
        SignerWithStake {
            party_id,
            verification_key,
            verification_key_signature: None,
            operational_certificate: None,
            stake,
        }
    }

    /// Computes the hash of SignerWithStake
    pub fn compute_hash(&self) -> String {
        let mut hasher = Sha256::new();
        hasher.update(self.party_id.as_bytes());
        hasher.update(self.verification_key.as_bytes());
        if let Some(verification_key_signature) = &self.verification_key_signature {
            hasher.update(verification_key_signature.as_bytes());
        }
        if let Some(operational_certificate) = &self.operational_certificate {
            hasher.update(operational_certificate.as_bytes());
        }
        hasher.update(self.stake.to_be_bytes());
        hex::encode(hasher.finalize())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_signer_compute_hash() {
        let hash_expected = "1a71566d70060d38ed94cc7760b0c38d34dd2729a1a1ea70ef983d2c780a4d77";

        assert_eq!(
            hash_expected,
            Signer::new(
                "1".to_string(),
                "verification-key-123".to_string(),
                None,
                None
            )
            .compute_hash()
        );
        assert_ne!(
            hash_expected,
            Signer::new(
                "0".to_string(),
                "verification-key-123".to_string(),
                None,
                None
            )
            .compute_hash()
        );
        assert_ne!(
            hash_expected,
            Signer::new(
                "1".to_string(),
                "verification-key-456".to_string(),
                None,
                None
            )
            .compute_hash()
        );
    }

    #[test]
    fn test_signer_with_stake_compute_hash() {
        let hash_expected = "16362ace34bdb40c10d79c08fcfa5b0b14c74b6681635723c89aee52d4134971";

        assert_eq!(
            hash_expected,
            SignerWithStake::new("1".to_string(), "verification-key-123".to_string(), 10)
                .compute_hash()
        );
        assert_ne!(
            hash_expected,
            SignerWithStake::new("0".to_string(), "verification-key-123".to_string(), 10)
                .compute_hash()
        );
        assert_ne!(
            hash_expected,
            SignerWithStake::new("1".to_string(), "verification-key-456".to_string(), 10)
                .compute_hash()
        );
        assert_ne!(
            hash_expected,
            SignerWithStake::new("1".to_string(), "verification-key-123".to_string(), 20)
                .compute_hash()
        );
    }
}
