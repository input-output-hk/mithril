use crate::{
    crypto_helper::KESPeriod,
    entities::{
        HexEncodedOpCert, HexEncodedVerificationKey, HexEncodedVerificationKeySignature, PartyId,
        Stake,
    },
};

use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};

/// Signer represents a signing participant in the network
#[derive(Clone, Debug, PartialEq, Eq, Default, Serialize, Deserialize)]
pub struct Signer {
    /// The unique identifier of the signer
    // TODO: Should be removed once the signer certification is fully deployed
    pub party_id: PartyId,

    /// The public key used to authenticate signer signature
    pub verification_key: HexEncodedVerificationKey,

    /// The encoded signer 'Mithril verification key' signature (signed by the Cardano node KES secret key)
    // TODO: Option should be removed once the signer certification is fully deployed
    #[serde(skip_serializing_if = "Option::is_none")]
    pub verification_key_signature: Option<HexEncodedVerificationKeySignature>,

    /// The encoded operational certificate of stake pool operator attached to the signer node
    // TODO: Option should be removed once the signer certification is fully deployed
    #[serde(skip_serializing_if = "Option::is_none")]
    pub operational_certificate: Option<HexEncodedOpCert>,

    /// The kes period used to compute the verification key signature
    // TODO: This kes period shoud not be used as is and should probably be within an allowed range of kes period for the epoch
    #[serde(skip_serializing_if = "Option::is_none")]
    pub kes_period: Option<KESPeriod>,
}

impl Signer {
    /// Signer factory
    pub fn new(
        party_id: PartyId,
        verification_key: HexEncodedVerificationKey,
        verification_key_signature: Option<HexEncodedVerificationKeySignature>,
        operational_certificate: Option<HexEncodedOpCert>,
        kes_period: Option<KESPeriod>,
    ) -> Signer {
        Signer {
            party_id,
            verification_key,
            verification_key_signature,
            operational_certificate,
            kes_period,
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

impl From<SignerWithStake> for Signer {
    fn from(other: SignerWithStake) -> Self {
        Signer::new(
            other.party_id,
            other.verification_key,
            other.verification_key_signature,
            other.operational_certificate,
            other.kes_period,
        )
    }
}

/// Signer represents a signing party in the network (including its stakes)
#[derive(Clone, Debug, PartialEq, Eq, Default, Serialize, Deserialize)]
pub struct SignerWithStake {
    /// The unique identifier of the signer
    // TODO: Should be removed once the signer certification is fully deployed
    pub party_id: PartyId,

    /// The public key used to authenticate signer signature
    pub verification_key: HexEncodedVerificationKey,

    /// The encoded signer 'Mithril verification key' signature (signed by the Cardano node KES secret key)
    // TODO: Option should be removed once the signer certification is fully deployed
    #[serde(skip_serializing_if = "Option::is_none")]
    pub verification_key_signature: Option<HexEncodedVerificationKeySignature>,

    /// The encoded operational certificate of stake pool operator attached to the signer node
    // TODO: Option should be removed once the signer certification is fully deployed
    #[serde(skip_serializing_if = "Option::is_none")]
    pub operational_certificate: Option<HexEncodedOpCert>,

    /// The kes period used to compute the verification key signature
    // TODO: This kes period shoud not be used as is and should probably be within an allowed range of kes period for the epoch
    #[serde(skip_serializing_if = "Option::is_none")]
    pub kes_period: Option<KESPeriod>,

    /// The signer stake
    pub stake: Stake,
}

impl SignerWithStake {
    /// SignerWithStake factory
    pub fn new(
        party_id: PartyId,
        verification_key: HexEncodedVerificationKey,
        verification_key_signature: Option<HexEncodedVerificationKeySignature>,
        operational_certificate: Option<HexEncodedOpCert>,
        kes_period: Option<KESPeriod>,
        stake: Stake,
    ) -> SignerWithStake {
        SignerWithStake {
            party_id,
            verification_key,
            verification_key_signature,
            operational_certificate,
            kes_period,
            stake,
        }
    }

    /// Turn a [Signer] into a [SignerWithStake].
    pub fn from_signer(signer: Signer, stake: Stake) -> Self {
        Self {
            party_id: signer.party_id,
            verification_key: signer.verification_key,
            verification_key_signature: signer.verification_key_signature,
            operational_certificate: signer.operational_certificate,
            kes_period: signer.kes_period,
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
    fn test_stake_signers_from_into() {
        let signer_expected = Signer::new("1".to_string(), "123456".to_string(), None, None, None);
        let signer_with_stake =
            SignerWithStake::new("1".to_string(), "123456".to_string(), None, None, None, 100);

        let signer_into: Signer = signer_with_stake.into();
        assert_eq!(signer_expected, signer_into);
    }

    #[test]
    fn test_signer_compute_hash() {
        let hash_expected = "1a71566d70060d38ed94cc7760b0c38d34dd2729a1a1ea70ef983d2c780a4d77";

        assert_eq!(
            hash_expected,
            Signer::new(
                "1".to_string(),
                "verification-key-123".to_string(),
                None,
                None,
                None,
            )
            .compute_hash()
        );
        assert_ne!(
            hash_expected,
            Signer::new(
                "0".to_string(),
                "verification-key-123".to_string(),
                None,
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
            SignerWithStake::new(
                "1".to_string(),
                "verification-key-123".to_string(),
                None,
                None,
                None,
                10
            )
            .compute_hash()
        );
        assert_ne!(
            hash_expected,
            SignerWithStake::new(
                "0".to_string(),
                "verification-key-123".to_string(),
                None,
                None,
                None,
                10
            )
            .compute_hash()
        );
        assert_ne!(
            hash_expected,
            SignerWithStake::new(
                "1".to_string(),
                "verification-key-456".to_string(),
                None,
                None,
                None,
                10
            )
            .compute_hash()
        );
        assert_ne!(
            hash_expected,
            SignerWithStake::new(
                "1".to_string(),
                "verification-key-123".to_string(),
                None,
                None,
                None,
                20
            )
            .compute_hash()
        );
    }
}
