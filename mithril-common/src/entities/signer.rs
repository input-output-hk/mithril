#[cfg(feature = "future_snark")]
use crate::crypto_helper::{
    ProtocolSignerVerificationKeyForSnark, ProtocolSignerVerificationKeySignatureForSnark,
};
use crate::{
    crypto_helper::{
        KesEvolutions, ProtocolOpCert, ProtocolSignerVerificationKeyForConcatenation,
        ProtocolSignerVerificationKeySignatureForConcatenation,
    },
    entities::{PartyId, Stake},
};
use std::fmt::{Debug, Formatter};

use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};

/// Signer represents a signing participant in the network
#[derive(Clone, Eq, Serialize, Deserialize)]
pub struct Signer {
    /// The unique identifier of the signer
    ///
    /// Used only for testing when SPO pool id is not certified
    pub party_id: PartyId,

    /// The verification key for the Concatenation proof system
    #[serde(rename = "verification_key")]
    pub verification_key_for_concatenation: ProtocolSignerVerificationKeyForConcatenation,

    /// The KES signature over the verification key for Concatenation
    ///
    /// None is used only for testing when SPO pool id is not certified
    #[serde(
        skip_serializing_if = "Option::is_none",
        rename = "verification_key_signature"
    )]
    pub verification_key_signature_for_concatenation:
        Option<ProtocolSignerVerificationKeySignatureForConcatenation>,

    /// The operational certificate of stake pool operator attached to the signer node
    ///
    /// None is used only for testing when SPO pool id is not certified
    #[serde(skip_serializing_if = "Option::is_none")]
    pub operational_certificate: Option<ProtocolOpCert>,

    /// The number of evolutions of the KES key since the start KES period of the operational certificate at the time of signature.
    #[serde(rename = "kes_period", skip_serializing_if = "Option::is_none")]
    pub kes_evolutions: Option<KesEvolutions>,

    /// The verification key for the SNARK proof system
    #[cfg(feature = "future_snark")]
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub verification_key_for_snark: Option<ProtocolSignerVerificationKeyForSnark>,

    /// The KES signature over the verification key for SNARK
    #[cfg(feature = "future_snark")]
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub verification_key_signature_for_snark:
        Option<ProtocolSignerVerificationKeySignatureForSnark>,
}

impl PartialEq for Signer {
    fn eq(&self, other: &Self) -> bool {
        self.party_id.eq(&other.party_id)
    }
}

impl PartialOrd for Signer {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Signer {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.party_id.cmp(&other.party_id)
    }
}

impl Signer {
    /// Signer factory
    pub fn new(
        party_id: PartyId,
        verification_key_for_concatenation: ProtocolSignerVerificationKeyForConcatenation,
        verification_key_signature_for_concatenation: Option<
            ProtocolSignerVerificationKeySignatureForConcatenation,
        >,
        operational_certificate: Option<ProtocolOpCert>,
        kes_evolutions: Option<KesEvolutions>,
        #[cfg(feature = "future_snark")] verification_key_for_snark: Option<
            ProtocolSignerVerificationKeyForSnark,
        >,
        #[cfg(feature = "future_snark")] verification_key_signature_for_snark: Option<
            ProtocolSignerVerificationKeySignatureForSnark,
        >,
    ) -> Signer {
        Signer {
            party_id,
            verification_key_for_concatenation,
            verification_key_signature_for_concatenation,
            operational_certificate,
            kes_evolutions,
            #[cfg(feature = "future_snark")]
            verification_key_for_snark,
            #[cfg(feature = "future_snark")]
            verification_key_signature_for_snark,
        }
    }

    /// Convert the given values to a vec of signers.
    pub fn vec_from<T: Into<Signer>>(from: Vec<T>) -> Vec<Self> {
        from.into_iter().map(|f| f.into()).collect()
    }

    /// Computes the hash of Signer
    pub fn compute_hash(&self) -> String {
        let mut hasher = Sha256::new();
        hasher.update(self.party_id.as_bytes());
        hasher.update(
            self.verification_key_for_concatenation
                .to_json_hex()
                .unwrap()
                .as_bytes(),
        );

        if let Some(verification_key_signature) = &self.verification_key_signature_for_concatenation
        {
            hasher.update(verification_key_signature.to_json_hex().unwrap().as_bytes());
        }
        if let Some(operational_certificate) = &self.operational_certificate {
            hasher.update(operational_certificate.to_json_hex().unwrap().as_bytes());
        }

        #[cfg(feature = "future_snark")]
        if let Some(verification_key_for_snark) = &self.verification_key_for_snark {
            hasher.update(verification_key_for_snark.to_json_hex().unwrap().as_bytes());
        }
        #[cfg(feature = "future_snark")]
        if let Some(verification_key_signature_for_snark) =
            &self.verification_key_signature_for_snark
        {
            hasher.update(verification_key_signature_for_snark.to_json_hex().unwrap().as_bytes());
        }

        hex::encode(hasher.finalize())
    }
}

impl Debug for Signer {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let should_be_exhaustive = f.alternate();
        let mut debug = f.debug_struct("Signer");
        debug.field("party_id", &self.party_id);

        match should_be_exhaustive {
            true => {
                debug
                    .field(
                        "verification_key_for_concatenation",
                        &format_args!("{:?}", self.verification_key_for_concatenation),
                    )
                    .field(
                        "verification_key_signature_for_concatenation",
                        &format_args!("{:?}", self.verification_key_signature_for_concatenation),
                    )
                    .field(
                        "operational_certificate",
                        &format_args!("{:?}", self.operational_certificate),
                    )
                    .field("kes_evolutions", &format_args!("{:?}", self.kes_evolutions));

                #[cfg(feature = "future_snark")]
                {
                    debug
                        .field(
                            "verification_key_for_snark",
                            &format_args!("{:?}", self.verification_key_for_snark),
                        )
                        .field(
                            "verification_key_signature_for_snark",
                            &format_args!("{:?}", self.verification_key_signature_for_snark),
                        );
                }

                debug.finish()
            }
            false => debug.finish_non_exhaustive(),
        }
    }
}

impl From<SignerWithStake> for Signer {
    fn from(other: SignerWithStake) -> Self {
        Signer::new(
            other.party_id,
            other.verification_key_for_concatenation,
            other.verification_key_signature_for_concatenation,
            other.operational_certificate,
            other.kes_evolutions,
            #[cfg(feature = "future_snark")]
            other.verification_key_for_snark,
            #[cfg(feature = "future_snark")]
            other.verification_key_signature_for_snark,
        )
    }
}

/// Signer represents a signing party in the network (including its stakes)
#[derive(Clone, Eq, Serialize, Deserialize)]
pub struct SignerWithStake {
    /// The unique identifier of the signer
    ///
    /// Used only for testing when SPO pool id is not certified
    pub party_id: PartyId,

    /// The verification key for the Concatenation proof system
    #[serde(rename = "verification_key")]
    pub verification_key_for_concatenation: ProtocolSignerVerificationKeyForConcatenation,

    /// The KES signature over the verification key for Concatenation
    ///
    /// None is used only for testing when SPO pool id is not certified
    #[serde(
        skip_serializing_if = "Option::is_none",
        rename = "verification_key_signature"
    )]
    pub verification_key_signature_for_concatenation:
        Option<ProtocolSignerVerificationKeySignatureForConcatenation>,

    /// The operational certificate of stake pool operator attached to the signer node
    ///
    /// None is used only for testing when SPO pool id is not certified
    #[serde(skip_serializing_if = "Option::is_none")]
    pub operational_certificate: Option<ProtocolOpCert>,

    /// The number of evolutions of the KES key since the start KES period of the operational certificate at the time of signature.
    #[serde(rename = "kes_period", skip_serializing_if = "Option::is_none")]
    pub kes_evolutions: Option<KesEvolutions>,

    /// The signer stake
    pub stake: Stake,

    /// The verification key for the SNARK proof system
    #[cfg(feature = "future_snark")]
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub verification_key_for_snark: Option<ProtocolSignerVerificationKeyForSnark>,

    /// The KES signature over the verification key for SNARK (hex encoded)
    #[cfg(feature = "future_snark")]
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub verification_key_signature_for_snark:
        Option<ProtocolSignerVerificationKeySignatureForSnark>,
}

impl PartialEq for SignerWithStake {
    fn eq(&self, other: &Self) -> bool {
        self.party_id.eq(&other.party_id)
    }
}

impl PartialOrd for SignerWithStake {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for SignerWithStake {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.party_id.cmp(&other.party_id)
    }
}

impl SignerWithStake {
    /// SignerWithStake factory
    #[allow(clippy::too_many_arguments)] // TODO: fix this
    pub fn new(
        party_id: PartyId,
        verification_key_for_concatenation: ProtocolSignerVerificationKeyForConcatenation,
        verification_key_signature_for_concatenation: Option<
            ProtocolSignerVerificationKeySignatureForConcatenation,
        >,
        operational_certificate: Option<ProtocolOpCert>,
        kes_evolutions: Option<KesEvolutions>,
        stake: Stake,
        #[cfg(feature = "future_snark")] verification_key_for_snark: Option<
            ProtocolSignerVerificationKeyForSnark,
        >,
        #[cfg(feature = "future_snark")] verification_key_signature_for_snark: Option<
            ProtocolSignerVerificationKeySignatureForSnark,
        >,
    ) -> SignerWithStake {
        SignerWithStake {
            party_id,
            verification_key_for_concatenation,
            verification_key_signature_for_concatenation,
            operational_certificate,
            kes_evolutions,
            stake,
            #[cfg(feature = "future_snark")]
            verification_key_for_snark,
            #[cfg(feature = "future_snark")]
            verification_key_signature_for_snark,
        }
    }

    /// Turn a [Signer] into a [SignerWithStake].
    pub fn from_signer(signer: Signer, stake: Stake) -> Self {
        Self {
            party_id: signer.party_id,
            verification_key_for_concatenation: signer.verification_key_for_concatenation,
            verification_key_signature_for_concatenation: signer
                .verification_key_signature_for_concatenation,
            operational_certificate: signer.operational_certificate,
            kes_evolutions: signer.kes_evolutions,
            stake,
            #[cfg(feature = "future_snark")]
            verification_key_for_snark: signer.verification_key_for_snark,
            #[cfg(feature = "future_snark")]
            verification_key_signature_for_snark: signer.verification_key_signature_for_snark,
        }
    }

    /// Computes the hash of SignerWithStake
    pub fn compute_hash(&self) -> String {
        let mut hasher = Sha256::new();
        hasher.update(self.party_id.as_bytes());
        hasher.update(
            self.verification_key_for_concatenation
                .to_json_hex()
                .unwrap()
                .as_bytes(),
        );

        if let Some(verification_key_signature) = &self.verification_key_signature_for_concatenation
        {
            hasher.update(verification_key_signature.to_json_hex().unwrap().as_bytes());
        }
        if let Some(operational_certificate) = &self.operational_certificate {
            hasher.update(operational_certificate.to_json_hex().unwrap().as_bytes());
        }
        hasher.update(self.stake.to_be_bytes());

        #[cfg(feature = "future_snark")]
        if let Some(verification_key_for_snark) = &self.verification_key_for_snark {
            hasher.update(verification_key_for_snark.to_json_hex().unwrap().as_bytes());
        }
        #[cfg(feature = "future_snark")]
        if let Some(verification_key_signature_for_snark) =
            &self.verification_key_signature_for_snark
        {
            hasher.update(verification_key_signature_for_snark.to_json_hex().unwrap().as_bytes());
        }

        hex::encode(hasher.finalize())
    }
}

impl Debug for SignerWithStake {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let should_be_exhaustive = f.alternate();
        let mut debug = f.debug_struct("SignerWithStake");
        debug.field("party_id", &self.party_id).field("stake", &self.stake);

        match should_be_exhaustive {
            true => {
                debug
                    .field(
                        "verification_key_for_concatenation",
                        &format_args!("{:?}", self.verification_key_for_concatenation),
                    )
                    .field(
                        "verification_key_signature_for_concatenation",
                        &format_args!("{:?}", self.verification_key_signature_for_concatenation),
                    )
                    .field(
                        "operational_certificate",
                        &format_args!("{:?}", self.operational_certificate),
                    )
                    .field("kes_evolutions", &format_args!("{:?}", self.kes_evolutions));

                #[cfg(feature = "future_snark")]
                {
                    debug
                        .field(
                            "verification_key_for_snark",
                            &format_args!("{:?}", self.verification_key_for_snark),
                        )
                        .field(
                            "verification_key_signature_for_snark",
                            &format_args!("{:?}", self.verification_key_signature_for_snark),
                        );
                }

                debug.finish()
            }
            false => debug.finish_non_exhaustive(),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::test::{builder::MithrilFixtureBuilder, double::fake_keys};

    use super::*;

    #[test]
    fn test_stake_signers_from_into() {
        let verification_key = MithrilFixtureBuilder::default()
            .with_signers(1)
            .build()
            .signers_with_stake()[0]
            .verification_key_for_concatenation;
        let signer_expected = Signer::new(
            "1".to_string(),
            verification_key,
            None,
            None,
            None,
            #[cfg(feature = "future_snark")]
            None,
            #[cfg(feature = "future_snark")]
            None,
        );
        let signer_with_stake = SignerWithStake::new(
            "1".to_string(),
            verification_key,
            None,
            None,
            None,
            100,
            #[cfg(feature = "future_snark")]
            None,
            #[cfg(feature = "future_snark")]
            None,
        );

        let signer_into: Signer = signer_with_stake.into();
        assert_eq!(signer_expected, signer_into);
    }

    #[test]
    fn test_signer_compute_hash() {
        const HASH_EXPECTED: &str =
            "02778791113dcd8647b019366e223bfe3aa8a054fa6d9d1918b6b669de485f1c";

        assert_eq!(
            HASH_EXPECTED,
            Signer::new(
                "1".to_string(),
                fake_keys::signer_verification_key()[3].try_into().unwrap(),
                None,
                None,
                None,
                #[cfg(feature = "future_snark")]
                None,
                #[cfg(feature = "future_snark")]
                None,
            )
            .compute_hash()
        );
        assert_ne!(
            HASH_EXPECTED,
            Signer::new(
                "0".to_string(),
                fake_keys::signer_verification_key()[3].try_into().unwrap(),
                None,
                None,
                None,
                #[cfg(feature = "future_snark")]
                None,
                #[cfg(feature = "future_snark")]
                None,
            )
            .compute_hash()
        );
        assert_ne!(
            HASH_EXPECTED,
            Signer::new(
                "1".to_string(),
                fake_keys::signer_verification_key()[0].try_into().unwrap(),
                None,
                None,
                None,
                #[cfg(feature = "future_snark")]
                None,
                #[cfg(feature = "future_snark")]
                None,
            )
            .compute_hash()
        );
    }

    #[test]
    fn test_signer_with_stake_compute_hash() {
        #[cfg(not(feature = "future_snark"))]
        const EXPECTED_HASH: &str =
            "9a832baccd04aabfc419f57319e3831a1655a95bf3bf5ed96a1167d1e81b5085";
        #[cfg(feature = "future_snark")]
        const EXPECTED_HASH: &str =
            "6158c4f514b1e15dc745845dac9014e710ee6b2f0c5b2b1023d5207cf6b75db9";
        let signers = MithrilFixtureBuilder::default()
            .with_signers(2)
            .build()
            .signers_with_stake();
        let signer = signers[0].clone();

        assert_eq!(EXPECTED_HASH, signer.compute_hash());

        {
            let mut signer_different_party_id = signer.clone();
            signer_different_party_id.party_id = "whatever".to_string();

            assert_ne!(EXPECTED_HASH, signer_different_party_id.compute_hash());
        }
        {
            let mut signer_different_verification_key = signer.clone();
            signer_different_verification_key.verification_key_for_concatenation =
                signers[1].verification_key_for_concatenation;

            assert_ne!(
                EXPECTED_HASH,
                signer_different_verification_key.compute_hash()
            );
        }
        {
            let mut signer_different_stake = signer.clone();
            signer_different_stake.stake += 20;

            assert_ne!(EXPECTED_HASH, signer_different_stake.compute_hash());
        }
    }
}
