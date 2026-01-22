use std::{
    cmp::Ordering,
    hash::{Hash, Hasher},
};

use serde::{Deserialize, Serialize};

use crate::{
    AggregateVerificationKey, LotteryIndex, MembershipDigest, Parameters, Stake, StmResult,
    VerificationKeyForConcatenation, proof_system::SingleSignatureForConcatenation,
    signature_scheme::BlsSignature,
};

use super::SignatureError;

/// Single signature created by a single party who has won the lottery.
/// Contains the underlying signature for the proof system and the registration index of the signer.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SingleSignature {
    /// Underlying signature for concatenation proof system.
    #[serde(flatten)]
    pub(crate) concatenation_signature: SingleSignatureForConcatenation,
    /// Merkle tree index of the signer.
    pub signer_index: LotteryIndex,
}

impl SingleSignature {
    /// Verify a `SingleSignature` by validating the underlying single signature for proof system.
    ///
    /// It only works for concatenation proof system.
    pub fn verify<D: MembershipDigest>(
        &self,
        params: &Parameters,
        pk: &VerificationKeyForConcatenation,
        stake: &Stake,
        avk: &AggregateVerificationKey<D>,
        msg: &[u8],
    ) -> StmResult<()> {
        self.concatenation_signature.verify(
            params,
            pk,
            stake,
            avk.to_concatenation_aggregate_verification_key(),
            msg,
        )
    }

    /// Verify that all indices of a signature are valid.
    pub(crate) fn check_indices(
        &self,
        params: &Parameters,
        stake: &Stake,
        msg: &[u8],
        total_stake: &Stake,
    ) -> StmResult<()> {
        self.concatenation_signature
            .check_indices(params, stake, msg, total_stake)
    }

    /// Convert a `SingleSignature` into bytes
    ///
    /// # Layout
    /// * Concatenation proof system single signature bytes:
    /// *   Length of indices
    /// *   Indices
    /// *   Sigma
    /// * Merkle index of the signer.
    pub fn to_bytes(&self) -> Vec<u8> {
        let mut output = Vec::new();
        let indices = self.get_concatenation_signature_indices();
        output.extend_from_slice(&(indices.len() as u64).to_be_bytes());

        for index in indices {
            output.extend_from_slice(&index.to_be_bytes());
        }

        output.extend_from_slice(&self.get_concatenation_signature_sigma().to_bytes());

        output.extend_from_slice(&self.signer_index.to_be_bytes());
        output
    }

    /// Extract a `SingleSignature` from a byte slice.
    pub fn from_bytes<D: MembershipDigest>(bytes: &[u8]) -> StmResult<SingleSignature> {
        let mut u64_bytes = [0u8; 8];

        u64_bytes.copy_from_slice(bytes.get(0..8).ok_or(SignatureError::SerializationError)?);
        let nr_indexes = u64::from_be_bytes(u64_bytes) as usize;

        let mut indexes = Vec::new();
        for i in 0..nr_indexes {
            u64_bytes.copy_from_slice(
                bytes
                    .get(8 + i * 8..16 + i * 8)
                    .ok_or(SignatureError::SerializationError)?,
            );
            indexes.push(u64::from_be_bytes(u64_bytes));
        }

        let offset = 8 + nr_indexes * 8;
        let sigma = BlsSignature::from_bytes(
            bytes
                .get(offset..offset + 48)
                .ok_or(SignatureError::SerializationError)?,
        )?;

        u64_bytes.copy_from_slice(
            bytes
                .get(offset + 48..offset + 56)
                .ok_or(SignatureError::SerializationError)?,
        );
        let signer_index = u64::from_be_bytes(u64_bytes);

        Ok(SingleSignature {
            concatenation_signature: SingleSignatureForConcatenation::new(sigma, indexes),
            signer_index,
        })
    }

    /// Get indices of the single signature for concatenation proof system.
    pub fn get_concatenation_signature_indices(&self) -> Vec<LotteryIndex> {
        self.concatenation_signature.get_indices().to_vec()
    }

    /// Get underlying BLS signature of the concatenation single signature.
    pub fn get_concatenation_signature_sigma(&self) -> BlsSignature {
        self.concatenation_signature.get_sigma()
    }

    /// Set the indices of the underlying single signature for proof system.
    pub fn set_concatenation_signature_indices(&mut self, indices: &[LotteryIndex]) {
        self.concatenation_signature.set_indices(indices)
    }
}

impl Hash for SingleSignature {
    fn hash<H: Hasher>(&self, state: &mut H) {
        Hash::hash_slice(&self.concatenation_signature.get_sigma().to_bytes(), state)
    }
}

impl PartialEq for SingleSignature {
    fn eq(&self, other: &Self) -> bool {
        self.concatenation_signature == other.concatenation_signature
    }
}

impl Eq for SingleSignature {}

impl PartialOrd for SingleSignature {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(std::cmp::Ord::cmp(self, other))
    }
}

impl Ord for SingleSignature {
    fn cmp(&self, other: &Self) -> Ordering {
        self.signer_index.cmp(&other.signer_index)
    }
}

#[cfg(test)]
mod tests {

    use rand_chacha::ChaCha20Rng;
    use rand_core::SeedableRng;

    use crate::{
        KeyRegistration, MithrilMembershipDigest, Parameters, RegistrationEntry, Signer,
        SingleSignature, VerificationKeyProofOfPossessionForConcatenation,
        proof_system::ConcatenationProofSigner, signature_scheme::BlsSigningKey,
    };

    mod golden {
        use super::*;

        type D = MithrilMembershipDigest;

        const GOLDEN_BYTES: &[u8; 96] = &[
            0, 0, 0, 0, 0, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 4, 0, 0, 0, 0, 0,
            0, 0, 5, 0, 0, 0, 0, 0, 0, 0, 8, 149, 157, 201, 187, 140, 54, 0, 128, 209, 88, 16, 203,
            61, 78, 77, 98, 161, 133, 58, 152, 29, 74, 217, 113, 64, 100, 10, 161, 186, 167, 133,
            114, 211, 153, 218, 56, 223, 84, 105, 242, 41, 54, 224, 170, 208, 185, 126, 83, 0, 0,
            0, 0, 0, 0, 0, 1,
        ];

        fn golden_value() -> SingleSignature {
            let mut rng = ChaCha20Rng::from_seed([0u8; 32]);
            let message = [0u8; 16];
            let params = Parameters {
                m: 10,
                k: 5,
                phi_f: 0.8,
            };
            let sk_1 = BlsSigningKey::generate(&mut rng);
            let sk_2 = BlsSigningKey::generate(&mut rng);
            let pk_1 = VerificationKeyProofOfPossessionForConcatenation::from(&sk_1);
            let pk_2 = VerificationKeyProofOfPossessionForConcatenation::from(&sk_2);

            let mut registration = KeyRegistration::initialize();
            let entry1 = RegistrationEntry::new(pk_1, 1).unwrap();
            let entry2 = RegistrationEntry::new(pk_2, 1).unwrap();
            registration.register_by_entry(&entry1).unwrap();
            registration.register_by_entry(&entry2).unwrap();
            let closed_key_registration = registration.close_registration();

            let signer: Signer<MithrilMembershipDigest> = Signer::new(
                1,
                ConcatenationProofSigner::new(
                    1,
                    2,
                    params,
                    sk_1,
                    pk_1.vk,
                    closed_key_registration
                        .clone()
                        .key_registration
                        .into_merkle_tree()
                        .unwrap(),
                ),
                closed_key_registration,
                params,
                1,
            );
            signer.create_single_signature(&message).unwrap()
        }

        #[test]
        fn golden_conversions() {
            let value = SingleSignature::from_bytes::<D>(GOLDEN_BYTES)
                .expect("This from bytes should not fail");
            assert_eq!(golden_value(), value);

            let serialized = SingleSignature::to_bytes(&value);
            let golden_serialized = SingleSignature::to_bytes(&golden_value());
            assert_eq!(golden_serialized, serialized);
        }
    }

    mod golden_json {
        use super::*;

        const GOLDEN_JSON: &str = r#"
        {
            "sigma": [
                149, 157, 201, 187, 140, 54, 0, 128, 209, 88, 16, 203, 61, 78, 77, 98, 161,
                133, 58, 152, 29, 74, 217, 113, 64, 100, 10, 161, 186, 167, 133, 114, 211,
                153, 218, 56, 223, 84, 105, 242, 41, 54, 224, 170, 208, 185, 126, 83
            ],
            "indexes": [1, 4, 5, 8],
            "signer_index": 1
        }"#;

        fn golden_value() -> SingleSignature {
            let mut rng = ChaCha20Rng::from_seed([0u8; 32]);
            let message = [0u8; 16];
            let params = Parameters {
                m: 10,
                k: 5,
                phi_f: 0.8,
            };
            let sk_1 = BlsSigningKey::generate(&mut rng);
            let sk_2 = BlsSigningKey::generate(&mut rng);
            let pk_1 = VerificationKeyProofOfPossessionForConcatenation::from(&sk_1);
            let pk_2 = VerificationKeyProofOfPossessionForConcatenation::from(&sk_2);

            let mut registration = KeyRegistration::initialize();
            let entry1 = RegistrationEntry::new(pk_1, 1).unwrap();
            let entry2 = RegistrationEntry::new(pk_2, 1).unwrap();
            registration.register_by_entry(&entry1).unwrap();
            registration.register_by_entry(&entry2).unwrap();

            let closed_key_registration = registration.close_registration();

            let signer: Signer<MithrilMembershipDigest> = Signer::new(
                1,
                ConcatenationProofSigner::new(
                    1,
                    2,
                    params,
                    sk_1,
                    pk_1.vk,
                    closed_key_registration
                        .clone()
                        .key_registration
                        .into_merkle_tree()
                        .unwrap(),
                ),
                closed_key_registration.clone(),
                params,
                1,
            );
            signer.create_single_signature(&message).unwrap()
        }

        #[test]
        fn golden_conversions() {
            let value = serde_json::from_str(GOLDEN_JSON)
                .expect("This JSON deserialization should not fail");
            assert_eq!(golden_value(), value);

            let serialized =
                serde_json::to_string(&value).expect("This JSON serialization should not fail");
            let golden_serialized = serde_json::to_string(&golden_value())
                .expect("This JSON serialization should not fail");
            assert_eq!(golden_serialized, serialized);
        }
    }
}
