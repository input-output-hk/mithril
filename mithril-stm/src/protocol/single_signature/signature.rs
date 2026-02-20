use serde::{Deserialize, Serialize};
use std::{
    cmp::Ordering,
    hash::{Hash, Hasher},
};

use crate::{
    AggregateVerificationKey, LotteryIndex, MembershipDigest, Parameters, SignerIndex, Stake,
    StmResult, VerificationKeyForConcatenation, proof_system::SingleSignatureForConcatenation,
    signature_scheme::BlsSignature,
};
#[cfg(feature = "future_snark")]
use crate::{RegistrationEntryForSnark, proof_system::SingleSignatureForSnark};

use super::SignatureError;

/// Single signature created by a single party who has won the lottery.
/// Contains the underlying signature for the proof system and the registration index of the signer.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SingleSignature {
    /// Underlying signature for concatenation proof system.
    #[serde(flatten)]
    pub(crate) concatenation_signature: SingleSignatureForConcatenation,
    /// Merkle tree index of the signer.
    pub signer_index: SignerIndex,
    /// Underlying signature for snark proof system.
    #[cfg(feature = "future_snark")]
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub(crate) snark_signature: Option<SingleSignatureForSnark>,
}

impl SingleSignature {
    /// Verify a `SingleSignature` by validating the underlying single signature for proof system.
    pub fn verify<D: MembershipDigest>(
        &self,
        params: &Parameters,
        pk: &VerificationKeyForConcatenation,
        stake: &Stake,
        avk: &AggregateVerificationKey<D>,
        msg: &[u8],
        #[cfg(feature = "future_snark")] snark_registration_entry: Option<
            RegistrationEntryForSnark,
        >,
    ) -> StmResult<()> {
        #[cfg(feature = "future_snark")]
        if let (Some(snark_signature), Some(entry), Some(snark_avk)) = (
            &self.snark_signature,
            snark_registration_entry,
            avk.to_snark_aggregate_verification_key(),
        ) {
            snark_signature.verify(params, &entry.0, msg, &entry.1, snark_avk)?;
        }

        self.concatenation_signature.verify(
            params,
            pk,
            stake,
            avk.to_concatenation_aggregate_verification_key(),
            msg,
        )
    }

    /// Verify that all indices of a concatenation signature are valid.
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
    /// * (Optional) - `future_snark` Snark proof system single signature bytes:
    /// *   Schnorr signature bytes
    /// *   Minimum winning lottery index
    pub fn to_bytes(&self) -> Vec<u8> {
        let mut output = Vec::new();
        let indices = self.get_concatenation_signature_indices();
        output.extend_from_slice(&(indices.len() as u64).to_be_bytes());

        for index in indices {
            output.extend_from_slice(&index.to_be_bytes());
        }

        output.extend_from_slice(&self.get_concatenation_signature_sigma().to_bytes());

        output.extend_from_slice(&self.signer_index.to_be_bytes());

        #[cfg(feature = "future_snark")]
        if let Some(snark_signature) = &self.snark_signature {
            let unique_schnorr_signature = snark_signature.get_schnorr_signature();
            let minimum_winning_lottery_index = snark_signature.get_minimum_winning_lottery_index();
            output.extend_from_slice(&unique_schnorr_signature.to_bytes());
            output.extend_from_slice(&minimum_winning_lottery_index.to_be_bytes());
        }

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

        #[cfg(feature = "future_snark")]
        let snark_signature = {
            let snark_offset = offset + 56;
            if snark_offset < bytes.len() {
                let schnorr_signature = crate::UniqueSchnorrSignature::from_bytes(
                    bytes
                        .get(snark_offset..snark_offset + 96)
                        .ok_or(SignatureError::SerializationError)?,
                )?;
                u64_bytes.copy_from_slice(
                    bytes
                        .get(snark_offset + 96..snark_offset + 104)
                        .ok_or(SignatureError::SerializationError)?,
                );
                let minimum_winning_lottery_index = u64::from_be_bytes(u64_bytes);
                Some(SingleSignatureForSnark::new(
                    schnorr_signature,
                    minimum_winning_lottery_index,
                ))
            } else {
                None
            }
        };

        Ok(SingleSignature {
            concatenation_signature: SingleSignatureForConcatenation::new(sigma, indexes),
            signer_index,
            #[cfg(feature = "future_snark")]
            snark_signature,
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

    /// Set the indices of the single signature for concatenation proof system.
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

    #[cfg(feature = "future_snark")]
    use crate::{
        ClosedRegistrationEntry, MembershipDigest, VerificationKeyForSnark,
        proof_system::SnarkProofSigner, protocol::RegistrationEntryForSnark,
        signature_scheme::SchnorrSigningKey,
    };
    use crate::{
        KeyRegistration, MithrilMembershipDigest, Parameters, RegistrationEntry, SingleSignature,
        VerificationKeyProofOfPossessionForConcatenation, proof_system::ConcatenationProofSigner,
        signature_scheme::BlsSigningKey,
    };

    mod golden {
        use super::*;

        type D = MithrilMembershipDigest;

        #[cfg(not(feature = "future_snark"))]
        const GOLDEN_BYTES: &[u8; 96] = &[
            0, 0, 0, 0, 0, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 4, 0, 0, 0, 0, 0,
            0, 0, 5, 0, 0, 0, 0, 0, 0, 0, 8, 149, 157, 201, 187, 140, 54, 0, 128, 209, 88, 16, 203,
            61, 78, 77, 98, 161, 133, 58, 152, 29, 74, 217, 113, 64, 100, 10, 161, 186, 167, 133,
            114, 211, 153, 218, 56, 223, 84, 105, 242, 41, 54, 224, 170, 208, 185, 126, 83, 0, 0,
            0, 0, 0, 0, 0, 1,
        ];

        #[cfg(feature = "future_snark")]
        const GOLDEN_BYTES: &[u8; 208] = &[
            0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 4, 0, 0, 0, 0, 0,
            0, 0, 5, 0, 0, 0, 0, 0, 0, 0, 6, 0, 0, 0, 0, 0, 0, 0, 7, 140, 18, 156, 86, 86, 16, 179,
            117, 148, 17, 195, 177, 207, 235, 93, 252, 78, 244, 112, 94, 47, 18, 158, 15, 78, 76,
            80, 43, 116, 242, 116, 205, 252, 21, 194, 58, 162, 117, 201, 62, 40, 190, 21, 183, 178,
            186, 196, 136, 0, 0, 0, 0, 0, 0, 0, 1, 198, 195, 131, 147, 143, 246, 147, 31, 112, 104,
            4, 197, 184, 150, 239, 16, 122, 195, 82, 217, 135, 174, 163, 231, 197, 102, 37, 57,
            253, 182, 126, 72, 116, 67, 192, 99, 53, 189, 46, 158, 53, 70, 174, 132, 144, 179, 25,
            203, 87, 11, 59, 253, 155, 114, 211, 22, 16, 29, 4, 233, 203, 127, 170, 6, 128, 135,
            196, 3, 229, 138, 6, 47, 81, 118, 6, 77, 1, 148, 175, 28, 88, 124, 103, 229, 155, 213,
            96, 68, 7, 94, 216, 151, 207, 157, 220, 67, 0, 0, 0, 0, 0, 0, 0, 0,
        ];

        fn golden_value() -> SingleSignature {
            let mut rng = ChaCha20Rng::from_seed([0u8; 32]);
            #[cfg(not(feature = "future_snark"))]
            let message = [0u8; 16];

            #[cfg(feature = "future_snark")]
            let message = [0u8; 32];

            let params = Parameters {
                m: 10,
                k: 5,
                phi_f: 0.8,
            };
            let sk_1 = BlsSigningKey::generate(&mut rng);
            let sk_2 = BlsSigningKey::generate(&mut rng);
            let pk_1 = VerificationKeyProofOfPossessionForConcatenation::from(&sk_1);
            let pk_2 = VerificationKeyProofOfPossessionForConcatenation::from(&sk_2);

            #[cfg(feature = "future_snark")]
            let (schnorr_sk_1, schnorr_vk_1) = {
                let sk = SchnorrSigningKey::generate(&mut rng);
                let vk = VerificationKeyForSnark::new_from_signing_key(sk.clone());
                (sk, vk)
            };
            #[cfg(feature = "future_snark")]
            let schnorr_vk_2 = {
                let sk = SchnorrSigningKey::generate(&mut rng);
                VerificationKeyForSnark::new_from_signing_key(sk)
            };

            let mut registration = KeyRegistration::initialize();
            let entry1 = RegistrationEntry::new(
                pk_1,
                1,
                #[cfg(feature = "future_snark")]
                Some(schnorr_vk_1),
            )
            .unwrap();
            let entry2 = RegistrationEntry::new(
                pk_2,
                1,
                #[cfg(feature = "future_snark")]
                Some(schnorr_vk_2),
            )
            .unwrap();
            registration.register_by_entry(&entry1).unwrap();
            registration.register_by_entry(&entry2).unwrap();
            let closed_key_registration = registration.close_registration();

            let concatenation_proof_signer: ConcatenationProofSigner<D> =
                ConcatenationProofSigner::new(
                    1,
                    2,
                    params,
                    sk_1,
                    pk_1.vk,
                    closed_key_registration
                        .to_merkle_tree()
                        .to_merkle_tree_batch_commitment(),
                );
            let concatenation_signature =
                concatenation_proof_signer.create_single_signature(&message).unwrap();

            #[cfg(feature = "future_snark")]
            let snark_signature = {
                let key_registration_commitment = closed_key_registration
                    .to_merkle_tree::<<D as MembershipDigest>::SnarkHash, RegistrationEntryForSnark>(
                ).to_merkle_tree_commitment();
                let closed_registration_entry =
                    ClosedRegistrationEntry::from((entry1, closed_key_registration.total_stake));
                let lottery_target_value =
                    closed_registration_entry.get_lottery_target_value().unwrap();
                let snark_proof_signer = SnarkProofSigner::<D>::new(
                    params,
                    schnorr_sk_1,
                    schnorr_vk_1,
                    lottery_target_value,
                    key_registration_commitment,
                );
                Some(
                    snark_proof_signer
                        .create_single_signature(&message, &mut rng)
                        .unwrap(),
                )
            };

            SingleSignature {
                concatenation_signature,
                signer_index: 1,
                #[cfg(feature = "future_snark")]
                snark_signature,
            }
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

        type D = MithrilMembershipDigest;

        #[cfg(not(feature = "future_snark"))]
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

        #[cfg(feature = "future_snark")]
        const GOLDEN_JSON: &str = r#"
        {
            "sigma": [
                140, 18, 156, 86, 86, 16, 179, 117, 148, 17, 195, 177, 207, 235, 93, 252,
                78, 244, 112, 94, 47, 18, 158, 15, 78, 76, 80, 43, 116, 242, 116, 205,
                252, 21, 194, 58, 162, 117, 201, 62, 40, 190, 21, 183, 178, 186, 196, 136
            ],
            "indexes": [3, 4, 5, 6, 7],
            "signer_index": 1,
            "snark_signature": {
                "schnorr_signature": {
                    "commitment_point": [
                        198, 195, 131, 147, 143, 246, 147, 31, 112, 104, 4, 197, 184, 150,
                        239, 16, 122, 195, 82, 217, 135, 174, 163, 231, 197, 102, 37, 57,
                        253, 182, 126, 72
                    ],
                    "response": [
                        116, 67, 192, 99, 53, 189, 46, 158, 53, 70, 174, 132, 144, 179, 25,
                        203, 87, 11, 59, 253, 155, 114, 211, 22, 16, 29, 4, 233, 203, 127,
                        170, 6
                    ],
                    "challenge": [
                        128, 135, 196, 3, 229, 138, 6, 47, 81, 118, 6, 77, 1, 148, 175, 28,
                        88, 124, 103, 229, 155, 213, 96, 68, 7, 94, 216, 151, 207, 157,
                        220, 67
                    ]
                },
                "minimum_winning_lottery_index": 0
            }
        }"#;

        fn golden_value() -> SingleSignature {
            let mut rng = ChaCha20Rng::from_seed([0u8; 32]);
            #[cfg(not(feature = "future_snark"))]
            let message = [0u8; 16];

            #[cfg(feature = "future_snark")]
            let message = [0u8; 32];

            let params = Parameters {
                m: 10,
                k: 5,
                phi_f: 0.8,
            };
            let sk_1 = BlsSigningKey::generate(&mut rng);
            let sk_2 = BlsSigningKey::generate(&mut rng);
            let pk_1 = VerificationKeyProofOfPossessionForConcatenation::from(&sk_1);
            let pk_2 = VerificationKeyProofOfPossessionForConcatenation::from(&sk_2);

            #[cfg(feature = "future_snark")]
            let (schnorr_sk_1, schnorr_vk_1) = {
                let sk = SchnorrSigningKey::generate(&mut rng);
                let vk = VerificationKeyForSnark::new_from_signing_key(sk.clone());
                (sk, vk)
            };
            #[cfg(feature = "future_snark")]
            let schnorr_vk_2 = {
                let sk = SchnorrSigningKey::generate(&mut rng);
                VerificationKeyForSnark::new_from_signing_key(sk)
            };

            let mut registration = KeyRegistration::initialize();
            let entry1 = RegistrationEntry::new(
                pk_1,
                1,
                #[cfg(feature = "future_snark")]
                Some(schnorr_vk_1),
            )
            .unwrap();
            let entry2 = RegistrationEntry::new(
                pk_2,
                1,
                #[cfg(feature = "future_snark")]
                Some(schnorr_vk_2),
            )
            .unwrap();
            registration.register_by_entry(&entry1).unwrap();
            registration.register_by_entry(&entry2).unwrap();

            let closed_key_registration = registration.close_registration();

            let concatenation_proof_signer: ConcatenationProofSigner<D> =
                ConcatenationProofSigner::new(
                    1,
                    2,
                    params,
                    sk_1,
                    pk_1.vk,
                    closed_key_registration
                        .to_merkle_tree()
                        .to_merkle_tree_batch_commitment(),
                );
            let concatenation_signature =
                concatenation_proof_signer.create_single_signature(&message).unwrap();

            #[cfg(feature = "future_snark")]
            let snark_signature = {
                let key_registration_commitment = closed_key_registration
                    .to_merkle_tree::<<D as MembershipDigest>::SnarkHash, RegistrationEntryForSnark>(
                ).to_merkle_tree_commitment();
                let closed_registration_entry =
                    ClosedRegistrationEntry::from((entry1, closed_key_registration.total_stake));
                let lottery_target_value =
                    closed_registration_entry.get_lottery_target_value().unwrap();
                let snark_proof_signer = SnarkProofSigner::<D>::new(
                    params,
                    schnorr_sk_1,
                    schnorr_vk_1,
                    lottery_target_value,
                    key_registration_commitment,
                );
                Some(
                    snark_proof_signer
                        .create_single_signature(&message, &mut rng)
                        .unwrap(),
                )
            };

            SingleSignature {
                concatenation_signature,
                signer_index: 1,
                #[cfg(feature = "future_snark")]
                snark_signature,
            }
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
