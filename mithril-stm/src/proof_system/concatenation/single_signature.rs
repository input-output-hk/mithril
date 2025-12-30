use std::{
    cmp::Ordering,
    hash::{Hash, Hasher},
};

use anyhow::{Context, anyhow};
use serde::{Deserialize, Serialize};

use crate::{
    AggregateVerificationKey, Index, MembershipDigest, Parameters, SignatureError, Stake,
    StmResult, VerificationKey, is_lottery_won, signature_scheme::BlsSignature,
};

/// Single signature for the concatenation proof system.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub(crate) struct SingleSignatureForConcatenation {
    /// The underlying BLS signature
    sigma: BlsSignature,
    /// The index(es) for which the signature is valid
    indexes: Vec<Index>,
}

impl SingleSignatureForConcatenation {
    pub(crate) fn new(sigma: BlsSignature, indexes: Vec<Index>) -> Self {
        Self { sigma, indexes }
    }

    /// Verify a `SingleSignatureForConcatenation` by validating the underlying BLS signature and checking
    /// that the lottery was won for all indexes.
    pub(crate) fn verify<D: MembershipDigest>(
        &self,
        params: &Parameters,
        pk: &VerificationKey,
        stake: &Stake,
        avk: &AggregateVerificationKey<D>,
        msg: &[u8],
    ) -> StmResult<()> {
        let msgp = avk.get_merkle_tree_batch_commitment().concatenate_with_message(msg);
        self.sigma
            .verify(&msgp, pk)
            .with_context(|| "Single signature verification failed.")?;
        self.check_indices(params, stake, &msgp, &avk.get_total_stake())
            .with_context(|| "Single signature verification failed.")?;
        Ok(())
    }

    /// Verify that all indices of single signature are valid by cehcking bounds and lottery win.
    pub(crate) fn check_indices(
        &self,
        params: &Parameters,
        stake: &Stake,
        msg: &[u8],
        total_stake: &Stake,
    ) -> StmResult<()> {
        for &index in &self.indexes {
            if index > params.m {
                return Err(anyhow!(SignatureError::IndexBoundFailed(index, params.m)));
            }

            let ev = self.sigma.evaluate_dense_mapping(msg, index);

            if !is_lottery_won(params.phi_f, ev, *stake, *total_stake) {
                return Err(anyhow!(SignatureError::LotteryLost));
            }
        }
        Ok(())
    }

    pub(crate) fn get_indices(&self) -> Vec<Index> {
        self.indexes.clone()
    }

    pub(crate) fn set_indices(&mut self, indices: &[Index]) {
        self.indexes = indices.to_vec()
    }

    pub(crate) fn get_sigma(&self) -> BlsSignature {
        self.sigma
    }

    #[cfg(test)]
    /// Convert a `SingleSignatureForConcatenation` into bytes
    ///
    /// # Layout
    /// * Number of valid indices (as u64)
    /// * Winning indices for the signature
    /// * BLS Signature
    pub(crate) fn to_bytes(&self) -> Vec<u8> {
        let mut output = Vec::new();
        output.extend_from_slice(&(self.indexes.len() as u64).to_be_bytes());

        for index in &self.indexes {
            output.extend_from_slice(&index.to_be_bytes());
        }

        output.extend_from_slice(&self.sigma.to_bytes());
        output
    }

    #[cfg(test)]
    /// Extract a `SingleSignatureForConcatenation` from a byte slice.
    pub(crate) fn from_bytes(bytes: &[u8]) -> StmResult<SingleSignatureForConcatenation> {
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

        Ok(SingleSignatureForConcatenation { sigma, indexes })
    }
}

impl Hash for SingleSignatureForConcatenation {
    fn hash<H: Hasher>(&self, state: &mut H) {
        Hash::hash_slice(&self.sigma.to_bytes(), state)
    }
}

impl PartialEq for SingleSignatureForConcatenation {
    fn eq(&self, other: &Self) -> bool {
        self.sigma == other.sigma
    }
}

impl Eq for SingleSignatureForConcatenation {}

impl PartialOrd for SingleSignatureForConcatenation {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(std::cmp::Ord::cmp(self, other))
    }
}

impl Ord for SingleSignatureForConcatenation {
    fn cmp(&self, other: &Self) -> Ordering {
        self.sigma.cmp(&other.sigma)
    }
}

#[cfg(test)]
mod tests {

    use rand_chacha::ChaCha20Rng;
    use rand_core::SeedableRng;

    use crate::{
        ClosedKeyRegistration, KeyRegistration, MithrilMembershipDigest, Parameters, Signer,
        signature_scheme::{BlsSigningKey, BlsVerificationKeyProofOfPossession},
    };

    use super::*;

    mod golden {
        use super::*;

        const GOLDEN_BYTES: &[u8; 88] = &[
            0, 0, 0, 0, 0, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 4, 0, 0, 0, 0, 0,
            0, 0, 5, 0, 0, 0, 0, 0, 0, 0, 8, 149, 157, 201, 187, 140, 54, 0, 128, 209, 88, 16, 203,
            61, 78, 77, 98, 161, 133, 58, 152, 29, 74, 217, 113, 64, 100, 10, 161, 186, 167, 133,
            114, 211, 153, 218, 56, 223, 84, 105, 242, 41, 54, 224, 170, 208, 185, 126, 83,
        ];

        fn golden_value() -> SingleSignatureForConcatenation {
            let mut rng = ChaCha20Rng::from_seed([0u8; 32]);
            let msg = [0u8; 16];
            let params = Parameters {
                m: 10,
                k: 5,
                phi_f: 0.8,
            };
            let sk_1 = BlsSigningKey::generate(&mut rng);
            let sk_2 = BlsSigningKey::generate(&mut rng);
            let pk_1 = BlsVerificationKeyProofOfPossession::from(&sk_1);
            let pk_2 = BlsVerificationKeyProofOfPossession::from(&sk_2);
            let mut key_reg = KeyRegistration::init();
            key_reg.register(1, pk_1).unwrap();
            key_reg.register(1, pk_2).unwrap();
            let closed_key_reg: ClosedKeyRegistration<MithrilMembershipDigest> = key_reg.close();
            let signer = Signer::set_signer(1, 1, params, sk_1, pk_1.vk, closed_key_reg);
            signer.sign(&msg).unwrap().concatenation_signature
        }

        #[test]
        fn golden_conversions() {
            let value = SingleSignatureForConcatenation::from_bytes(GOLDEN_BYTES)
                .expect("This from bytes should not fail");
            assert_eq!(golden_value(), value);

            let serialized = SingleSignatureForConcatenation::to_bytes(&value);
            let golden_serialized = SingleSignatureForConcatenation::to_bytes(&golden_value());
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
            "indexes": [1, 4, 5, 8]
        }"#;

        fn golden_value() -> SingleSignatureForConcatenation {
            let mut rng = ChaCha20Rng::from_seed([0u8; 32]);
            let msg = [0u8; 16];
            let params = Parameters {
                m: 10,
                k: 5,
                phi_f: 0.8,
            };
            let sk_1 = BlsSigningKey::generate(&mut rng);
            let sk_2 = BlsSigningKey::generate(&mut rng);
            let pk_1 = BlsVerificationKeyProofOfPossession::from(&sk_1);
            let pk_2 = BlsVerificationKeyProofOfPossession::from(&sk_2);
            let mut key_reg = KeyRegistration::init();
            key_reg.register(1, pk_1).unwrap();
            key_reg.register(1, pk_2).unwrap();
            let closed_key_reg: ClosedKeyRegistration<MithrilMembershipDigest> = key_reg.close();
            let signer = Signer::set_signer(1, 1, params, sk_1, pk_1.vk, closed_key_reg);
            signer.sign(&msg).unwrap().concatenation_signature
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
