use std::{
    cmp::Ordering,
    hash::{Hash, Hasher},
};

use blake2::digest::{Digest, FixedOutput};
use serde::{Deserialize, Serialize};

use crate::bls_multi_signature::BlsSignature;
use crate::eligibility_check::is_lottery_won;
use crate::{
    AggregateVerificationKey, Index, Parameters, Stake, StmSignatureError, VerificationKey,
};

/// Signature created by a single party who has won the lottery.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SingleSignature {
    /// The signature from the underlying MSP scheme.
    pub sigma: BlsSignature,
    /// The index(es) for which the signature is valid
    pub indexes: Vec<Index>,
    /// Merkle tree index of the signer.
    pub signer_index: Index,
}

impl SingleSignature {
    /// Verify an stm signature by checking that the lottery was won, the merkle path is correct,
    /// the indexes are in the desired range and the underlying multi signature validates.
    pub fn verify<D: Clone + Digest + FixedOutput>(
        &self,
        params: &Parameters,
        pk: &VerificationKey,
        stake: &Stake,
        avk: &AggregateVerificationKey<D>,
        msg: &[u8],
    ) -> Result<(), StmSignatureError> {
        let msgp = avk.get_merkle_tree_batch_commitment().concatenate_with_message(msg);
        self.basic_verify(params, pk, stake, &msgp, &avk.get_total_stake())?;
        Ok(())
    }

    /// Verify that all indices of a signature are valid.
    pub(crate) fn check_indices(
        &self,
        params: &Parameters,
        stake: &Stake,
        msg: &[u8],
        total_stake: &Stake,
    ) -> Result<(), StmSignatureError> {
        for &index in &self.indexes {
            if index > params.m {
                return Err(StmSignatureError::IndexBoundFailed(index, params.m));
            }

            let ev = self.sigma.evaluate_dense_mapping(msg, index);

            if !is_lottery_won(params.phi_f, ev, *stake, *total_stake) {
                return Err(StmSignatureError::LotteryLost);
            }
        }

        Ok(())
    }

    /// Convert an `SingleSignature` into bytes
    ///
    /// # Layout
    /// * Stake
    /// * Number of valid indexes (as u64)
    /// * Indexes of the signature
    /// * Public Key
    /// * Signature
    /// * Merkle index of the signer.
    pub fn to_bytes(&self) -> Vec<u8> {
        let mut output = Vec::new();
        output.extend_from_slice(&(self.indexes.len() as u64).to_be_bytes());

        for index in &self.indexes {
            output.extend_from_slice(&index.to_be_bytes());
        }

        output.extend_from_slice(&self.sigma.to_bytes());

        output.extend_from_slice(&self.signer_index.to_be_bytes());
        output
    }

    /// Extract a batch compatible `SingleSignature` from a byte slice.
    pub fn from_bytes<D: Clone + Digest + FixedOutput>(
        bytes: &[u8],
    ) -> Result<SingleSignature, StmSignatureError> {
        let mut u64_bytes = [0u8; 8];

        u64_bytes.copy_from_slice(bytes.get(0..8).ok_or(StmSignatureError::SerializationError)?);
        let nr_indexes = u64::from_be_bytes(u64_bytes) as usize;

        let mut indexes = Vec::new();
        for i in 0..nr_indexes {
            u64_bytes.copy_from_slice(
                bytes
                    .get(8 + i * 8..16 + i * 8)
                    .ok_or(StmSignatureError::SerializationError)?,
            );
            indexes.push(u64::from_be_bytes(u64_bytes));
        }

        let offset = 8 + nr_indexes * 8;
        let sigma = BlsSignature::from_bytes(
            bytes
                .get(offset..offset + 48)
                .ok_or(StmSignatureError::SerializationError)?,
        )?;

        u64_bytes.copy_from_slice(
            bytes
                .get(offset + 48..offset + 56)
                .ok_or(StmSignatureError::SerializationError)?,
        );
        let signer_index = u64::from_be_bytes(u64_bytes);

        Ok(SingleSignature {
            sigma,
            indexes,
            signer_index,
        })
    }

    /// Compare two `SingleSignature` by their signers' merkle tree indexes.
    fn compare_signer_index(&self, other: &Self) -> Ordering {
        self.signer_index.cmp(&other.signer_index)
    }

    /// Compare two `SingleSignature` by their signers' merkle tree indexes.
    #[deprecated(since = "0.5.0", note = "This function will be removed")]
    pub fn cmp_stm_sig(&self, other: &Self) -> Ordering {
        Self::compare_signer_index(self, other)
    }

    /// Verify a basic signature by checking that the lottery was won,
    /// the indexes are in the desired range and the underlying multi signature validates.
    pub(crate) fn basic_verify(
        &self,
        params: &Parameters,
        pk: &VerificationKey,
        stake: &Stake,
        msg: &[u8],
        total_stake: &Stake,
    ) -> Result<(), StmSignatureError> {
        self.sigma.verify(msg, pk)?;
        self.check_indices(params, stake, msg, total_stake)?;

        Ok(())
    }

    /// Will be deprecated. Use `basic_verify` instead.
    #[deprecated(since = "0.5.0", note = "Use `basic_verify` instead")]
    pub fn core_verify(
        &self,
        params: &Parameters,
        pk: &VerificationKey,
        stake: &Stake,
        msg: &[u8],
        total_stake: &Stake,
    ) -> Result<(), StmSignatureError> {
        Self::basic_verify(self, params, pk, stake, msg, total_stake)
    }
}

impl Hash for SingleSignature {
    fn hash<H: Hasher>(&self, state: &mut H) {
        Hash::hash_slice(&self.sigma.to_bytes(), state)
    }
}

impl PartialEq for SingleSignature {
    fn eq(&self, other: &Self) -> bool {
        self.sigma == other.sigma
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
    mod golden {
        use blake2::{Blake2b, digest::consts::U32};
        use rand_chacha::ChaCha20Rng;
        use rand_core::SeedableRng;

        use crate::bls_multi_signature::{BlsSigningKey, BlsVerificationKeyProofOfPossession};
        use crate::{ClosedKeyRegistration, KeyRegistration, Parameters, Signer, SingleSignature};

        type D = Blake2b<U32>;

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
            let closed_key_reg: ClosedKeyRegistration<D> = key_reg.close();
            let signer = Signer::set_signer(1, 1, params, sk_1, pk_1.vk, closed_key_reg);
            signer.sign(&msg).unwrap()
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
