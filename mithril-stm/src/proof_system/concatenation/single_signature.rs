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
    /// Create and return a new instance of `SingleSignatureForConcatenation` for given `sigma` and
    /// `indexes`.
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
        self.sigma.verify(&msgp, pk).with_context(
            || "Single signature verification failed for concatenation proof system.",
        )?;
        self.check_indices(params, stake, &msgp, &avk.get_total_stake())
            .with_context(
                || {
                format!(
                    "Single signature verification failed for concatenation proof system; indexes: {:?}",
                    self.indexes
                )
            })?;
        Ok(())
    }

    /// Verify that all indices of single signature are valid by checking bounds and lottery win.
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

    /// Return `indices` of the single signature
    pub(crate) fn get_indices(&self) -> &[Index] {
        &self.indexes
    }

    /// Set `indexes` of single signature to given `indices`
    pub(crate) fn set_indices(&mut self, indices: &[Index]) {
        self.indexes = indices.to_vec()
    }

    /// Return `sigma` of single signature
    pub(crate) fn get_sigma(&self) -> BlsSignature {
        self.sigma
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
