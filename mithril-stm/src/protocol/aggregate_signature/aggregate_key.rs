use serde::{Deserialize, Serialize};

use crate::{
    OutdatedClosedKeyRegistration, MembershipDigest, Stake,
    membership_commitment::{
        MerkleBatchPath, MerkleTreeBatchCommitment, MerkleTreeConcatenationLeaf,
    },
};

/// Stm aggregate key (batch compatible), which contains the merkle tree commitment and the total stake of the system.
/// Batch Compat Merkle tree commitment includes the number of leaves in the tree in order to obtain batch path.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(bound(
    serialize = "MerkleBatchPath<D::ConcatenationHash>: Serialize",
    deserialize = "MerkleBatchPath<D::ConcatenationHash>: Deserialize<'de>"
))]
pub struct AggregateVerificationKey<D: MembershipDigest> {
    mt_commitment: MerkleTreeBatchCommitment<D::ConcatenationHash, MerkleTreeConcatenationLeaf>,
    total_stake: Stake,
}

impl<D: MembershipDigest> AggregateVerificationKey<D> {
    pub(crate) fn get_merkle_tree_batch_commitment(
        &self,
    ) -> MerkleTreeBatchCommitment<D::ConcatenationHash, MerkleTreeConcatenationLeaf> {
        self.mt_commitment.clone()
    }

    pub fn get_total_stake(&self) -> Stake {
        self.total_stake
    }
}

impl<D: MembershipDigest> PartialEq for AggregateVerificationKey<D> {
    fn eq(&self, other: &Self) -> bool {
        self.mt_commitment == other.mt_commitment && self.total_stake == other.total_stake
    }
}

impl<D: MembershipDigest> Eq for AggregateVerificationKey<D> {}

impl<D: MembershipDigest> From<&OutdatedClosedKeyRegistration<D>> for AggregateVerificationKey<D> {
    fn from(reg: &OutdatedClosedKeyRegistration<D>) -> Self {
        Self {
            mt_commitment: reg.merkle_tree.to_merkle_tree_batch_commitment(),
            total_stake: reg.total_stake,
        }
    }
}

#[cfg(test)]
mod tests {
    use rand_chacha::ChaCha20Rng;
    use rand_core::SeedableRng;

    use super::*;

    mod golden {

        use crate::{Clerk, Initializer, KeyRegistration, MithrilMembershipDigest, Parameters};

        use super::*;

        const GOLDEN_JSON: &str = r#"
        {
            "mt_commitment":{
                "root":[4,3,108,183,145,65,166,69,250,202,51,64,90,232,45,103,56,138,102,63,209,245,81,22,120,16,6,96,140,204,210,55],
                "nr_leaves":4,
                "hasher":null
                },
            "total_stake":6
        }"#;

        fn golden_value() -> AggregateVerificationKey<MithrilMembershipDigest> {
            let mut rng = ChaCha20Rng::from_seed([0u8; 32]);
            let params = Parameters {
                m: 10,
                k: 5,
                phi_f: 0.8,
            };
            let number_of_parties = 4;
            let mut key_reg = KeyRegistration::init();
            for stake in 0..number_of_parties {
                let initializer = Initializer::new(params, stake, &mut rng);
                key_reg.register(initializer.stake, initializer.pk).unwrap();
            }

            let closed_key_reg: ClosedKeyRegistration<MithrilMembershipDigest> = key_reg.close();
            let clerk = Clerk::new_clerk_from_closed_key_registration(&params, &closed_key_reg);
            clerk.compute_aggregate_verification_key()
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
