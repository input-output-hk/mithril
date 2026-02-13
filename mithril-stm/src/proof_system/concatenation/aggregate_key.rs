use serde::{Deserialize, Serialize};

use crate::{
    ClosedKeyRegistration, MembershipDigest, RegistrationEntryForConcatenation, Stake, StmResult,
    membership_commitment::{
        MerkleBatchPath, MerkleTreeBatchCommitment, MerkleTreeConcatenationLeaf, MerkleTreeError,
    },
};

/// Aggregate verification key of the concatenation proof system.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(bound(
    serialize = "MerkleBatchPath<D::ConcatenationHash>: Serialize",
    deserialize = "MerkleBatchPath<D::ConcatenationHash>: Deserialize<'de>"
))]
pub struct AggregateVerificationKeyForConcatenation<D: MembershipDigest> {
    mt_commitment: MerkleTreeBatchCommitment<D::ConcatenationHash, MerkleTreeConcatenationLeaf>,
    total_stake: Stake,
}

impl<D: MembershipDigest> AggregateVerificationKeyForConcatenation<D> {
    /// Get the Merkle tree batch commitment.
    pub(crate) fn get_merkle_tree_batch_commitment(
        &self,
    ) -> MerkleTreeBatchCommitment<D::ConcatenationHash, MerkleTreeConcatenationLeaf> {
        self.mt_commitment.clone()
    }

    /// Get the total stake.
    pub fn get_total_stake(&self) -> Stake {
        self.total_stake
    }

    /// Convert the aggregate verification key for concatenation to bytes.
    pub fn to_bytes(&self) -> Vec<u8> {
        let mut bytes = Vec::new();
        bytes.extend(self.mt_commitment.to_bytes());
        bytes.extend(self.total_stake.to_be_bytes());

        bytes
    }

    pub fn from_bytes(bytes: &[u8]) -> StmResult<Self> {
        let mut u64_bytes = [0u8; 8];
        let size = bytes.len();

        u64_bytes.copy_from_slice(&bytes[size - 8..]);
        let stake = u64::from_be_bytes(u64_bytes);
        let mt_commitment = MerkleTreeBatchCommitment::from_bytes(
            bytes.get(..size - 8).ok_or(MerkleTreeError::SerializationError)?,
        )?;
        Ok(Self {
            mt_commitment,
            total_stake: stake,
        })
    }
}

impl<D: MembershipDigest> PartialEq for AggregateVerificationKeyForConcatenation<D> {
    fn eq(&self, other: &Self) -> bool {
        self.mt_commitment == other.mt_commitment && self.total_stake == other.total_stake
    }
}

impl<D: MembershipDigest> Eq for AggregateVerificationKeyForConcatenation<D> {}

impl<D: MembershipDigest> From<&ClosedKeyRegistration>
    for AggregateVerificationKeyForConcatenation<D>
{
    fn from(reg: &ClosedKeyRegistration) -> Self {
        Self {
            mt_commitment: reg
                .to_merkle_tree::<D::ConcatenationHash, RegistrationEntryForConcatenation>()
                .to_merkle_tree_batch_commitment(),
            total_stake: reg.total_stake,
        }
    }
}

#[cfg(test)]
mod tests {
    use rand_chacha::ChaCha20Rng;
    use rand_core::SeedableRng;

    use crate::{
        KeyRegistration, MithrilMembershipDigest, Parameters, RegistrationEntry,
        VerificationKeyProofOfPossessionForConcatenation,
        proof_system::AggregateVerificationKeyForConcatenation,
        proof_system::concatenation::clerk::ConcatenationClerk, signature_scheme::BlsSigningKey,
    };

    type D = MithrilMembershipDigest;
    mod golden {
        use super::*;

        const GOLDEN_BYTES: &[u8; 48] = &[
            0, 0, 0, 0, 0, 0, 0, 2, 56, 37, 95, 107, 157, 98, 252, 194, 190, 204, 170, 26, 224, 10,
            212, 7, 214, 89, 116, 196, 217, 122, 111, 56, 113, 253, 96, 45, 170, 121, 235, 159, 0,
            0, 0, 0, 0, 0, 0, 2,
        ];

        fn golden_value() -> AggregateVerificationKeyForConcatenation<D> {
            let mut rng = ChaCha20Rng::from_seed([0u8; 32]);
            let params = Parameters {
                m: 10,
                k: 5,
                phi_f: 0.8,
            };
            let sk_1 = BlsSigningKey::generate(&mut rng);
            let sk_2 = BlsSigningKey::generate(&mut rng);
            let pk_1 = VerificationKeyProofOfPossessionForConcatenation::from(&sk_1);
            let pk_2 = VerificationKeyProofOfPossessionForConcatenation::from(&sk_2);

            let mut key_reg = KeyRegistration::initialize();
            let entry1 = RegistrationEntry::new(
                pk_1,
                1,
                #[cfg(feature = "future_snark")]
                None,
            )
            .unwrap();
            let entry2 = RegistrationEntry::new(
                pk_2,
                1,
                #[cfg(feature = "future_snark")]
                None,
            )
            .unwrap();

            key_reg.register_by_entry(&entry1).unwrap();
            key_reg.register_by_entry(&entry2).unwrap();
            let closed_key_reg = key_reg.close_registration();

            let clerk = ConcatenationClerk::new_clerk_from_closed_key_registration(
                &params,
                &closed_key_reg,
            );

            clerk.compute_aggregate_verification_key_for_concatenation()
        }

        #[test]
        fn golden_conversions() {
            let value = AggregateVerificationKeyForConcatenation::from_bytes(GOLDEN_BYTES)
                .expect("This from bytes should not fail");
            assert_eq!(golden_value(), value);

            let serialized = AggregateVerificationKeyForConcatenation::to_bytes(&value);
            let golden_serialized =
                AggregateVerificationKeyForConcatenation::to_bytes(&golden_value());
            assert_eq!(golden_serialized, serialized);
        }
    }

    mod golden_json {
        use super::*;

        const GOLDEN_JSON: &str = r#"
        {
            "mt_commitment":{
                "root":[56,37,95,107,157,98,252,194,190,204,170,26,224,10,212,7,214,89,116,196,217,122,111,56,113,253,96,45,170,121,235,159],
                "nr_leaves":2,
                "hasher":null
            },
            "total_stake":2
        }
        "#;

        fn golden_value() -> AggregateVerificationKeyForConcatenation<D> {
            let mut rng = ChaCha20Rng::from_seed([0u8; 32]);
            let params = Parameters {
                m: 10,
                k: 5,
                phi_f: 0.8,
            };
            let sk_1 = BlsSigningKey::generate(&mut rng);
            let sk_2 = BlsSigningKey::generate(&mut rng);
            let pk_1 = VerificationKeyProofOfPossessionForConcatenation::from(&sk_1);
            let pk_2 = VerificationKeyProofOfPossessionForConcatenation::from(&sk_2);

            let mut key_reg = KeyRegistration::initialize();
            let entry1 = RegistrationEntry::new(
                pk_1,
                1,
                #[cfg(feature = "future_snark")]
                None,
            )
            .unwrap();
            let entry2 = RegistrationEntry::new(
                pk_2,
                1,
                #[cfg(feature = "future_snark")]
                None,
            )
            .unwrap();

            key_reg.register_by_entry(&entry1).unwrap();
            key_reg.register_by_entry(&entry2).unwrap();
            let closed_key_reg = key_reg.close_registration();

            let clerk = ConcatenationClerk::new_clerk_from_closed_key_registration(
                &params,
                &closed_key_reg,
            );

            clerk.compute_aggregate_verification_key_for_concatenation()
        }

        #[test]
        fn golden_conversions() {
            let value: AggregateVerificationKeyForConcatenation<D> =
                serde_json::from_str(GOLDEN_JSON)
                    .expect("This JSON deserialization should not fail");

            let serialized =
                serde_json::to_string(&value).expect("This JSON serialization should not fail");
            let golden_serialized = serde_json::to_string(&golden_value())
                .expect("This JSON serialization should not fail");
            assert_eq!(golden_serialized, serialized);
        }
    }
}
