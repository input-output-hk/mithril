use serde::{Deserialize, Serialize};

use crate::{
    ClosedKeyRegistration, MembershipDigest, RegistrationEntryForSnark, Stake, StmResult,
    membership_commitment::{MerkleTreeCommitment, MerkleTreeError, MerkleTreeSnarkLeaf},
};

/// Aggregate verification key for the SNARK proof system.
///
/// This key embeds the Merkle tree commitment over the SNARK registration entries
/// (Schnorr verification keys and lottery target values), along with the total
/// registered stake.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AggregateVerificationKeyForSnark<D: MembershipDigest> {
    merkle_tree_commitment: MerkleTreeCommitment<D::SnarkHash, MerkleTreeSnarkLeaf>,
    total_stake: Stake,
}

impl<D: MembershipDigest> AggregateVerificationKeyForSnark<D> {
    /// Get the Merkle tree commitment.
    pub(crate) fn get_merkle_tree_commitment(
        &self,
    ) -> &MerkleTreeCommitment<D::SnarkHash, MerkleTreeSnarkLeaf> {
        &self.merkle_tree_commitment
    }

    /// Get the total stake.
    pub fn get_total_stake(&self) -> Stake {
        self.total_stake
    }

    /// Serialize the aggregate verification key for SNARK to bytes.
    ///
    /// Layout: `merkle_tree_commitment || total_stake (8 bytes BE)`
    pub fn to_bytes(&self) -> Vec<u8> {
        let mut bytes = Vec::new();
        bytes.extend(self.merkle_tree_commitment.to_bytes());
        bytes.extend(self.total_stake.to_be_bytes());

        bytes
    }

    /// Deserialize the aggregate verification key for SNARK from bytes.
    pub fn from_bytes(bytes: &[u8]) -> StmResult<Self> {
        if bytes.len() < 8 {
            return Err(MerkleTreeError::SerializationError.into());
        }

        let commitment_end = bytes.len() - 8;
        let merkle_tree_commitment = MerkleTreeCommitment::from_bytes(
            bytes
                .get(..commitment_end)
                .ok_or(MerkleTreeError::SerializationError)?,
        )?;

        let mut u64_bytes = [0u8; 8];
        u64_bytes.copy_from_slice(
            bytes
                .get(commitment_end..commitment_end + 8)
                .ok_or(MerkleTreeError::SerializationError)?,
        );
        let total_stake = u64::from_be_bytes(u64_bytes);

        Ok(Self {
            merkle_tree_commitment,
            total_stake,
        })
    }
}

impl<D: MembershipDigest> PartialEq for AggregateVerificationKeyForSnark<D> {
    fn eq(&self, other: &Self) -> bool {
        self.merkle_tree_commitment.root == other.merkle_tree_commitment.root
            && self.total_stake == other.total_stake
    }
}

impl<D: MembershipDigest> Eq for AggregateVerificationKeyForSnark<D> {}

impl<D: MembershipDigest> From<&ClosedKeyRegistration> for AggregateVerificationKeyForSnark<D> {
    fn from(registration: &ClosedKeyRegistration) -> Self {
        Self {
            merkle_tree_commitment: registration
                .to_merkle_tree::<D::SnarkHash, RegistrationEntryForSnark>()
                .to_merkle_tree_commitment(),
            total_stake: registration.total_stake,
        }
    }
}

#[cfg(test)]
mod tests {
    use rand_chacha::ChaCha20Rng;
    use rand_core::SeedableRng;

    use crate::{
        Initializer, KeyRegistration, MithrilMembershipDigest, Parameters, RegistrationEntry,
        proof_system::AggregateVerificationKeyForSnark,
        proof_system::halo2_snark::clerk::SnarkClerk,
    };

    type D = MithrilMembershipDigest;

    fn setup_closed_registration(
        number_of_parties: u64,
    ) -> (Parameters, crate::ClosedKeyRegistration) {
        let mut rng = ChaCha20Rng::from_seed([0u8; 32]);
        let parameters = Parameters {
            m: 10,
            k: 5,
            phi_f: 0.8,
        };

        let mut key_registration = KeyRegistration::initialize();
        for stake in 1..=number_of_parties {
            let initializer = Initializer::new(parameters, stake, &mut rng);
            let entry = RegistrationEntry::new(
                initializer.get_verification_key_proof_of_possession_for_concatenation(),
                initializer.stake,
                #[cfg(feature = "future_snark")]
                initializer.schnorr_verification_key,
            )
            .unwrap();
            key_registration.register_by_entry(&entry).unwrap();
        }

        let closed_registration = key_registration.close_registration(&parameters).unwrap();
        (parameters, closed_registration)
    }

    mod golden {
        use super::*;

        const GOLDEN_BYTES: &[u8; 40] = &[
            44, 84, 216, 246, 141, 120, 242, 182, 103, 85, 253, 105, 87, 28, 199, 233, 121, 66, 21,
            104, 195, 7, 166, 38, 168, 15, 50, 78, 108, 149, 244, 92, 0, 0, 0, 0, 0, 0, 0, 3,
        ];

        fn golden_value() -> AggregateVerificationKeyForSnark<D> {
            let (parameters, closed_registration) = setup_closed_registration(2);
            let clerk = SnarkClerk::new_clerk_from_closed_key_registration(
                &parameters,
                &closed_registration,
            );

            clerk.compute_aggregate_verification_key_for_snark()
        }

        #[test]
        fn golden_conversions() {
            let value = AggregateVerificationKeyForSnark::<D>::from_bytes(GOLDEN_BYTES)
                .expect("This from bytes should not fail");
            assert_eq!(golden_value(), value);

            let serialized = AggregateVerificationKeyForSnark::<D>::to_bytes(&value);
            let golden_serialized =
                AggregateVerificationKeyForSnark::<D>::to_bytes(&golden_value());
            assert_eq!(golden_serialized, serialized);
        }
    }

    mod golden_json {
        use super::*;

        const GOLDEN_JSON: &str = r#"
        {
            "merkle_tree_commitment":{
                "root":[44,84,216,246,141,120,242,182,103,85,253,105,87,28,199,233,121,66,21,104,195,7,166,38,168,15,50,78,108,149,244,92],
                "hasher":null
            },
            "total_stake":3
        }
        "#;

        fn golden_value() -> AggregateVerificationKeyForSnark<D> {
            let (parameters, closed_registration) = setup_closed_registration(2);
            let clerk = SnarkClerk::new_clerk_from_closed_key_registration(
                &parameters,
                &closed_registration,
            );

            clerk.compute_aggregate_verification_key_for_snark()
        }

        #[test]
        fn golden_conversions() {
            let value: AggregateVerificationKeyForSnark<D> = serde_json::from_str(GOLDEN_JSON)
                .expect("This JSON deserialization should not fail");

            let serialized =
                serde_json::to_string(&value).expect("This JSON serialization should not fail");
            let golden_serialized = serde_json::to_string(&golden_value())
                .expect("This JSON serialization should not fail");
            assert_eq!(golden_serialized, serialized);
        }
    }
}
