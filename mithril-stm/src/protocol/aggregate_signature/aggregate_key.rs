use serde::{Deserialize, Serialize};

use crate::{
    MembershipDigest, membership_commitment::MerkleBatchPath, proof_system::ConcatenationProofKey,
};

/// An STM aggregate signature.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(bound(
    serialize = "MerkleBatchPath<D::ConcatenationHash>: Serialize",
    deserialize = "MerkleBatchPath<D::ConcatenationHash>: Deserialize<'de>"
))]
pub enum AggregateVerificationKey<D: MembershipDigest> {
    /// A future proof system.
    #[cfg(feature = "future_snark")]
    Future,

    /// Concatenation proof system.
    // The 'untagged' attribute is required for backward compatibility.
    // It implies that this variant is placed at the end of the enum.
    // It will be removed when the support for JSON hex encoding is dropped in the calling crates.
    #[serde(untagged)]
    Concatenation(ConcatenationProofKey<D>),
}

impl<D: MembershipDigest> AggregateVerificationKey<D> {
    /// If the aggregate signature is a concatenation proof, return it.
    pub fn to_concatenation_proof_key(&self) -> Option<&ConcatenationProofKey<D>> {
        match self {
            AggregateVerificationKey::Concatenation(key) => Some(key),
            #[cfg(feature = "future_snark")]
            AggregateVerificationKey::Future => None,
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
