use serde::{Deserialize, Serialize};

#[cfg(feature = "future_snark")]
use crate::proof_system::AggregateVerificationKeyForSnark;
use crate::{
    AggregateSignatureType, ClosedKeyRegistration, MembershipDigest, StmResult,
    membership_commitment::MerkleBatchPath, proof_system::AggregateVerificationKeyForConcatenation,
};

use super::AggregateVerificationKeyError;

/// Aggregate verification key
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(bound(
    serialize = "MerkleBatchPath<D::ConcatenationHash>: Serialize",
    deserialize = "MerkleBatchPath<D::ConcatenationHash>: Deserialize<'de>"
))]
pub enum AggregateVerificationKey<D: MembershipDigest> {
    // #[cfg(feature = "future_snark")]
    // /// A future aggregate verification key.
    // Future,
    #[cfg(feature = "future_snark")]
    Snark(AggregateVerificationKeyForSnark<D>),

    /// Concatenation aggregate verification key.
    // The 'untagged' attribute is required for backward compatibility.
    // It implies that this variant is placed at the end of the enum.
    // It will be removed when the support for JSON hex encoding is dropped in the calling crates.
    #[serde(untagged)]
    Concatenation(AggregateVerificationKeyForConcatenation<D>),
}

impl<D: MembershipDigest> AggregateVerificationKey<D> {
    /// If the aggregate verification key is a concatenation aggregate verification key, return it.
    pub fn to_concatenation_proof_key(
        &self,
    ) -> Option<&AggregateVerificationKeyForConcatenation<D>> {
        match self {
            AggregateVerificationKey::Concatenation(key) => Some(key),
            #[cfg(feature = "future_snark")]
            AggregateVerificationKey::Snark(_) => None,
        }
    }

    /// Convert an aggregate verification key to bytes
    pub fn to_bytes(&self) -> Vec<u8> {
        let mut aggregate_verification_key_bytes = Vec::new();
        let aggregate_signature_type: AggregateSignatureType = self.into();
        aggregate_verification_key_bytes
            .extend_from_slice(&[aggregate_signature_type.get_byte_encoding_prefix()]);

        let mut aggregate_verification_key_inner_bytes = match self {
            AggregateVerificationKey::Concatenation(concatenation_aggregate_verification_key) => {
                concatenation_aggregate_verification_key.to_bytes()
            }
            #[cfg(feature = "future_snark")]
            AggregateVerificationKey::Snark(_) => vec![],
        };
        aggregate_verification_key_bytes.append(&mut aggregate_verification_key_inner_bytes);

        aggregate_verification_key_bytes
    }

    /// Extract an aggregate verification key from a byte slice.
    pub fn from_bytes(bytes: &[u8]) -> StmResult<Self> {
        let aggregate_signature_type_byte = bytes
            .first()
            .ok_or(AggregateVerificationKeyError::SerializationError)?;
        let aggregate_verification_key_bytes = &bytes[1..];
        let aggregate_signature_type =
            AggregateSignatureType::from_byte_encoding_prefix(*aggregate_signature_type_byte)
                .ok_or(AggregateVerificationKeyError::SerializationError)?;

        match aggregate_signature_type {
            AggregateSignatureType::Concatenation => Ok(AggregateVerificationKey::Concatenation(
                AggregateVerificationKeyForConcatenation::from_bytes(
                    aggregate_verification_key_bytes,
                )?,
            )),
            #[cfg(feature = "future_snark")]
            AggregateSignatureType::Future => Ok(AggregateVerificationKey::Snark(
                AggregateVerificationKeyForSnark::from_bytes(aggregate_verification_key_bytes)?,
            )),
        }
    }
}

impl<D: MembershipDigest> PartialEq for AggregateVerificationKey<D> {
    fn eq(&self, other: &Self) -> bool {
        self.to_concatenation_proof_key() == other.to_concatenation_proof_key()
    }
}

impl<D: MembershipDigest> Eq for AggregateVerificationKey<D> {}

impl<D: MembershipDigest> From<&ClosedKeyRegistration> for AggregateVerificationKey<D> {
    fn from(reg: &ClosedKeyRegistration) -> Self {
        AggregateVerificationKey::Concatenation(AggregateVerificationKeyForConcatenation::from(reg))
    }
}

impl<D: MembershipDigest> From<&AggregateVerificationKey<D>> for AggregateSignatureType {
    fn from(aggregate_verification_key: &AggregateVerificationKey<D>) -> Self {
        match aggregate_verification_key {
            AggregateVerificationKey::Concatenation(_) => AggregateSignatureType::Concatenation,
            #[cfg(feature = "future_snark")]
            AggregateVerificationKey::Snark(_) => AggregateSignatureType::Future,
        }
    }
}

#[cfg(test)]
mod tests {
    use rand_chacha::ChaCha20Rng;
    use rand_core::SeedableRng;

    use crate::{
        Clerk, ClosedKeyRegistration, Initializer, KeyRegistration, MithrilMembershipDigest,
        Parameters, Signer,
    };

    use super::*;

    mod golden {
        use super::*;

        const GOLDEN_BYTES: &[u8; 49] = &[
            0, 0, 0, 0, 0, 0, 0, 0, 4, 4, 3, 108, 183, 145, 65, 166, 69, 250, 202, 51, 64, 90, 232,
            45, 103, 56, 138, 102, 63, 209, 245, 81, 22, 120, 16, 6, 96, 140, 204, 210, 55, 0, 0,
            0, 0, 0, 0, 0, 6,
        ];

        fn golden_value() -> AggregateVerificationKey<MithrilMembershipDigest> {
            let mut rng = ChaCha20Rng::from_seed([0u8; 32]);
            let params = Parameters {
                m: 10,
                k: 5,
                phi_f: 0.8,
            };
            let number_of_parties = 4;
            let mut key_reg = KeyRegistration::initialize();
            let initializers: Vec<Initializer> = (0..number_of_parties)
                .map(|stake| Initializer::new(params, stake, &mut rng))
                .collect();
            for initializer in &initializers {
                key_reg
                    .register(
                        initializer.clone().stake,
                        &initializer.bls_verification_key_proof_of_possession,
                    )
                    .unwrap();
            }

            let closed_key_reg: ClosedKeyRegistration = key_reg.close_registration();
            let signer: Signer<MithrilMembershipDigest> =
                initializers[0].clone().try_create_signer(&closed_key_reg).unwrap();
            let clerk = Clerk::new_clerk_from_signer(&signer);
            clerk.compute_aggregate_verification_key()
        }

        #[test]
        fn golden_conversions() {
            let value =
                AggregateVerificationKey::<MithrilMembershipDigest>::from_bytes(GOLDEN_BYTES)
                    .expect("This from bytes should not fail");
            assert_eq!(golden_value(), value);

            let serialized = AggregateVerificationKey::<MithrilMembershipDigest>::to_bytes(&value);
            let golden_serialized =
                AggregateVerificationKey::<MithrilMembershipDigest>::to_bytes(&golden_value());
            assert_eq!(golden_serialized, serialized);
        }
    }

    mod golden_json {
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
            let mut key_reg = KeyRegistration::initialize();
            let initializers: Vec<Initializer> = (0..number_of_parties)
                .map(|stake| Initializer::new(params, stake, &mut rng))
                .collect();
            for initializer in &initializers {
                key_reg
                    .register(
                        initializer.clone().stake,
                        &initializer.bls_verification_key_proof_of_possession,
                    )
                    .unwrap();
            }

            let closed_key_reg: ClosedKeyRegistration = key_reg.close_registration();
            let signer: Signer<MithrilMembershipDigest> =
                initializers[0].clone().try_create_signer(&closed_key_reg).unwrap();
            let clerk = Clerk::new_clerk_from_signer(&signer);
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
