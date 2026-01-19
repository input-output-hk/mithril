use std::{collections::HashMap, fmt::Display, hash::Hash, str::FromStr};

use anyhow::anyhow;
use serde::{Deserialize, Serialize};

use crate::{
    MembershipDigest, Parameters, StmError, StmResult, membership_commitment::MerkleBatchPath,
    proof_system::ConcatenationProof,
};

use super::{AggregateSignatureError, AggregateVerificationKey};

/// The type of STM aggregate signature.
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum AggregateSignatureType {
    /// Concatenation proof system.
    #[default]
    Concatenation,
    /// Snark proof system.
    /// TODO: Implement this variant everywhere
    #[cfg(feature = "future_snark")]
    Snark,
}

impl AggregateSignatureType {
    /// The prefix byte used in the byte representation of the aggregate signature type.
    ///
    /// IMPORTANT: This value is used in serialization/deserialization. Changing it will break compatibility.
    pub fn get_byte_encoding_prefix(&self) -> u8 {
        match self {
            AggregateSignatureType::Concatenation => 0,
            #[cfg(feature = "future_snark")]
            AggregateSignatureType::Snark => 255,
        }
    }

    /// Create an aggregate signature type from a prefix byte.
    ///
    /// IMPORTANT: This value is used in serialization/deserialization. Changing it will break compatibility.
    pub fn from_byte_encoding_prefix(byte: u8) -> Option<Self> {
        match byte {
            0 => Some(AggregateSignatureType::Concatenation),
            #[cfg(feature = "future_snark")]
            255 => Some(AggregateSignatureType::Snark),
            _ => None,
        }
    }
}

impl<D: MembershipDigest> From<&AggregateSignature<D>> for AggregateSignatureType {
    fn from(aggr_sig: &AggregateSignature<D>) -> Self {
        match aggr_sig {
            AggregateSignature::Concatenation(_) => AggregateSignatureType::Concatenation,
            #[cfg(feature = "future_snark")]
            AggregateSignature::Snark => AggregateSignatureType::Snark,
        }
    }
}

impl FromStr for AggregateSignatureType {
    type Err = StmError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "Concatenation" => Ok(AggregateSignatureType::Concatenation),
            #[cfg(feature = "future_snark")]
            "Snark" => Ok(AggregateSignatureType::Snark),
            _ => Err(anyhow!("Unknown aggregate signature type: {}", s)),
        }
    }
}

impl Display for AggregateSignatureType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AggregateSignatureType::Concatenation => write!(f, "Concatenation"),
            #[cfg(feature = "future_snark")]
            AggregateSignatureType::Snark => write!(f, "Snark"),
        }
    }
}

/// An STM aggregate signature.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(bound(
    serialize = "MerkleBatchPath<D::ConcatenationHash>: Serialize",
    deserialize = "MerkleBatchPath<D::ConcatenationHash>: Deserialize<'de>"
))]
pub enum AggregateSignature<D: MembershipDigest> {
    /// Snark proof system.
    /// TODO: Implement this variant everywhere
    #[cfg(feature = "future_snark")]
    Snark,

    /// Concatenation proof system.
    // The 'untagged' attribute is required for backward compatibility.
    // It implies that this variant is placed at the end of the enum.
    // It will be removed when the support for JSON hex encoding is dropped in the calling crates.
    #[serde(untagged)]
    Concatenation(ConcatenationProof<D>),
}

impl<D: MembershipDigest> AggregateSignature<D> {
    /// Verify an aggregate signature
    pub fn verify(
        &self,
        msg: &[u8],
        avk: &AggregateVerificationKey<D>,
        parameters: &Parameters,
    ) -> StmResult<()> {
        match self {
            AggregateSignature::Concatenation(concatenation_proof) => concatenation_proof.verify(
                msg,
                avk.to_concatenation_proof_key().unwrap(),
                parameters,
            ),
            #[cfg(feature = "future_snark")]
            AggregateSignature::Snark => Err(anyhow!(
                AggregateSignatureError::UnsupportedProofSystem(self.into())
            )),
        }
    }

    /// Batch verify a set of aggregate signatures
    pub fn batch_verify(
        stm_signatures: &[Self],
        msgs: &[Vec<u8>],
        avks: &[AggregateVerificationKey<D>],
        parameters: &[Parameters],
    ) -> StmResult<()> {
        let stm_signatures: HashMap<AggregateSignatureType, Vec<Self>> =
            stm_signatures.iter().fold(HashMap::new(), |mut acc, sig| {
                acc.entry(sig.into()).or_default().push(sig.clone());
                acc
            });
        stm_signatures.into_iter().try_for_each(
            |(aggregate_signature_type, aggregate_signatures)| match aggregate_signature_type {
                AggregateSignatureType::Concatenation => {
                    let aggregate_signatures_length = aggregate_signatures.len();
                    let concatenation_proofs = aggregate_signatures
                        .into_iter()
                        .filter_map(|s| s.to_concatenation_proof().cloned())
                        .collect::<Vec<_>>();
                    if concatenation_proofs.len() != aggregate_signatures_length {
                        return Err(anyhow!(AggregateSignatureError::BatchInvalid));
                    }
                    let avks = avks
                        .iter()
                        .filter_map(|avk| avk.to_concatenation_proof_key())
                        .cloned()
                        .collect::<Vec<_>>();
                    ConcatenationProof::batch_verify(&concatenation_proofs, msgs, &avks, parameters)
                }
                #[cfg(feature = "future_snark")]
                AggregateSignatureType::Snark => Err(anyhow!(
                    AggregateSignatureError::UnsupportedProofSystem(aggregate_signature_type)
                )),
            },
        )
    }

    /// Convert an aggregate signature to bytes
    pub fn to_bytes(&self) -> Vec<u8> {
        let mut aggregate_signature_bytes = Vec::new();
        let aggregate_signature_type: AggregateSignatureType = self.into();
        aggregate_signature_bytes
            .extend_from_slice(&[aggregate_signature_type.get_byte_encoding_prefix()]);

        let mut proof_bytes = match self {
            AggregateSignature::Concatenation(concatenation_proof) => {
                concatenation_proof.to_bytes()
            }
            #[cfg(feature = "future_snark")]
            AggregateSignature::Snark => vec![],
        };
        aggregate_signature_bytes.append(&mut proof_bytes);

        aggregate_signature_bytes
    }

    /// Extract an aggregate signature from a byte slice.
    pub fn from_bytes(bytes: &[u8]) -> StmResult<Self> {
        let proof_type_byte = bytes.first().ok_or(AggregateSignatureError::SerializationError)?;
        let proof_bytes = &bytes[1..];
        let proof_type = AggregateSignatureType::from_byte_encoding_prefix(*proof_type_byte)
            .ok_or(AggregateSignatureError::SerializationError)?;

        match proof_type {
            AggregateSignatureType::Concatenation => Ok(AggregateSignature::Concatenation(
                ConcatenationProof::from_bytes(proof_bytes)?,
            )),
            #[cfg(feature = "future_snark")]
            AggregateSignatureType::Snark => Ok(AggregateSignature::Snark),
        }
    }

    /// If the aggregate signature is a concatenation proof, return it.
    pub fn to_concatenation_proof(&self) -> Option<&ConcatenationProof<D>> {
        match self {
            AggregateSignature::Concatenation(proof) => Some(proof),
            #[cfg(feature = "future_snark")]
            AggregateSignature::Snark => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    mod aggregate_signature_type_golden {
        use super::*;

        #[test]
        fn golden_bytes_encoding_prefix() {
            assert_eq!(
                0u8,
                AggregateSignatureType::Concatenation.get_byte_encoding_prefix()
            );
            assert_eq!(
                AggregateSignatureType::from_byte_encoding_prefix(0u8),
                Some(AggregateSignatureType::Concatenation)
            );
        }
    }

    mod aggregate_signature_golden_concatenation {
        use rand_chacha::ChaCha20Rng;
        use rand_core::SeedableRng;

        use super::{AggregateSignature, AggregateSignatureType};
        use crate::{
            Clerk, KeyRegistration, MithrilMembershipDigest, Parameters, RegistrationEntry, Signer,
            VerificationKeyProofOfPossessionForConcatenation,
            proof_system::ConcatenationProofSigner, signature_scheme::BlsSigningKey,
        };

        type D = MithrilMembershipDigest;

        const GOLDEN_JSON: &str = r#"
        {
            "signatures": [
                [
                {
                    "sigma": [
                    149, 157, 201, 187, 140, 54, 0, 128, 209, 88, 16, 203, 61, 78, 77, 98,
                    161, 133, 58, 152, 29, 74, 217, 113, 64, 100, 10, 161, 186, 167, 133,
                    114, 211, 153, 218, 56, 223, 84, 105, 242, 41, 54, 224, 170, 208, 185,
                    126, 83
                    ],
                    "indexes": [1, 4, 5, 8],
                    "signer_index": 0
                },
                [
                    [
                    143, 161, 255, 48, 78, 57, 204, 220, 25, 221, 164, 252, 248, 14, 56,
                    126, 186, 135, 228, 188, 145, 181, 52, 200, 97, 99, 213, 46, 0, 199,
                    193, 89, 187, 88, 29, 135, 173, 244, 86, 36, 83, 54, 67, 164, 6, 137,
                    94, 72, 6, 105, 128, 128, 93, 48, 176, 11, 4, 246, 138, 48, 180, 133,
                    90, 142, 192, 24, 193, 111, 142, 31, 76, 111, 110, 234, 153, 90, 208,
                    192, 31, 124, 95, 102, 49, 158, 99, 52, 220, 165, 94, 251, 68, 69,
                    121, 16, 224, 194
                    ],
                    1
                ]
                ],
                [
                {
                    "sigma": [
                    149, 169, 22, 201, 216, 97, 163, 188, 115, 210, 217, 236, 233, 161,
                    201, 13, 42, 132, 12, 63, 5, 31, 120, 22, 78, 177, 125, 134, 208, 205,
                    73, 58, 247, 141, 59, 62, 187, 81, 213, 30, 153, 218, 41, 42, 110,
                    156, 161, 205
                    ],
                    "indexes": [0, 3, 6],
                    "signer_index": 1
                },
                [
                    [
                    145, 56, 175, 32, 122, 187, 214, 226, 251, 148, 88, 9, 1, 103, 159,
                    146, 80, 166, 107, 243, 251, 236, 41, 28, 111, 128, 207, 164, 132,
                    147, 228, 83, 246, 228, 170, 68, 89, 78, 60, 28, 123, 130, 88, 234,
                    38, 97, 42, 65, 1, 100, 53, 18, 78, 131, 8, 61, 122, 131, 238, 84,
                    233, 223, 154, 118, 118, 73, 28, 27, 101, 78, 80, 233, 123, 206, 220,
                    174, 134, 205, 71, 110, 112, 180, 97, 98, 0, 113, 69, 145, 231, 168,
                    43, 173, 172, 56, 104, 208
                    ],
                    1
                ]
                ]
            ],
            "batch_proof": { "values": [], "indices": [0, 1], "hasher": null }
        }
        "#;

        fn golden_value() -> AggregateSignature<D> {
            let mut rng = ChaCha20Rng::from_seed([0u8; 32]);
            let msg = [0u8; 16];
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
            let entry1 = RegistrationEntry::new(pk_1, 1).unwrap();
            let entry2 = RegistrationEntry::new(pk_2, 1).unwrap();

            key_reg.register_by_entry(&entry1).unwrap();
            key_reg.register_by_entry(&entry2).unwrap();
            let closed_key_reg = key_reg.close_registration();

            let clerk = Clerk::new_clerk_from_closed_key_registration(&params, &closed_key_reg);

            let signer_1: Signer<D> = Signer::new(
                0,
                ConcatenationProofSigner::new(
                    1,
                    2,
                    params,
                    sk_1,
                    pk_1.vk,
                    closed_key_reg.clone().key_registration.into_merkle_tree().unwrap(),
                ),
                closed_key_reg.clone(),
                params,
                1,
            );

            let signer_2: Signer<D> = Signer::new(
                1,
                ConcatenationProofSigner::new(
                    1,
                    2,
                    params,
                    sk_2,
                    pk_2.vk,
                    closed_key_reg.clone().key_registration.into_merkle_tree().unwrap(),
                ),
                closed_key_reg.clone(),
                params,
                1,
            );
            let signature_1 = signer_1.create_single_signature(&msg).unwrap();
            let signature_2 = signer_2.create_single_signature(&msg).unwrap();

            clerk
                .aggregate_signatures_with_type(
                    &[signature_1, signature_2],
                    &msg,
                    AggregateSignatureType::Concatenation,
                )
                .unwrap()
        }

        #[test]
        fn golden_conversions() {
            let value: AggregateSignature<D> = serde_json::from_str(GOLDEN_JSON)
                .expect("This JSON deserialization should not fail");

            let serialized =
                serde_json::to_string(&value).expect("This JSON serialization should not fail");
            let golden_serialized = serde_json::to_string(&golden_value())
                .expect("This JSON serialization should not fail");
            assert_eq!(golden_serialized, serialized);
        }
    }
}
