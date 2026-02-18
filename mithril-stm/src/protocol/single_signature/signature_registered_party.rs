use serde::{Deserialize, Serialize, Serializer, ser::SerializeTuple};

use crate::{ClosedRegistrationEntry, MembershipDigest, StmResult};

use super::{SignatureError, SingleSignature};

/// Signature with its registered party.
#[derive(Debug, Clone, Hash, Deserialize, Eq, PartialEq, Ord, PartialOrd)]
pub struct SingleSignatureWithRegisteredParty {
    /// Stm signature
    pub sig: SingleSignature,
    /// Registered party
    pub reg_party: ClosedRegistrationEntry,
}

impl SingleSignatureWithRegisteredParty {
    /// Convert `SingleSignatureWithRegisteredParty` to bytes
    /// # Layout
    /// * RegParty length (u64 big-endian)
    /// * RegParty
    /// * Signature length (u64 big-endian)
    /// * Signature
    pub fn to_bytes(&self) -> Vec<u8> {
        let mut out = Vec::new();
        let reg_party_bytes = self.reg_party.to_bytes();
        out.extend_from_slice(&(reg_party_bytes.len() as u64).to_be_bytes());
        out.extend_from_slice(&reg_party_bytes);
        let sig_bytes = self.sig.to_bytes();
        out.extend_from_slice(&(sig_bytes.len() as u64).to_be_bytes());
        out.extend_from_slice(&sig_bytes);
        out
    }
    /// Extract a `SingleSignatureWithRegisteredParty` from a byte slice.
    pub fn from_bytes<D: MembershipDigest>(
        bytes: &[u8],
    ) -> StmResult<SingleSignatureWithRegisteredParty> {
        let mut u64_bytes = [0u8; 8];

        u64_bytes.copy_from_slice(bytes.get(0..8).ok_or(SignatureError::SerializationError)?);
        let size_reg_party = u64::from_be_bytes(u64_bytes) as usize;
        let reg_party = ClosedRegistrationEntry::from_bytes(
            bytes
                .get(8..8 + size_reg_party)
                .ok_or(SignatureError::SerializationError)?,
        )?;

        let sig_offset = 8 + size_reg_party;
        u64_bytes.copy_from_slice(
            bytes
                .get(sig_offset..sig_offset + 8)
                .ok_or(SignatureError::SerializationError)?,
        );
        let size_sig = u64::from_be_bytes(u64_bytes) as usize;
        let sig = SingleSignature::from_bytes::<D>(
            bytes
                .get(sig_offset + 8..sig_offset + 8 + size_sig)
                .ok_or(SignatureError::SerializationError)?,
        )?;

        Ok(SingleSignatureWithRegisteredParty { sig, reg_party })
    }
}

impl Serialize for SingleSignatureWithRegisteredParty {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut tuple = serializer.serialize_tuple(2)?;
        tuple.serialize_element(&self.sig)?;
        tuple.serialize_element(&self.reg_party)?;
        tuple.end()
    }
}

#[cfg(test)]
mod tests {
    mod golden {
        use rand_chacha::ChaCha20Rng;
        use rand_core::SeedableRng;

        use crate::{
            ClosedKeyRegistration, KeyRegistration, MithrilMembershipDigest, Parameters,
            RegistrationEntry, SingleSignature, SingleSignatureWithRegisteredParty,
            VerificationKeyProofOfPossessionForConcatenation,
            proof_system::ConcatenationProofSigner, signature_scheme::BlsSigningKey,
        };
        #[cfg(feature = "future_snark")]
        use crate::{
            ClosedRegistrationEntry, MembershipDigest, VerificationKeyForSnark,
            proof_system::SnarkProofSigner, protocol::RegistrationEntryForSnark,
            signature_scheme::SchnorrSigningKey,
        };

        type D = MithrilMembershipDigest;

        #[cfg(not(feature = "future_snark"))]
        const GOLDEN_JSON: &str = r#"
        [
            {
                "sigma": [
                149, 157, 201, 187, 140, 54, 0, 128, 209, 88, 16, 203, 61, 78, 77, 98,
                161, 133, 58, 152, 29, 74, 217, 113, 64, 100, 10, 161, 186, 167, 133, 114,
                211, 153, 218, 56, 223, 84, 105, 242, 41, 54, 224, 170, 208, 185, 126, 83
                ],
                "indexes": [1, 4, 5, 8],
                "signer_index": 1
            },
            [
                [
                143, 161, 255, 48, 78, 57, 204, 220, 25, 221, 164, 252, 248, 14, 56, 126,
                186, 135, 228, 188, 145, 181, 52, 200, 97, 99, 213, 46, 0, 199, 193, 89,
                187, 88, 29, 135, 173, 244, 86, 36, 83, 54, 67, 164, 6, 137, 94, 72, 6,
                105, 128, 128, 93, 48, 176, 11, 4, 246, 138, 48, 180, 133, 90, 142, 192,
                24, 193, 111, 142, 31, 76, 111, 110, 234, 153, 90, 208, 192, 31, 124, 95,
                102, 49, 158, 99, 52, 220, 165, 94, 251, 68, 69, 121, 16, 224, 194
                ],
                1
            ]
        ]
        "#;

        #[cfg(feature = "future_snark")]
        const GOLDEN_JSON: &str = r#"
        [
            {
                "sigma": [
                    149, 157, 201, 187, 140, 54, 0, 128, 209, 88, 16, 203, 61, 78, 77, 98,
                    161, 133, 58, 152, 29, 74, 217, 113, 64, 100, 10, 161, 186, 167, 133, 114,
                    211, 153, 218, 56, 223, 84, 105, 242, 41, 54, 224, 170, 208, 185, 126, 83
                ],
                "indexes": [1, 4, 5, 8],
                "signer_index": 1,
                "snark_signature": {
                    "schnorr_signature": {
                        "commitment_point": [
                            38, 159, 207, 207, 130, 159, 24, 165, 64, 2, 139, 15, 69, 205, 101,
                            166, 100, 45, 22, 225, 113, 161, 32, 186, 193, 17, 159, 158, 47,
                            139, 78, 169
                        ],
                        "response": [
                            7, 184, 24, 233, 2, 217, 182, 50, 37, 59, 170, 168, 201, 44, 166,
                            4, 116, 226, 215, 37, 101, 8, 124, 0, 194, 124, 216, 214, 3, 145,
                            255, 3
                        ],
                        "challenge": [
                            56, 236, 11, 143, 28, 124, 255, 116, 177, 19, 148, 255, 229, 226,
                            85, 103, 170, 181, 124, 105, 71, 188, 44, 85, 205, 222, 230, 116,
                            210, 97, 8, 22
                        ]
                    },
                    "minimum_winning_lottery_index": 0
                }
            },
            [
                [
                    143, 161, 255, 48, 78, 57, 204, 220, 25, 221, 164, 252, 248, 14, 56, 126,
                    186, 135, 228, 188, 145, 181, 52, 200, 97, 99, 213, 46, 0, 199, 193, 89,
                    187, 88, 29, 135, 173, 244, 86, 36, 83, 54, 67, 164, 6, 137, 94, 72, 6,
                    105, 128, 128, 93, 48, 176, 11, 4, 246, 138, 48, 180, 133, 90, 142, 192,
                    24, 193, 111, 142, 31, 76, 111, 110, 234, 153, 90, 208, 192, 31, 124, 95,
                    102, 49, 158, 99, 52, 220, 165, 94, 251, 68, 69, 121, 16, 224, 194
                ],
                1,
                [
                    228, 235, 159, 243, 100, 74, 143, 74, 193, 127, 170, 87, 102, 1, 10, 220,
                    173, 141, 223, 53, 86, 104, 169, 168, 82, 136, 67, 233, 108, 18, 229, 93
                ],
                [
                    0, 0, 0, 0, 255, 255, 255, 255, 254, 91, 254, 255, 2, 164, 189, 83, 5,
                    216, 161, 9, 8, 216, 57, 51, 72, 125, 157, 41, 83, 167, 237, 115
                ]
            ]
        ]
        "#;

        fn golden_value() -> SingleSignatureWithRegisteredParty {
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

            let mut key_reg = KeyRegistration::initialize();
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
            key_reg.register_by_entry(&entry1).unwrap();
            key_reg.register_by_entry(&entry2).unwrap();
            let closed_key_reg: ClosedKeyRegistration = key_reg.close_registration();
            let total_stake = closed_key_reg.total_stake;

            let concatenation_proof_signer: ConcatenationProofSigner<D> =
                ConcatenationProofSigner::new(
                    1,
                    2,
                    params,
                    sk_1,
                    pk_1.vk,
                    closed_key_reg.to_merkle_tree(),
                );
            let concatenation_signature =
                concatenation_proof_signer.create_single_signature(&msg).unwrap();

            #[cfg(feature = "future_snark")]
            let snark_signature = {
                let key_registration_commitment = closed_key_reg
                    .to_merkle_tree::<<D as MembershipDigest>::SnarkHash, RegistrationEntryForSnark>(
                );
                let closed_registration_entry =
                    ClosedRegistrationEntry::from((entry1, closed_key_reg.total_stake));
                let lottery_target_value =
                    closed_registration_entry.get_lottery_target_value().unwrap();
                let snark_proof_signer = SnarkProofSigner::<D>::new(
                    params,
                    schnorr_sk_1,
                    schnorr_vk_1,
                    lottery_target_value,
                    key_registration_commitment,
                );
                Some(snark_proof_signer.create_single_signature(&msg, &mut rng).unwrap())
            };

            let signature = SingleSignature {
                concatenation_signature,
                signer_index: 1,
                #[cfg(feature = "future_snark")]
                snark_signature,
            };

            SingleSignatureWithRegisteredParty {
                sig: signature,
                reg_party: (entry1, total_stake).into(),
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
