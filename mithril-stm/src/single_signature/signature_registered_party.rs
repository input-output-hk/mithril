use blake2::digest::{Digest, FixedOutput};
use serde::{Deserialize, Serialize, Serializer, ser::SerializeTuple};

use crate::key_registration::RegisteredParty;
use crate::{SingleSignature, StmSignatureError};

/// Signature with its registered party.
#[derive(Debug, Clone, Hash, Deserialize, Eq, PartialEq, Ord, PartialOrd)]
pub struct SingleSignatureWithRegisteredParty {
    /// Stm signature
    pub sig: SingleSignature,
    /// Registered party
    pub reg_party: RegisteredParty,
}

impl SingleSignatureWithRegisteredParty {
    /// Convert `SingleSignatureWithRegisteredParty` to bytes
    /// # Layout
    /// * RegParty
    /// * Signature
    pub fn to_bytes(&self) -> Vec<u8> {
        let mut out = Vec::new();
        out.extend_from_slice(&self.reg_party.to_bytes());
        out.extend_from_slice(&self.sig.to_bytes());

        out
    }
    ///Extract a `SingleSignatureWithRegisteredParty` from a byte slice.
    pub fn from_bytes<D: Digest + Clone + FixedOutput>(
        bytes: &[u8],
    ) -> Result<SingleSignatureWithRegisteredParty, StmSignatureError> {
        let reg_party = RegisteredParty::from_bytes(
            bytes.get(0..104).ok_or(StmSignatureError::SerializationError)?,
        )?;
        let sig = SingleSignature::from_bytes::<D>(
            bytes.get(104..).ok_or(StmSignatureError::SerializationError)?,
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
        use blake2::{Blake2b, digest::consts::U32};
        use rand_chacha::ChaCha20Rng;
        use rand_core::SeedableRng;

        use crate::bls_multi_signature::{BlsSigningKey, BlsVerificationKeyProofOfPossession};
        use crate::{
            ClosedKeyRegistration, KeyRegistration, Parameters, Signer,
            SingleSignatureWithRegisteredParty,
        };

        type D = Blake2b<U32>;

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
            let pk_1 = BlsVerificationKeyProofOfPossession::from(&sk_1);
            let pk_2 = BlsVerificationKeyProofOfPossession::from(&sk_2);
            let mut key_reg = KeyRegistration::init();
            key_reg.register(1, pk_1).unwrap();
            key_reg.register(1, pk_2).unwrap();
            let closed_key_reg: ClosedKeyRegistration<D> = key_reg.close();
            let signer = Signer::set_signer(1, 1, params, sk_1, pk_1.vk, closed_key_reg.clone());
            let signature = signer.sign(&msg).unwrap();
            SingleSignatureWithRegisteredParty {
                sig: signature,
                reg_party: closed_key_reg.reg_parties[0],
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
