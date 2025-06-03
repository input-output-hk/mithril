//! BLST Multi-signature module

pub(super) mod helper;
mod proof_of_possession;
mod signature;
mod signing_key;
mod verification_key;

pub use crate::bls_multi_signature::proof_of_possession::ProofOfPossession;
pub use crate::bls_multi_signature::signature::Signature;
pub use crate::bls_multi_signature::signing_key::SigningKey;
pub use crate::bls_multi_signature::verification_key::{VerificationKey, VerificationKeyPoP};

use serde::de::Visitor;
use serde::{Deserialize, Deserializer, Serialize, Serializer};

/// String used to generate the proofs of possession.
pub(crate) const POP: &[u8] = b"PoP";

// ---------------------------------------------------------------------
// Serde implementation
// ---------------------------------------------------------------------

macro_rules! impl_serde {
    ($st:ty,$visitor:ident,$size:expr) => {
        impl Serialize for $st {
            fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
            where
                S: Serializer,
            {
                use serde::ser::SerializeTuple;
                let mut seq = serializer.serialize_tuple($size)?;
                for e in self.to_bytes().iter() {
                    seq.serialize_element(e)?;
                }
                seq.end()
            }
        }

        impl<'de> Deserialize<'de> for $st {
            fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
            where
                D: Deserializer<'de>,
            {
                struct $visitor;

                impl<'de> Visitor<'de> for $visitor {
                    type Value = $st;

                    fn expecting(
                        &self,
                        formatter: &mut ::core::fmt::Formatter,
                    ) -> ::core::fmt::Result {
                        formatter
                            .write_str(format!("a multi signature {}", stringify!($st)).as_str())
                    }

                    fn visit_seq<A>(self, mut seq: A) -> Result<$st, A::Error>
                    where
                        A: serde::de::SeqAccess<'de>,
                    {
                        let mut bytes = [0u8; $size];
                        for i in 0..$size {
                            bytes[i] =
                                seq.next_element()?.ok_or(serde::de::Error::invalid_length(
                                    i,
                                    &format!("expected bytes{}", $size.to_string()).as_str(),
                                ))?;
                        }
                        <$st>::from_bytes(&bytes).map_err(|_| {
                            serde::de::Error::custom(
                                &format!("deserialization failed [{}]", stringify!($st)).as_str(),
                            )
                        })
                    }
                }

                deserializer.deserialize_tuple($size, $visitor)
            }
        }
    };
}
impl_serde!(SigningKey, SigningKeyVisitor, 32);
impl_serde!(VerificationKey, VerificationKeyVisitor, 96);
impl_serde!(ProofOfPossession, ProofOfPossessionVisitor, 96);
impl_serde!(Signature, SignatureVisitor, 48);

#[cfg(test)]
mod tests {
    use super::*;
    use crate::bls_multi_signature::helper::unsafe_helpers::{p1_affine_to_sig, p2_affine_to_vk};
    use crate::error::{MultiSignatureError, RegisterError};
    use crate::key_reg::KeyReg;
    use blst::{blst_p1, blst_p2};
    use proptest::prelude::*;
    use rand_chacha::ChaCha20Rng;
    use rand_core::SeedableRng;

    impl PartialEq for SigningKey {
        fn eq(&self, other: &Self) -> bool {
            self.to_blst_sk().to_bytes() == other.to_blst_sk().to_bytes()
        }
    }

    impl Eq for SigningKey {}

    proptest! {
        #![proptest_config(ProptestConfig::with_cases(1000))]

        #[test]
        fn test_sig(
            msg in prop::collection::vec(any::<u8>(), 1..128),
            seed in any::<[u8;32]>(),
        ) {
            let sk = SigningKey::gen(&mut ChaCha20Rng::from_seed(seed));
            let vk = VerificationKey::from(&sk);
            let sig = sk.sign(&msg);

            sig.verify(&msg, &vk).unwrap();
        }

        #[test]
        fn test_invalid_sig(msg in prop::collection::vec(any::<u8>(), 1..128), seed in any::<[u8;32]>()) {
            let mut rng = ChaCha20Rng::from_seed(seed);
            let sk1 = SigningKey::gen(&mut rng);
            let vk1 = VerificationKey::from(&sk1);
            let sk2 = SigningKey::gen(&mut rng);
            let fake_sig = sk2.sign(&msg);

            let result = fake_sig.verify(&msg, &vk1);
            assert_eq!(result, Err(MultiSignatureError::SignatureInvalid(fake_sig)));
        }

        #[test]
        fn test_infinity_sig(msg in prop::collection::vec(any::<u8>(), 1..128), seed in any::<[u8;32]>()) {
            let mut rng = ChaCha20Rng::from_seed(seed);
            let sk = SigningKey::gen(&mut rng);
            let vk = VerificationKey::from(&sk);

            let p1 = blst_p1::default();
            let sig_infinity = Signature(p1_affine_to_sig(&p1));

            let result = sig_infinity.verify(&msg, &vk);
            assert_eq!(result, Err(MultiSignatureError::SignatureInfinity(sig_infinity)));
        }

        #[test]
        fn test_infinity_vk(seed in any::<[u8;32]>()) {
            let mut rng = ChaCha20Rng::from_seed(seed);
            let sk = SigningKey::gen(&mut rng);
            let pop = ProofOfPossession::from(&sk);

            let p2 = blst_p2::default();
            let vk_infinity = VerificationKey(p2_affine_to_vk(&p2));
            let vkpop_infinity = VerificationKeyPoP { vk: vk_infinity, pop };

            let result = vkpop_infinity.check();
            assert_eq!(result, Err(MultiSignatureError::VerificationKeyInfinity(Box::new(vkpop_infinity.vk))));
        }

        #[test]
        fn test_keyreg_with_infinity_vk(num_sigs in 2..16usize, seed in any::<[u8;32]>()) {
            let mut rng = ChaCha20Rng::from_seed(seed);
            let mut kr = KeyReg::init();

            let sk = SigningKey::gen(&mut rng);
            let pop = ProofOfPossession::from(&sk);
            let p2 = blst_p2::default();
            let vk_infinity = VerificationKey(p2_affine_to_vk(&p2));
            let vkpop_infinity = VerificationKeyPoP { vk: vk_infinity, pop };

            for _ in 0..num_sigs {
                let sk = SigningKey::gen(&mut rng);
                let vkpop = VerificationKeyPoP::from(&sk);
                let _ = kr.register(1, vkpop);
            }

            let result = kr.register(1, vkpop_infinity);
            assert_eq!(result, Err(RegisterError::VerificationKeyInfinity(Box::new(vkpop_infinity.vk))));
        }

        #[test]
        fn test_aggregate_sig(msg in prop::collection::vec(any::<u8>(), 1..128),
                              num_sigs in 2..16,
                              seed in any::<[u8;32]>(),
        ) {
            let mut rng = ChaCha20Rng::from_seed(seed);
            let mut mvks = Vec::new();
            let mut sigs = Vec::new();
            for _ in 0..num_sigs {
                let sk = SigningKey::gen(&mut rng);
                let vk = VerificationKey::from(&sk);
                let sig = sk.sign(&msg);
                assert!(sig.verify(&msg, &vk).is_ok());
                sigs.push(sig);
                mvks.push(vk);
            }

            let result = Signature::verify_aggregate(&msg, &mvks, &sigs);
            assert!(result.is_ok(), "Aggregate verification failed {result:?}");
        }

        #[test]
        fn test_eval_sanity_check(msg in prop::collection::vec(any::<u8>(), 1..128),
                                  idx in any::<u64>(),
                                  seed in any::<[u8;32]>()) {
            let sk = SigningKey::gen(&mut ChaCha20Rng::from_seed(seed));
            let sig = sk.sign(&msg);
            sig.eval(&msg, idx);
        }

        #[test]
        fn serialize_deserialize_vk(seed in any::<u64>()) {
            let mut rng = rand_chacha::ChaCha8Rng::seed_from_u64(seed);
            let sk = SigningKey::gen(&mut rng);
            let vk = VerificationKey::from(&sk);
            let vk_bytes = vk.to_bytes();
            let vk2 = VerificationKey::from_bytes(&vk_bytes).unwrap();
            assert_eq!(vk, vk2);
            let vkpop = VerificationKeyPoP::from(&sk);
            let vkpop_bytes = vkpop.to_bytes();
            let vkpop2: VerificationKeyPoP = VerificationKeyPoP::from_bytes(&vkpop_bytes).unwrap();
            assert_eq!(vkpop, vkpop2);

            // Now we test serde
            let encoded = bincode::serde::encode_to_vec(vk, bincode::config::legacy()).unwrap();
            assert_eq!(encoded, vk_bytes);
            let (decoded,_) = bincode::serde::decode_from_slice::<VerificationKey,_>(&encoded, bincode::config::legacy()).unwrap();
            assert_eq!(vk, decoded);
            let encoded = bincode::serde::encode_to_vec(vkpop, bincode::config::legacy()).unwrap();
            let (decoded,_) = bincode::serde::decode_from_slice::<VerificationKeyPoP,_>(&encoded, bincode::config::legacy()).unwrap();
            assert_eq!(vkpop, decoded);
        }

        #[test]
        fn serialize_deserialize_sk(seed in any::<u64>()) {
            let mut rng = rand_chacha::ChaCha8Rng::seed_from_u64(seed);
            let sk = SigningKey::gen(&mut rng);
            let sk_bytes: [u8; 32] = sk.to_bytes();
            let sk2 = SigningKey::from_bytes(&sk_bytes).unwrap();
            assert_eq!(sk, sk2);

            // Now we test serde
            let encoded = bincode::serde::encode_to_vec(&sk, bincode::config::legacy()).unwrap();
            let (decoded,_) = bincode::serde::decode_from_slice::<SigningKey,_>(&encoded, bincode::config::legacy()).unwrap();
            assert_eq!(sk, decoded);

            // See that it is consistent with raw serialisation
            let (decoded_bytes,_) = bincode::serde::decode_from_slice::<SigningKey,_>(&sk_bytes, bincode::config::legacy()).unwrap();
            assert_eq!(sk, decoded_bytes);
        }

        #[test]
        fn batch_verify(num_batches in 2..10usize,
                              seed in any::<[u8;32]>(),
        ) {
            let mut rng = ChaCha20Rng::from_seed(seed);
            let num_sigs = 10;
            let mut batch_msgs = Vec::new();
            let mut batch_vk = Vec::new();
            let mut batch_sig = Vec::new();
            for _ in 0..num_batches {
                let mut msg = [0u8; 32];
                rng.fill_bytes(&mut msg);
                let mut mvks = Vec::new();
                let mut sigs = Vec::new();
                for _ in 0..num_sigs {
                    let sk = SigningKey::gen(&mut rng);
                    let vk = VerificationKey::from(&sk);
                    let sig = sk.sign(&msg);
                    sigs.push(sig);
                    mvks.push(vk);
                }
                assert!(Signature::verify_aggregate(&msg, &mvks, &sigs).is_ok());
                let (agg_vk, agg_sig) = Signature::aggregate(&mvks, &sigs).unwrap();
                batch_msgs.push(msg.to_vec());
                batch_vk.push(agg_vk);
                batch_sig.push(agg_sig);
            }
            assert!(Signature::batch_verify_aggregates(&batch_msgs, &batch_vk, &batch_sig).is_ok());

            // If we have an invalid signature, the batch verification will fail
            let mut msg = [0u8; 32];
            rng.fill_bytes(&mut msg);
            let sk = SigningKey::gen(&mut rng);
            let fake_sig = sk.sign(&msg);
            batch_sig[0] = fake_sig;

            let batch_result = Signature::batch_verify_aggregates(&batch_msgs, &batch_vk, &batch_sig);
            assert_eq!(batch_result, Err(MultiSignatureError::BatchInvalid));
        }
    }
}
