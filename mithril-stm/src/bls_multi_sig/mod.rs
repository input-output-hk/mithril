mod pop;
mod signature;
pub mod signing_key;
mod verification_key;
mod verification_key_pop;

pub use crate::bls_multi_sig::pop::ProofOfPossession;
pub use crate::bls_multi_sig::signature::Signature;
pub use crate::bls_multi_sig::signing_key::SigningKey;
pub use crate::bls_multi_sig::verification_key::VerificationKey;
pub use crate::bls_multi_sig::verification_key_pop::VerificationKeyPoP;
use crate::error::MultiSignatureError;

use blst::min_sig::{PublicKey as BlstVk, SecretKey as BlstSk, Signature as BlstSig};
use blst::{blst_p1, blst_p2};
use serde::de::Visitor;
use serde::{Deserialize, Deserializer, Serialize, Serializer};

/// String used to generate the proofs of possession.
const POP: &[u8] = b"PoP";

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

// ---------------------------------------------------------------------
// Unsafe helpers
// ---------------------------------------------------------------------

mod unsafe_helpers {
    use super::*;
    use crate::error::MultiSignatureError::SerializationError;
    use blst::{
        blst_fp12, blst_fp12_finalverify, blst_p1_affine, blst_p1_affine_generator,
        blst_p1_compress, blst_p1_from_affine, blst_p1_to_affine, blst_p1_uncompress,
        blst_p2_affine, blst_p2_affine_generator, blst_p2_from_affine, blst_p2_to_affine,
        blst_scalar, blst_sk_to_pk_in_g1,
    };

    /// Check manually if the pairing `e(g1,mvk) = e(k2,g2)` holds.
    pub(crate) fn verify_pairing(vk: &VerificationKey, pop: &ProofOfPossession) -> bool {
        unsafe {
            let g1_p = *blst_p1_affine_generator();
            let mvk_p = std::mem::transmute::<BlstVk, blst_p2_affine>(vk.to_blst_vk());
            let ml_lhs = blst_fp12::miller_loop(&mvk_p, &g1_p);

            let mut k2_p = blst_p1_affine::default();
            blst_p1_to_affine(&mut k2_p, &pop.to_k2());
            let g2_p = *blst_p2_affine_generator();
            let ml_rhs = blst_fp12::miller_loop(&g2_p, &k2_p);

            blst_fp12_finalverify(&ml_lhs, &ml_rhs)
        }
    }

    pub(crate) fn compress_p1(k2: &blst_p1) -> [u8; 48] {
        let mut bytes = [0u8; 48];
        unsafe { blst_p1_compress(bytes.as_mut_ptr(), k2) }
        bytes
    }

    pub(crate) fn uncompress_p1(bytes: &[u8]) -> Result<blst_p1, MultiSignatureError> {
        unsafe {
            if bytes.len() == 48 {
                let mut point = blst_p1_affine::default();
                let mut out = blst_p1::default();
                blst_p1_uncompress(&mut point, bytes.as_ptr());
                blst_p1_from_affine(&mut out, &point);
                Ok(out)
            } else {
                Err(SerializationError)
            }
        }
    }

    pub(crate) fn scalar_to_pk_in_g1(sk: &BlstSk) -> blst_p1 {
        unsafe {
            let sk_scalar = std::mem::transmute::<&BlstSk, &blst_scalar>(sk);
            let mut out = blst_p1::default();
            blst_sk_to_pk_in_g1(&mut out, sk_scalar);
            out
        }
    }

    pub(crate) fn vk_from_p2_affine(vk: &VerificationKey) -> blst_p2 {
        unsafe {
            let mut projective_p2 = blst_p2::default();
            blst_p2_from_affine(
                &mut projective_p2,
                &std::mem::transmute::<BlstVk, blst_p2_affine>(vk.to_blst_vk()),
            );
            projective_p2
        }
    }

    pub(crate) fn sig_to_p1(sig: &BlstSig) -> blst_p1 {
        unsafe {
            let mut projective_p1 = blst_p1::default();
            blst_p1_from_affine(
                &mut projective_p1,
                &std::mem::transmute::<BlstSig, blst_p1_affine>(*sig),
            );
            projective_p1
        }
    }

    pub(crate) fn p2_affine_to_vk(grouped_vks: &blst_p2) -> BlstVk {
        unsafe {
            let mut affine_p2 = blst_p2_affine::default();
            blst_p2_to_affine(&mut affine_p2, grouped_vks);
            std::mem::transmute::<blst_p2_affine, BlstVk>(affine_p2)
        }
    }

    pub(crate) fn p1_affine_to_sig(grouped_sigs: &blst_p1) -> BlstSig {
        unsafe {
            let mut affine_p1 = blst_p1_affine::default();
            blst_p1_to_affine(&mut affine_p1, grouped_sigs);
            std::mem::transmute::<blst_p1_affine, BlstSig>(affine_p1)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::bls_multi_sig::unsafe_helpers::{p1_affine_to_sig, p2_affine_to_vk};
    use crate::error::RegisterError;
    use crate::key_reg::KeyReg;
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
            let encoded = bincode::serialize(&vk).unwrap();
            assert_eq!(encoded, vk_bytes);
            let decoded: VerificationKey = bincode::deserialize(&encoded).unwrap();
            assert_eq!(vk, decoded);
            let encoded = bincode::serialize(&vkpop).unwrap();
            let decoded: VerificationKeyPoP = bincode::deserialize(&encoded).unwrap();
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
            let encoded = bincode::serialize(&sk).unwrap();
            let decoded: SigningKey = bincode::deserialize(&encoded).unwrap();
            assert_eq!(sk, decoded);

            // See that it is consistent with raw serialisation
            let decoded_bytes: SigningKey = bincode::deserialize(&sk_bytes).unwrap();
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
