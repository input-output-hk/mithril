//! Base multisignature scheme, used as a primitive for STM.
//! See Section 2.4 of [the paper](https://eprint.iacr.org/2021/916).
//!

use super::stm::Index;

use crate::error::MultiSignatureError;
use blake2::{Blake2b, Digest};

use blstrs::*;
use ff::Field;
use group::prime::PrimeCurveAffine;
use group::Curve;
use group::Group;

// We use `min_sig` resulting in signatures of 48 bytes and public keys of
// 96. We can switch that around if desired by using `min_pk`.
// todo: Maybe we want this as a compilation flag?
// use blst::min_sig::{
//     AggregatePublicKey, AggregateSignature, PublicKey as BlstPk, SecretKey as BlstSk,
//     Signature as BlstSig,
// };
// use blst::{blst_p1, blst_p1_affine, blst_p1_compress, blst_p1_from_affine, blst_p1_uncompress, blst_p2_uncompress, blst_scalar_from_bendian, Pairing};

use rand_core::{CryptoRng, RngCore};
use serde::{de::Visitor, Deserialize, Deserializer, Serialize, Serializer};
use std::convert::TryInto;
use std::{
    cmp::Ordering,
    hash::{Hash, Hasher},
    iter::Sum,
    ops::Sub,
};

/// String used to generate the proofs of possession.
const POP: &[u8] = b"PoP";

// ---------------------------------------------------------------------
// Multi signature keys
// ---------------------------------------------------------------------

/// MultiSig secret key, which is a wrapper over the BlstSk type from the blst
/// library.
#[derive(Debug, Clone)]
pub struct SigningKey(Scalar);

impl SigningKey {
    /// Generate a secret key
    pub fn gen(rng: &mut (impl RngCore + CryptoRng)) -> Self {
        let mut ikm = [0u8; 32];
        rng.fill_bytes(&mut ikm);
        SigningKey(Scalar::random(rng))
    }

    /// Sign a message with the given secret key
    pub fn sign(&self, msg: &[u8]) -> Signature {
        Signature(G1Projective::hash_to_curve(msg, &[], &[]) * self.0)
    }

    /// Convert the secret key into byte string.
    pub fn to_bytes(&self) -> [u8; 32] {
        self.0.to_bytes_be()
    }

    /// Convert a string of bytes into a `SigningKey`.
    /// # Error
    /// Fails if the byte string represents a scalar larger than the group order.
    pub fn from_bytes(bytes: &[u8]) -> Result<Self, MultiSignatureError> {
        if bytes.len() < 32 {
            return Err(MultiSignatureError::SerializationError);
        }
        let mut array_bytes = [0u8; 32];
        array_bytes.copy_from_slice(&bytes[..32]);
        Ok(Self(Scalar::from_bytes_be(&array_bytes).unwrap()))
    }
}

/// MultiSig verification key, which iss a wrapper over the BlstPk (element in G2)
/// from the blst library.
#[derive(Debug, Clone, Copy)]
pub struct VerificationKey(G2Projective);

impl Default for VerificationKey {
    fn default() -> Self {
        VerificationKey(G2Projective::identity())
    }
}

impl Hash for VerificationKey {
    fn hash<H: Hasher>(&self, state: &mut H) {
        Hash::hash_slice(&self.to_bytes(), state)
    }
}

// We need to implement PartialEq instead of deriving it because we are implementing Hash.
impl PartialEq for VerificationKey {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl Eq for VerificationKey {}

impl VerificationKey {
    /// Convert an `VerificationKey` to its compressed byte representation.
    pub fn to_bytes(self) -> [u8; 96] {
        self.0.to_compressed()
    }

    /// Convert a compressed byte string into a `VerificationKey`.
    ///
    /// # Error
    /// This function fails if the bytes do not represent a compressed point of the curve.
    // todo: check that whether this checks that the point is in the prime order group.
    pub fn from_bytes(bytes: &[u8]) -> Result<Self, MultiSignatureError> {
        if bytes.len() < 96 {
            return Err(MultiSignatureError::SerializationError);
        }
        let mut array_bytes = [0u8; 96];
        array_bytes.copy_from_slice(&bytes[..96]);
        let key = if G2Projective::from_compressed(&array_bytes)
            .is_some()
            .unwrap_u8()
            == 1u8
        {
            G2Projective::from_compressed(&array_bytes).unwrap()
        } else {
            return Err(MultiSignatureError::SerializationError);
        };

        Ok(Self(key))
    }

    /// Compare two `VerificationKey`. Used for PartialOrd impl, used to order signatures. The comparison
    /// function can be anything, as long as it is consistent.
    fn cmp_msp_mvk(&self, other: &VerificationKey) -> Ordering {
        let self_bytes = self.to_bytes();
        let other_bytes = other.to_bytes();
        let mut result = Ordering::Equal;

        for (i, j) in self_bytes.iter().zip(other_bytes.iter()) {
            result = i.cmp(j);
            if result != Ordering::Equal {
                return result;
            }
        }

        result
    }
}

impl PartialOrd for VerificationKey {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp_msp_mvk(other))
    }
}

impl Ord for VerificationKey {
    fn cmp(&self, other: &Self) -> Ordering {
        self.cmp_msp_mvk(other)
    }
}

impl<'a> Sum<&'a Self> for VerificationKey {
    fn sum<I>(iter: I) -> Self
    where
        I: Iterator<Item = &'a Self>,
    {
        let mut start = G2Projective::identity();
        iter.map(|x| start += x.0);

        Self(start)
    }
}

impl<'a> Sum<&'a Self> for Signature {
    fn sum<I>(iter: I) -> Self
    where
        I: Iterator<Item = &'a Self>,
    {
        let mut start = G1Projective::identity();
        iter.map(|x| start += x.0);

        Self(start)
    }
}

/// MultiSig proof of possession, which contains two elements from G1. However,
/// the two elements have different types: `k1` is represented as a BlstSig
/// as it has the same structure, and this facilitates its verification. On
/// the other hand, `k2` is a G1 point, as it does not share structure with
/// the BLS signature, and we need to have an ad-hoc verification mechanism.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ProofOfPossession {
    k1: G1Projective,
    k2: G1Projective,
}

/// MultiSig public key, contains the verification key and the proof of possession.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct VerificationKeyPoP {
    /// The verification key.
    pub vk: VerificationKey,
    /// Proof of Possession.
    pub pop: ProofOfPossession,
}

impl From<&SigningKey> for VerificationKey {
    /// Convert a secret key into an `MspMvk`. This is performed by computing
    /// `MspMvk = g2 * sk`, where `g2` is the generator in G2. We can use the
    /// blst built-in function `sk_to_pk`.
    fn from(sk: &SigningKey) -> Self {
        VerificationKey(G2Affine::generator() * sk.0)
    }
}

// Again, unsafe code to access the algebraic operations.
// todo: particular care reviewing this (specially transmute)
impl From<&SigningKey> for ProofOfPossession {
    /// Convert a secret key into an `MspPoP`. This is performed by computing
    /// `k1 =  H_G1(b"PoP" || mvk) * sk` and `k2 = g1 * sk` where `H_G1` hashes into
    /// `G1` and `g1` is the generator in `G1`.
    fn from(sk: &SigningKey) -> Self {
        // use blst::{blst_scalar, blst_sk_to_pk_in_g1};
        let k1 = G1Projective::hash_to_curve(POP, &[], &[]) * sk.0;
        let k2 = G1Affine::generator() * sk.0;

        Self { k1, k2 }
    }
}

impl From<&SigningKey> for VerificationKeyPoP {
    /// Convert a secret key into an `MspPk` by simply converting to a
    /// `MspMvk` and `MspPoP`.
    fn from(sk: &SigningKey) -> Self {
        Self {
            vk: sk.into(),
            pop: sk.into(),
        }
    }
}

impl VerificationKeyPoP {
    /// if `e(k1,g2) = e(H_G1("PoP" || mvk),mvk)` and `e(g1,mvk) = e(k2,g2)`
    /// are both true, return 1. The first part is a signature verification
    /// of message "PoP", while the second we need to compute the pairing
    /// manually.
    // If we are really looking for performance improvements, we can combine the
    // two final exponantiations (for verifying k1 and k2) into a single one.
    pub fn check(&self) -> Result<(), MultiSignatureError> {
        let lhs_1 = pairing(&self.pop.k1.to_affine(), &G2Affine::generator());
        let rhs_1 = pairing(
            &G1Projective::hash_to_curve(POP, &[], &[]).to_affine(),
            &self.vk.0.to_affine(),
        );

        let lhs_2 = pairing(&G1Affine::generator(), &self.vk.0.to_affine());
        let rhs_2 = pairing(&self.pop.k2.to_affine(), &G2Affine::generator());

        if !(lhs_1 == rhs_1 && lhs_2 == rhs_2) {
            return Err(MultiSignatureError::InvalidKey(Box::new(*self)));
        }
        Ok(())
    }
    /// Convert to a 144 byte string.
    ///
    /// # Layout
    /// The layout of a `PublicKeyPoP` encoding is
    /// * Public key
    /// * Proof of Possession
    pub fn to_bytes(self) -> [u8; 192] {
        let mut pkpop_bytes = [0u8; 192];
        pkpop_bytes[..96].copy_from_slice(&self.vk.to_bytes());
        pkpop_bytes[96..].copy_from_slice(&self.pop.to_bytes());
        pkpop_bytes
    }

    /// Deserialise a byte string to a `PublicKeyPoP`.
    pub fn from_bytes(bytes: &[u8]) -> Result<Self, MultiSignatureError> {
        let mvk = VerificationKey::from_bytes(&bytes[..96])?;

        let pop = ProofOfPossession::from_bytes(&bytes[96..])?;

        Ok(Self { vk: mvk, pop })
    }
}

impl ProofOfPossession {
    /// Convert to a 96 byte string.
    ///
    /// # Layout
    /// The layout of a `MspPoP` encoding is
    /// * K1 (G1 point)
    /// * K2 (G1 point)
    pub fn to_bytes(self) -> [u8; 96] {
        let mut pop_bytes = [0u8; 96];
        pop_bytes[..48].copy_from_slice(&self.k1.to_compressed());
        pop_bytes[48..].copy_from_slice(&self.k2.to_compressed());
        pop_bytes
    }

    /// Deserialise a byte string to a `PublicKeyPoP`.
    pub fn from_bytes(bytes: &[u8]) -> Result<Self, MultiSignatureError> {
        let mut array_byte = [0u8; 48];
        array_byte.copy_from_slice(&bytes[..48]);
        let k1 = G1Projective::from_compressed(&array_byte).unwrap();

        array_byte.copy_from_slice(&bytes[48..]);
        let k2 = G1Projective::from_compressed(&array_byte).unwrap();

        Ok(Self { k1, k2 })
    }
}

// ---------------------------------------------------------------------
// Multi signature
// ---------------------------------------------------------------------

/// MultiSig signature, which is a wrapper over the `BlstSig` type.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Signature(G1Projective);

impl Signature {
    /// Verify a signature against a verification key.
    pub fn verify(&self, msg: &[u8], mvk: &VerificationKey) -> Result<(), MultiSignatureError> {
        let lhs = pairing(&self.0.to_affine(), &G2Affine::generator());
        let rhs = pairing(
            &G1Projective::hash_to_curve(msg, &[], &[]).to_affine(),
            &mvk.0.to_affine(),
        );
        if lhs == rhs {
            return Ok(());
        }

        Err(MultiSignatureError::InvalidSignature)
    }

    /// Check if the signature is valid for the given index. We hash the signature to produce a
    /// 64 bytes integer. We follow the same mechanism as Shelley
    /// for the lottery (i.e., we follow the VRF lottery mechanism as described in Section 16 of
    /// <https://hydra.iohk.io/build/8201171/download/1/ledger-spec.pdf>).
    // todo: if we are generic over the hash function, shouldn't we use the instance here?
    pub fn eval(&self, msg: &[u8], index: Index) -> [u8; 64] {
        let hasher = Blake2b::new()
            .chain(b"map")
            .chain(msg)
            .chain(&index.to_le_bytes())
            .chain(&self.to_bytes())
            .finalize();

        let mut output = [0u8; 64];
        output.copy_from_slice(hasher.as_slice());

        output
    }

    /// Convert an `Signature` to its compressed byte representation.
    pub fn to_bytes(self) -> [u8; 48] {
        self.0.to_compressed()
    }

    /// Convert a string of bytes into a `MspSig`.
    /// # Error
    /// Returns an error if the byte string does not represent a point in the curve.
    // todo: check that whether this checks that the point is in the prime order group.
    pub fn from_bytes(bytes: &[u8]) -> Result<Self, MultiSignatureError> {
        let mut array_byte = [0u8; 48];
        array_byte.copy_from_slice(&bytes[..48]);
        Ok(Self(G1Projective::from_compressed(&array_byte).unwrap()))
    }

    /// Compare two signatures. Used for PartialOrd impl, used to rank signatures. The comparison
    /// function can be anything, as long as it is consistent across different nodes.
    fn cmp_msp_sig(&self, other: &Self) -> Ordering {
        let self_bytes = self.to_bytes();
        let other_bytes = other.to_bytes();
        let mut result = Ordering::Equal;

        for (i, j) in self_bytes.iter().zip(other_bytes.iter()) {
            result = i.cmp(j);
            if result != Ordering::Equal {
                return result;
            }
        }
        result
    }
}

impl PartialOrd for Signature {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp_msp_sig(other))
    }
}

impl Ord for Signature {
    fn cmp(&self, other: &Self) -> Ordering {
        self.cmp_msp_sig(other)
    }
}

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
    use proptest::prelude::*;
    use rand_chacha::ChaCha20Rng;
    use rand_core::{OsRng, SeedableRng};

    impl PartialEq for SigningKey {
        fn eq(&self, other: &Self) -> bool {
            self.0.to_bytes_be() == other.0.to_bytes_be()
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
            let pk = VerificationKey::from(&sk);
            let sig = sk.sign(&msg);
            assert!(sig.verify(&msg, &pk).is_ok());
        }

        #[test]
        fn test_invalid_sig(msg in prop::collection::vec(any::<u8>(), 1..128),
                            seed in any::<[u8;32]>(),
        ) {
            let mut rng = ChaCha20Rng::from_seed(seed);
            let sk1 = SigningKey::gen(&mut rng);
            let pk1 = VerificationKey::from(&sk1);
            let sk2 = SigningKey::gen(&mut rng);
            let fake_sig = sk2.sign(&msg);
            assert!(fake_sig.verify(&msg, &pk1).is_err());
        }

        #[test]
        fn test_aggregate_sig(msg in prop::collection::vec(any::<u8>(), 1..128),
                              num_sigs in 1..16,
                              seed in any::<[u8;32]>(),
        ) {
            let mut rng = ChaCha20Rng::from_seed(seed);
            let mut mvks = Vec::new();
            let mut sigs = Vec::new();
            for _ in 0..num_sigs {
                let sk = SigningKey::gen(&mut rng);
                let pk = VerificationKey::from(&sk);
                let sig = sk.sign(&msg);
                assert!(sig.verify(&msg, &pk).is_ok());
                sigs.push(sig);
                mvks.push(pk);
            }
            let ivk = mvks.iter().sum();
            let mu: Signature = sigs.iter().sum();
            assert!(mu.verify(&msg, &ivk).is_ok());
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
        fn serialize_deserialize_pk(seed in any::<u64>()) {
            let mut rng = rand_chacha::ChaCha8Rng::seed_from_u64(seed);
            let sk = SigningKey::gen(&mut rng);
            let vk = VerificationKey::from(&sk);
            let vk_bytes = vk.to_bytes();
            let vk2 = VerificationKey::from_bytes(&vk_bytes).unwrap();
            assert_eq!(vk, vk2);
            let pk = VerificationKeyPoP::from(&sk);
            let pk_bytes = pk.to_bytes();
            let pk2: VerificationKeyPoP = VerificationKeyPoP::from_bytes(&pk_bytes).unwrap();
            assert_eq!(pk, pk2);

            // Now we test serde
            let encoded = bincode::serialize(&vk).unwrap();
            assert_eq!(encoded, vk_bytes);
            let decoded: VerificationKey = bincode::deserialize(&encoded).unwrap();
            assert_eq!(vk, decoded);
            let encoded = bincode::serialize(&pk).unwrap();
            let decoded: VerificationKeyPoP = bincode::deserialize(&encoded).unwrap();
            assert_eq!(pk, decoded);
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
    }

    #[test]
    fn test_gen() {
        for _ in 0..128 {
            let sk = SigningKey::gen(&mut OsRng);
            let pk = VerificationKeyPoP::from(&sk);
            assert!(pk.check().is_ok());
        }
    }
}
