//! Base multi-signature scheme, used as a primitive for STM.
//! See Section 2.4 of [the paper](https://eprint.iacr.org/2021/916).
//! This module uses the `blst` library as a backend for pairings.

use crate::error::{blst_err_to_mithril, MultiSignatureError};
use crate::stm::Index;
use blake2::{digest::consts::U16, Blake2b, Blake2b512, Digest};
use unsafe_helpers::*;

// We use `min_sig` resulting in signatures of 48 bytes and public keys of
// 96. We can switch that around if desired by using `min_vk`.
use blst::min_sig::{
    AggregatePublicKey, AggregateSignature, PublicKey as BlstVk, SecretKey as BlstSk,
    Signature as BlstSig,
};
use blst::{blst_p1, blst_p2, p1_affines, p2_affines, BLST_ERROR};

use rand_core::{CryptoRng, RngCore};
use serde::{de::Visitor, Deserialize, Deserializer, Serialize, Serializer};
use std::{
    cmp::Ordering,
    fmt::{Display, Formatter},
    hash::{Hash, Hasher},
    iter::Sum,
};
/// String used to generate the proofs of possession.
const POP: &[u8] = b"PoP";

/// MultiSig secret key, which is a wrapper over the BlstSk type from the blst
/// library.
#[derive(Debug, Clone)]
pub struct SigningKey(BlstSk);

impl SigningKey {
    /// Generate a secret key
    pub fn gen(rng: &mut (impl RngCore + CryptoRng)) -> Self {
        let mut ikm = [0u8; 32];
        rng.fill_bytes(&mut ikm);
        SigningKey(
            BlstSk::key_gen(&ikm, &[])
                .expect("Error occurs when the length of ikm < 32. This will not happen here."),
        )
    }

    /// Sign a message with the given secret key
    pub fn sign(&self, msg: &[u8]) -> Signature {
        Signature(self.0.sign(msg, &[], &[]))
    }

    /// Convert the secret key into byte string.
    pub fn to_bytes(&self) -> [u8; 32] {
        self.0.to_bytes()
    }

    /// Convert a string of bytes into a `SigningKey`.
    ///
    /// # Error
    /// Fails if the byte string represents a scalar larger than the group order.
    pub fn from_bytes(bytes: &[u8]) -> Result<Self, MultiSignatureError> {
        match BlstSk::from_bytes(&bytes[..32]) {
            Ok(sk) => Ok(Self(sk)),
            Err(e) => Err(blst_err_to_mithril(e, None, None)
                .expect_err("If deserialization is not successful, blst returns and error different to SUCCESS."))
        }
    }
}

/// MultiSig verification key, which is a wrapper over the BlstVk (element in G2)
/// from the blst library.
#[derive(Debug, Clone, Copy, Default)]
pub struct VerificationKey(BlstVk);

impl VerificationKey {
    /// Convert an `VerificationKey` to its compressed byte representation.
    pub fn to_bytes(self) -> [u8; 96] {
        self.0.to_bytes()
    }

    /// Convert a compressed byte string into a `VerificationKey`.
    ///
    /// # Error
    /// This function fails if the bytes do not represent a compressed point of the prime
    /// order subgroup of the curve Bls12-381.
    pub fn from_bytes(bytes: &[u8]) -> Result<Self, MultiSignatureError> {
        match BlstVk::key_validate(&bytes[..96]) {
            Ok(vk) => Ok(Self(vk)),
            Err(e) => Err(blst_err_to_mithril(e, None, None)
                .expect_err("If deserialization is not successful, blst returns and error different to SUCCESS."))
        }
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

impl Display for VerificationKey {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.to_bytes())
    }
}

impl Hash for VerificationKey {
    fn hash<H: Hasher>(&self, state: &mut H) {
        Hash::hash_slice(&self.to_bytes(), state)
    }
}

impl PartialEq for VerificationKey {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl Eq for VerificationKey {}

impl PartialOrd for VerificationKey {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(std::cmp::Ord::cmp(self, other))
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
        let keys: Vec<&BlstVk> = iter.map(|x| &x.0).collect();

        assert!(!keys.is_empty(), "One cannot add an empty vector");
        let aggregate_key = AggregatePublicKey::aggregate(&keys, false)
            .expect("An MspMvk is always a valid key. This function only fails if keys is empty or if the keys are invalid, none of which can happen.")
            .to_public_key();

        Self(aggregate_key)
    }
}

impl From<&SigningKey> for VerificationKey {
    /// Convert a secret key into an `MspMvk`. This is performed by computing
    /// `MspMvk = g2 * sk`, where `g2` is the generator in G2. We can use the
    /// blst built-in function `sk_to_pk`.
    fn from(sk: &SigningKey) -> Self {
        VerificationKey(sk.0.sk_to_pk())
    }
}

/// MultiSig proof of possession, which contains two elements from G1. However,
/// the two elements have different types: `k1` is represented as a BlstSig
/// as it has the same structure, and this facilitates its verification. On
/// the other hand, `k2` is a G1 point, as it does not share structure with
/// the BLS signature, and we need to have an ad-hoc verification mechanism.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ProofOfPossession {
    k1: BlstSig,
    k2: blst_p1,
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
        pop_bytes[..48].copy_from_slice(&self.k1.to_bytes());

        pop_bytes[48..].copy_from_slice(&compress_p1(&self.k2)[..]);
        pop_bytes
    }

    /// Deserialize a byte string to a `PublicKeyPoP`.
    pub fn from_bytes(bytes: &[u8]) -> Result<Self, MultiSignatureError> {
        let k1 = match BlstSig::from_bytes(&bytes[..48]) {
            Ok(key) => key,
            Err(e) => {
                return Err(blst_err_to_mithril(e, None, None)
                    .expect_err("If it passed, blst returns and error different to SUCCESS."))
            }
        };

        let k2 = uncompress_p1(&bytes[48..96])?;

        Ok(Self { k1, k2 })
    }
}

impl From<&SigningKey> for ProofOfPossession {
    /// Convert a secret key into an `MspPoP`. This is performed by computing
    /// `k1 =  H_G1(b"PoP" || mvk)` and `k2 = g1 * sk` where `H_G1` hashes into
    /// `G1` and `g1` is the generator in `G1`.
    fn from(sk: &SigningKey) -> Self {
        let k1 = sk.0.sign(POP, &[], &[]);
        let k2 = scalar_to_pk_in_g1(sk);

        Self { k1, k2 }
    }
}

/// MultiSig public key, contains the verification key and the proof of possession.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct VerificationKeyPoP {
    /// The verification key.
    pub vk: VerificationKey,
    /// Proof of Possession.
    pub pop: ProofOfPossession,
}

impl VerificationKeyPoP {
    /// if `e(k1,g2) = e(H_G1("PoP" || mvk),mvk)` and `e(g1,mvk) = e(k2,g2)`
    /// are both true, return 1. The first part is a signature verification
    /// of message "PoP", while the second we need to compute the pairing
    /// manually.
    // If we are really looking for performance improvements, we can combine the
    // two final exponentiations (for verifying k1 and k2) into a single one.
    pub fn check(&self) -> Result<(), MultiSignatureError> {
        match self.vk.0.validate() {
            Ok(_) => {
                let result = verify_pairing(&self.vk, &self.pop);
                if !(self.pop.k1.verify(false, POP, &[], &[], &self.vk.0, false)
                    == BLST_ERROR::BLST_SUCCESS
                    && result)
                {
                    return Err(MultiSignatureError::KeyInvalid(Box::new(*self)));
                }
                Ok(())
            }
            Err(e) => blst_err_to_mithril(e, None, Some(self.vk)),
        }
    }

    /// Convert to a 144 byte string.
    ///
    /// # Layout
    /// The layout of a `PublicKeyPoP` encoding is
    /// * Public key
    /// * Proof of Possession
    pub fn to_bytes(self) -> [u8; 192] {
        let mut vkpop_bytes = [0u8; 192];
        vkpop_bytes[..96].copy_from_slice(&self.vk.to_bytes());
        vkpop_bytes[96..].copy_from_slice(&self.pop.to_bytes());
        vkpop_bytes
    }

    /// Deserialize a byte string to a `PublicKeyPoP`.
    pub fn from_bytes(bytes: &[u8]) -> Result<Self, MultiSignatureError> {
        let mvk = VerificationKey::from_bytes(&bytes[..96])?;

        let pop = ProofOfPossession::from_bytes(&bytes[96..])?;

        Ok(Self { vk: mvk, pop })
    }
}

impl From<&SigningKey> for VerificationKeyPoP {
    /// Convert a secret key into a `VerificationKeyPoP` by simply converting to a
    /// `MspMvk` and `MspPoP`.
    fn from(sk: &SigningKey) -> Self {
        Self {
            vk: sk.into(),
            pop: sk.into(),
        }
    }
}

/// MultiSig signature, which is a wrapper over the `BlstSig` type.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Signature(BlstSig);

impl Signature {
    /// Verify a signature against a verification key.
    pub fn verify(&self, msg: &[u8], mvk: &VerificationKey) -> Result<(), MultiSignatureError> {
        blst_err_to_mithril(
            self.0.validate(true).map_or_else(
                |e| e,
                |_| self.0.verify(false, msg, &[], &[], &mvk.0, false),
            ),
            Some(*self),
            None,
        )
    }

    /// Dense mapping function indexed by the index to be evaluated.
    /// We hash the signature to produce a 64 bytes integer.
    /// The return value of this function refers to
    /// `ev = H("map" || msg || index || σ) <- MSP.Eval(msg,index,σ)` given in paper.
    pub fn eval(&self, msg: &[u8], index: Index) -> [u8; 64] {
        let hasher = Blake2b512::new()
            .chain_update(b"map")
            .chain_update(msg)
            .chain_update(index.to_le_bytes())
            .chain_update(self.to_bytes())
            .finalize();

        let mut output = [0u8; 64];
        output.copy_from_slice(hasher.as_slice());

        output
    }

    /// Convert an `Signature` to its compressed byte representation.
    pub fn to_bytes(self) -> [u8; 48] {
        self.0.to_bytes()
    }

    /// Convert a string of bytes into a `MspSig`.
    ///
    /// # Error
    /// Returns an error if the byte string does not represent a point in the curve.
    pub fn from_bytes(bytes: &[u8]) -> Result<Self, MultiSignatureError> {
        match BlstSig::sig_validate(&bytes[..48], true) {
            Ok(sig) => Ok(Self(sig)),
            Err(e) => Err(blst_err_to_mithril(e, None, None)
                .expect_err("If deserialization is not successful, blst returns and error different to SUCCESS."))
        }
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

    /// Aggregate a slice of verification keys and Signatures by first hashing the
    /// signatures into random scalars, and multiplying the signature and verification
    /// key with the resulting value. This follows the steps defined in Figure 6,
    /// `Aggregate` step.
    pub fn aggregate(
        vks: &[VerificationKey],
        sigs: &[Signature],
    ) -> Result<(VerificationKey, Signature), MultiSignatureError> {
        if vks.len() != sigs.len() || vks.is_empty() {
            return Err(MultiSignatureError::AggregateSignatureInvalid);
        }

        if vks.len() < 2 {
            return Ok((vks[0], sigs[0]));
        }

        let mut hashed_sigs = Blake2b::<U16>::new();
        for sig in sigs {
            hashed_sigs.update(sig.to_bytes());
        }

        // First we generate the scalars
        let mut scalars = Vec::with_capacity(vks.len() * 128);
        let mut signatures = Vec::with_capacity(vks.len());
        for (index, sig) in sigs.iter().enumerate() {
            let mut hasher = hashed_sigs.clone();
            hasher.update(index.to_be_bytes());
            signatures.push(sig.0);
            scalars.extend_from_slice(hasher.finalize().as_slice());
        }

        let transmuted_vks: Vec<blst_p2> = vks.iter().map(vk_from_p2_affine).collect();
        let transmuted_sigs: Vec<blst_p1> = signatures.iter().map(sig_to_p1).collect();

        let grouped_vks = p2_affines::from(transmuted_vks.as_slice());
        let grouped_sigs = p1_affines::from(transmuted_sigs.as_slice());

        let aggr_vk: BlstVk = p2_affine_to_vk(&grouped_vks.mult(&scalars, 128));
        let aggr_sig: BlstSig = p1_affine_to_sig(&grouped_sigs.mult(&scalars, 128));

        Ok((VerificationKey(aggr_vk), Signature(aggr_sig)))
    }

    /// Verify a set of signatures with their corresponding verification keys using the
    /// aggregation mechanism of Figure 6.
    pub fn verify_aggregate(
        msg: &[u8],
        vks: &[VerificationKey],
        sigs: &[Signature],
    ) -> Result<(), MultiSignatureError> {
        let (aggr_vk, aggr_sig) = Self::aggregate(vks, sigs)?;

        blst_err_to_mithril(
            aggr_sig.0.verify(false, msg, &[], &[], &aggr_vk.0, false),
            Some(aggr_sig),
            None,
        )
    }

    /// Batch verify several sets of signatures with their corresponding verification keys.
    #[cfg(feature = "batch-verify-aggregates")]
    pub fn batch_verify_aggregates(
        msgs: &[Vec<u8>],
        vks: &[VerificationKey],
        sigs: &[Signature],
    ) -> Result<(), MultiSignatureError> {
        let batched_sig: BlstSig = match AggregateSignature::aggregate(
            &(sigs.iter().map(|sig| &sig.0).collect::<Vec<&BlstSig>>()),
            false,
        ) {
            Ok(sig) => BlstSig::from_aggregate(&sig),
            Err(e) => return blst_err_to_mithril(e, None, None),
        };

        let p2_vks: Vec<&BlstVk> = vks.iter().map(|vk| &vk.0).collect();
        let slice_msgs = msgs
            .iter()
            .map(|msg| msg.as_slice())
            .collect::<Vec<&[u8]>>();

        blst_err_to_mithril(
            batched_sig.aggregate_verify(false, &slice_msgs, &[], &p2_vks, false),
            None,
            None,
        )
        .map_err(|_| MultiSignatureError::BatchInvalid)
    }
}

impl<'a> Sum<&'a Self> for Signature {
    fn sum<I>(iter: I) -> Self
    where
        I: Iterator<Item = &'a Self>,
    {
        let signatures: Vec<&BlstSig> = iter.map(|x| &x.0).collect();
        assert!(!signatures.is_empty(), "One cannot add an empty vector");
        let aggregate = AggregateSignature::aggregate(&signatures, false)
            .expect("An MspSig is always a valid signature. This function only fails if signatures is empty or if the signatures are invalid, none of which can happen.")
            .to_signature();

        Self(aggregate)
    }
}

impl PartialOrd for Signature {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(std::cmp::Ord::cmp(self, other))
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
            let mvk_p = std::mem::transmute::<BlstVk, blst_p2_affine>(vk.0);
            let ml_lhs = blst_fp12::miller_loop(&mvk_p, &g1_p);

            let mut k2_p = blst_p1_affine::default();
            blst_p1_to_affine(&mut k2_p, &pop.k2);
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

    pub(crate) fn scalar_to_pk_in_g1(sk: &SigningKey) -> blst_p1 {
        unsafe {
            let sk_scalar = std::mem::transmute::<&BlstSk, &blst_scalar>(&sk.0);
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
                &std::mem::transmute::<BlstVk, blst_p2_affine>(vk.0),
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
    use crate::error::RegisterError;
    use crate::key_reg::KeyReg;
    use proptest::prelude::*;
    use rand_chacha::ChaCha20Rng;
    use rand_core::SeedableRng;

    // ---------------------------------------------------------------------
    // Test helpers
    // ---------------------------------------------------------------------
    impl PartialEq for SigningKey {
        fn eq(&self, other: &Self) -> bool {
            self.0.to_bytes() == other.0.to_bytes()
        }
    }

    impl Eq for SigningKey {}

    // ---------------------------------------------------------------------
    // Property test: `test_sig`
    // Property test: `test_invalid_sig`
    // Property test: `test_infinity_sig`
    // Property test: `test_infinity_vk`
    // Property test: `test_keyreg_with_infinity_vk`
    // Property test: `test_aggregate_sig`
    // Property test: `test_eval_sanity_check`
    // Property test: `serialize_deserialize_vk`
    // Property test: `serialize_deserialize_sk`
    // Property test: `batch_verify`
    // ---------------------------------------------------------------------
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
