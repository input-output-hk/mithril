//! Base multisignature scheme, used as a primitive for STM.
//! See Section 2.4 of [the paper](https://eprint.iacr.org/2021/916).
//!
//! The following is an example showing how to use Msp with the BLS12 curve.
//! It creates 10 signatures of the same (arbitrary) message, then shows how to combine
//! those signatures into an aggregate signature.
//!
//! ```rust
//! # fn main() -> Result<(), Box<dyn std::error::Error>> {
//!     use mithril::msp::Msp; // Import the Msp module
//!     use rand_chacha::rand_core::{RngCore, SeedableRng}; // For RNG functionality
//!
//!     // We will create and aggregate 10 signatures in this example
//!     let num_sigs = 10;
//!
//!     // create and initialize the RNG
//!     let mut rng = rand_chacha::ChaCha8Rng::seed_from_u64(42);
//!
//!     // The message will just be some arbitrary data
//!     let mut msg = [0; 16];
//!     rng.fill_bytes(&mut msg);
//!
//!     let mut mvks = Vec::new(); // vec of verification keys
//!     let mut sigs = Vec::new(); // vec of signatures
//!
//!     for _ in 0..num_sigs {
//!         // Create a new keypair using the BLS12_377 curve
//!         let (sk, pk) = Msp::gen(&mut rng);
//!         // Sign the message using an individual secret key
//!         let sig = Msp::sig(&sk, &msg);
//!         // Check that the individual verification is valid
//!         assert!(Msp::ver(&msg, &pk.mvk, &sig));
//!         sigs.push(sig);
//!         mvks.push(pk.mvk);
//!     }
//!
//!     // Aggregate the verification keys
//!     let ivk = Msp::aggregate_keys(&mvks);
//!     // Aggregate the signatures keys
//!     let mu = Msp::aggregate_sigs(&sigs);
//!     // Check that the aggregated signature is valid
//!     assert!(Msp::aggregate_ver(&msg, &ivk, &mu));
//! # Ok(())
//! # }
//! ```

use super::stm::Index;

use blake2::{Blake2b, Digest};
use blst::{blst_p1, blst_p1_affine, blst_p1_compress, blst_p1_from_affine, blst_p1_uncompress};
use blst::min_sig::{
    AggregatePublicKey, AggregateSignature, PublicKey as BlstPk, SecretKey as BlstSk,
    Signature as BlstSig,
};
use rand_core::{CryptoRng, RngCore};
use std::cmp::Ordering;
use std::hash::{Hash, Hasher};
use std::iter::Sum;
use std::ops::Sub;
use crate::error::{MultiSignatureError, blst_err_to_atms};

/// Struct used to namespace the functions.
#[derive(Debug)]
pub struct Msp {}

/// MSP secret key.
#[derive(Debug, Clone)]
pub struct MspSk(BlstSk);

impl MspSk {
    /// Convert the secret key into byte string.
    pub fn to_bytes(&self) -> [u8; 32] {
        self.0.to_bytes()
    }

    /// Convert a string of bytes into a `SigningKey`.
    /// # Error
    /// Fails if the byte string represents a scalar larger than the group order.
    pub fn from_bytes(bytes: &[u8]) -> Result<Self, MultiSignatureError> {
        match BlstSk::from_bytes(&bytes[..32]) {
            Ok(sk) => Ok(Self(sk)),
            Err(e) => Err(blst_err_to_atms(e)
                .expect_err("If deserialisation is not successful, blst returns and error different to SUCCESS."))
        }
    }
}

/// MSP verification key.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct MspMvk(pub(crate) BlstPk);

impl MspMvk {
    /// Convert an `PublicKey` to its compressed byte representation.
    pub fn to_bytes(&self) -> [u8; 96] {
        self.0.to_bytes()
    }

    /// Convert a compressed byte string into a `PublicKey`.
    ///
    /// # Error
    /// This function fails if the bytes do not represent a compressed point of the curve.
    pub fn from_bytes(bytes: &[u8]) -> Result<Self, MultiSignatureError> {
        match BlstPk::from_bytes(&bytes[..96]) {
            Ok(pk) => Ok(Self(pk)),
            Err(e) => Err(blst_err_to_atms(e)
                .expect_err("If deserialisation is not successful, blst returns and error different to SUCCESS."))
        }
    }

    /// Compare two `MspMvk`. Used for PartialOrd impl, used to order signatures. The comparison
    /// function can be anything, as long as it is consistent.
    pub fn cmp_msp_mvk(&self, other: &MspMvk) -> Ordering {
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

impl PartialOrd for MspMvk {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp_msp_mvk(other))
    }
}

impl Ord for MspMvk {
    fn cmp(&self, other: &Self) -> Ordering {
        self.cmp_msp_mvk(other)
    }
}

impl<'a> Sum<&'a Self> for MspMvk {
    fn sum<I>(iter: I) -> Self
    where
        I: Iterator<Item = &'a Self>,
    {
        let mut aggregate_key = BlstPk::default();
        let keys: Vec<&BlstPk> = iter.map(|x| &x.0).collect();

        if !keys.is_empty() {
            aggregate_key = AggregatePublicKey::aggregate(&keys, false)
                .expect("An MspMvk is always a valid key. This function only fails if keys is empty or if the keys are invalid, none of which can happen.")
                .to_public_key();
        }

        Self(aggregate_key)
    }
}

// We need some unsafe code here due to what is being expoed in the rust FFI.
// todo: Take particular care reviewing this
impl Sub for MspMvk {
    type Output = Self;
    fn sub(self, rhs: Self) -> MspMvk {
        use blst::{blst_bendian_from_fp, blst_fp, blst_fp_cneg, blst_fp_from_bendian};
        let mut rhs_bytes = rhs.0.serialize();
        unsafe {
            let y_bytes: Vec<u8> = rhs_bytes[48..].to_vec();
            let mut y: blst_fp = blst_fp::default();
            let mut neg_y: blst_fp = blst_fp::default();
            blst_fp_from_bendian(&mut y, &y_bytes[0]);
            blst_fp_cneg(&mut neg_y, &y, true);

            blst_bendian_from_fp(&mut rhs_bytes[48], &neg_y);
        }
        let neg_rhs = BlstPk::deserialize(&rhs_bytes)
            .expect("The negative of a valid point is also a valid point.");
        MspMvk(
            AggregatePublicKey::aggregate(&[&neg_rhs, &self.0], false)
                .expect("Points are valid")
                .to_public_key(),
        )
    }
}

/// MSP proof of possession
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct MspPoP {
    pub(crate) k1: BlstSig,
    pub(crate) k2: blst_p1,
}

/// MSP public key, contains the verification key and proof of posession.
#[derive(Debug, Clone, Copy)]
pub struct MspPk {
    /// The verification key.
    pub mvk: MspMvk,
    /// Proof of Possession.
    pub pop: MspPoP,
}

impl Hash for MspPk {
    fn hash<H: Hasher>(&self, state: &mut H) {
        Hash::hash_slice(&self.mvk.to_bytes(), state)
    }
}

// We need to implement PartialEq instead of deriving it because we are implementing Hash.
impl PartialEq for MspPk {
    fn eq(&self, other: &Self) -> bool {
        self.mvk == other.mvk
    }
}

impl Eq for MspPk {}

/// MSP signature.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct MspSig(pub(crate) BlstSig);

impl<'a> Sum<&'a Self> for MspSig {
    fn sum<I>(iter: I) -> Self
    where
        I: Iterator<Item = &'a Self>,
    {
        let signatures: Vec<&BlstSig> = iter.map(|x| &x.0).collect();
        assert!(!signatures.is_empty(), "One cannot add an empty vector");
        let aggregate = AggregateSignature::aggregate(&signatures, false)
                .expect("Signatures are assumed verified before aggregation. If signatures are invalid, they should not be aggregated.")
                .to_signature();

        Self(aggregate)
    }
}

impl MspSig {
    /// Convert an `Signature` to its compressed byte representation.
    pub fn to_bytes(&self) -> [u8; 48] {
        self.0.to_bytes()
    }

    /// Convert a string of bytes into a `Signature`.
    /// # Error
    /// Returns an error if the byte string does not represent a point in the curve.
    pub fn from_bytes(bytes: &[u8]) -> Result<Self, MultiSignatureError> {
        match BlstSig::from_bytes(&bytes[..48]) {
            Ok(sig) => Ok(Self(sig)),
            Err(e) => Err(blst_err_to_atms(e)
                .expect_err("If deserialisation is not successful, blst returns and error different to SUCCESS."))
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
}

impl PartialOrd for MspSig {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp_msp_sig(other))
    }
}

impl Ord for MspSig {
    fn cmp(&self, other: &Self) -> Ordering {
        self.cmp_msp_sig(other)
    }
}

const POP: &[u8] = b"PoP";

impl From<&MspSk> for MspMvk {
    fn from(sk: &MspSk) -> Self {
        MspMvk(sk.0.sk_to_pk())
    }
}

// Again, unsafe code to access the algebraic operations.
// todo: particular care reviewing this
impl From<&MspSk> for MspPoP {
    fn from(sk: &MspSk) -> Self {
        use blst::{blst_scalar, blst_scalar_from_bendian, blst_sk_to_pk_in_g1};
        let k1 = sk.0.sign(POP, &[], &[]);
        // k2 <- g1^x
        let k2 = unsafe {
            let mut sk_scalar = blst_scalar::default();
            blst_scalar_from_bendian(&mut sk_scalar, &sk.0.to_bytes()[0]);

            let mut out = blst_p1::default();
            blst_sk_to_pk_in_g1(&mut out, &sk_scalar);
            out
        };
        // return sk,mvk,k=(k1,k2)
        Self { k1, k2 }
    }
}

impl From<&MspSk> for MspPk {
    fn from(sk: &MspSk) -> Self {
        Self {
            mvk: sk.into(),
            pop: sk.into(),
        }
    }
}

impl MspPk {
    /// if e(k1,g2) = e(H_G1("PoP"||mvk),mvk) and e(g1,mvk) = e(k2,g2)
    /// are both true, return 1. The first part is a signature verification
    /// of message "PoP", while the second we need to compute the pairing
    /// manually.
    // todo: review carefully. Unsafe to use algebraic operations
    fn check(&self) -> Result<(), MultiSignatureError> {
        use blst::{
            blst_fp12, blst_fp12_finalverify, blst_p1_affine, blst_p1_affine_generator,
            blst_p1_to_affine, blst_p2_affine, blst_p2_affine_generator, blst_p2_uncompress,
            BLST_ERROR,
        };
        let result = unsafe {
            let g1_p = *blst_p1_affine_generator();
            let mut mvk_p = blst_p2_affine::default();
            assert_eq!(
                blst_p2_uncompress(&mut mvk_p, &self.mvk.0.to_bytes()[0]),
                BLST_ERROR::BLST_SUCCESS
            );
            let ml_lhs = blst_fp12::miller_loop(&mvk_p, &g1_p);

            let mut k2_p = blst_p1_affine::default();
            blst_p1_to_affine(&mut k2_p, &self.pop.k2);
            let g2_p = *blst_p2_affine_generator();
            let ml_rhs = blst_fp12::miller_loop(&g2_p, &k2_p);

            blst_fp12_finalverify(&ml_lhs, &ml_rhs)
        };

        if !(self.pop.k1.verify(false, POP, &[], &[], &self.mvk.0, false) == BLST_ERROR::BLST_SUCCESS && result) {
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
    pub fn to_bytes(&self) -> [u8; 192] {
        let mut pkpop_bytes = [0u8; 192];
        pkpop_bytes[..96].copy_from_slice(&self.mvk.to_bytes());
        pkpop_bytes[96..].copy_from_slice(&self.pop.to_bytes());
        pkpop_bytes
    }

    /// Deserialise a byte string to a `PublicKeyPoP`.
    // todo: deserialise only valid keys?
    pub fn from_bytes(bytes: &[u8]) -> Result<Self, MultiSignatureError> {
        let mvk = MspMvk::from_bytes(&bytes[..96])?;

        let pop = MspPoP::from_bytes(&bytes[96..])?;

        Ok(Self { mvk , pop })
    }
}

impl MspPoP {
    /// Convert to a 144 byte string.
    ///
    /// # Layout
    /// The layout of a `MspPoP` encoding is
    /// * K1 (G1 point)
    /// * K2 (G1 point)
    pub fn to_bytes(&self) -> [u8; 96] {
        let mut pop_bytes = [0u8; 96];
        pop_bytes[..48].copy_from_slice(&self.k1.to_bytes());
        let k2_bytes = unsafe {
            let mut bytes = [0u8; 48];
            blst_p1_compress(&mut bytes[0], &self.k2);
            bytes
        };
        pop_bytes[48..].copy_from_slice(&k2_bytes);
        pop_bytes
    }

    /// Deserialise a byte string to a `PublicKeyPoP`.
    pub fn from_bytes(bytes: &[u8]) -> Result<Self, MultiSignatureError> {
        let k1 = match BlstSig::from_bytes(&bytes[..48]) {
            Ok(key) => key,
            Err(e) => {
                return Err(blst_err_to_atms(e)
                    .expect_err("If it passed, blst returns and error different to SUCCESS."))
            }
        };

        let k2 = unsafe {
            let mut point = blst_p1_affine::default();
            let mut out = blst_p1::default();
            blst_p1_uncompress(&mut point,  &bytes[48]);
            blst_p1_from_affine(&mut out, &point);
            out
        };

        Ok(Self{ k1, k2 })
    }
}

impl Msp {
    /// Create a new pubkey/secretkey pair.
    pub fn gen<R>(rng: &mut R) -> (MspSk, MspPk)
    where
        R: RngCore + CryptoRng,
    {
        let mut ikm = [0u8; 32];
        rng.fill_bytes(&mut ikm);
        let sk = MspSk(
            BlstSk::key_gen(&ikm, &[])
                .expect("Error occurs when the length of ikm < 32. This will not happen here."),
        );
        let pk: MspPk = (&sk).into();
        (sk, pk)
    }

    /// Check that a pubkey is well-formed.
    pub fn check(pk: &MspPk) -> Result<(), MultiSignatureError> {
        pk.check()
    }

    /// Sign a message using a secret key.
    pub fn sig(sk: &MspSk, msg: &[u8]) -> MspSig {
        MspSig(sk.0.sign(msg, &[], &[]))
    }

    /// Verify a signature against a verification key.
    pub fn ver(msg: &[u8], mvk: &MspMvk, sigma: &MspSig) -> bool {
        use blst::BLST_ERROR;
        sigma.0.verify(false, msg, &[], &[], &mvk.0, false) == BLST_ERROR::BLST_SUCCESS
    }

    /// Aggregate verification keys.
    pub fn aggregate_keys(mvks: &[MspMvk]) -> MspMvk {
        mvks.iter().sum()
    }

    /// Aggregate signatures.
    pub fn aggregate_sigs(sigmas: &[MspSig]) -> MspSig {
        sigmas.iter().sum()
    }

    /// Verify an aggregate signature (identical to `Msp::ver`).
    pub fn aggregate_ver(msg: &[u8], ivk: &MspMvk, mu: &MspSig) -> bool {
        Self::ver(msg, ivk, mu)
    }

    /// Hash the signature to produce a 64 bytes integer. We follow the same mechanism as Shelley
    /// for the lottery (i.e., we follow the VRF lottery mechanism as described in Section 16 of
    /// <https://hydra.iohk.io/build/8201171/download/1/ledger-spec.pdf>).
    // todo: if we are generic over the hash function, shouldn't we use the instance here?
    pub fn eval(msg: &[u8], index: Index, sigma: &MspSig) -> [u8; 64] {
        let hasher = Blake2b::new()
            .chain(b"map")
            .chain(msg)
            .chain(&index.to_le_bytes())
            .chain(&sigma.to_bytes())
            .finalize();

        let mut output = [0u8; 64];
        output.copy_from_slice(hasher.as_slice());

        output
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use proptest::prelude::*;
    use rand_chacha::ChaCha20Rng;
    use rand_core::{OsRng, SeedableRng};

    impl PartialEq for MspSk {
        fn eq(&self, other: &Self) -> bool {
            self.0.to_bytes() == other.0.to_bytes()
        }
    }

    impl Eq for MspSk {}

    proptest! {
        #![proptest_config(ProptestConfig::with_cases(1000))]

        #[test]
        fn test_sig(
            msg in prop::collection::vec(any::<u8>(), 1..128),
            seed in any::<[u8;32]>(),
        ) {
            let (sk, pk) = Msp::gen(&mut ChaCha20Rng::from_seed(seed));
            let sig = Msp::sig(&sk, &msg);
            assert!(Msp::ver(&msg, &pk.mvk, &sig));
        }

        #[test]
        fn test_invalid_sig(msg in prop::collection::vec(any::<u8>(), 1..128),
                            seed in any::<[u8;32]>(),
        ) {
            let mut rng = ChaCha20Rng::from_seed(seed);
            let (_sk1, pk1) = Msp::gen(&mut rng);
            let (sk2, _pk2) = Msp::gen(&mut rng);
            let fake_sig = Msp::sig(&sk2, &msg);
            assert!(!Msp::ver(&msg, &pk1.mvk, &fake_sig));
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
                let (sk, pk) = Msp::gen(&mut rng);
                let sig = Msp::sig(&sk, &msg);
                assert!(Msp::ver(&msg, &pk.mvk, &sig));
                sigs.push(sig);
                mvks.push(pk.mvk);
            }
            let ivk = Msp::aggregate_keys(&mvks);
            let mu = Msp::aggregate_sigs(&sigs);
            assert!(Msp::aggregate_ver(&msg, &ivk, &mu));
        }

        #[test]
        fn test_eval_sanity_check(msg in prop::collection::vec(any::<u8>(), 1..128),
                                  idx in any::<u64>(),
                                  seed in any::<[u8;32]>()) {
            let (sk, _pk) = Msp::gen(&mut ChaCha20Rng::from_seed(seed));
            let sig = Msp::sig(&sk, &msg);
            Msp::eval(&msg, idx, &sig);
        }

        #[test]
        fn serialize_deserialize_pk(seed in any::<u64>()) {
            let mut rng = rand_chacha::ChaCha8Rng::seed_from_u64(seed);
            let (_, pk) = Msp::gen(&mut rng);
            let pk_bytes = pk.to_bytes();
            let pk2: MspPk = MspPk::from_bytes(&pk_bytes).unwrap();
            assert!(pk == pk2);
        }

        #[test]
        fn serialize_deserialize_sk(seed in any::<u64>()) {
            let mut rng = rand_chacha::ChaCha8Rng::seed_from_u64(seed);
            let (sk, _) = Msp::gen(&mut rng);
            let sk_bytes: [u8; 32] = sk.to_bytes();
            let sk2 = MspSk::from_bytes(&sk_bytes).unwrap();
            assert!(sk == sk2);
        }
    }

    #[test]
    fn test_gen() {
        for _ in 0..128 {
            let (_sk, pk) = Msp::gen(&mut OsRng);
            assert!(Msp::check(&pk).is_ok());
        }
    }
}
