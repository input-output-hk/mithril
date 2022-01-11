//! In this example we provide a simple instance of Naive Schnorr MultiSignatures (NSMS)
//! using Ristretto, and use it to instantiate ATMS while being compatible with
//! Schnorr-signatures. Naive Schnorr MultiSignatures suffer from the rogue-key attack,
//! which forces us to include Proofs of Possession of the private key with respect to a
//! public key (same as what is required with the pairing-based multisignature
//! [msp](../src/msp.rs)). This construction is interactive, as we need signers to share
//! the committed randomness with all other participants.
//!
//! # DISCLAIMER
//! This implementation of Schnorr signatures is not secure. It is a minimal working
//! version to show how ATMS can be instantiated with it, and as a consequence adapt
//! ATMS to be more generic.

#![allow(clippy::type_complexity)]

use ark_ff::ToBytes;
use blake2::{Blake2b, Digest};
use curve25519_dalek::{
    constants::{RISTRETTO_BASEPOINT_POINT, BASEPOINT_ORDER}, ristretto::RistrettoPoint, scalar::Scalar,
    traits::Identity,
};
use mithril::{
    atms::{Asig, Atms, Avk},
    stm::Stake,
};
use rand::{CryptoRng, RngCore};
use std::{
    cmp::Ordering,
    hash::{Hash, Hasher},
    io::{Error, Write},
    iter::Sum,
    ops::Sub,
};

#[derive(Debug)]
pub struct NaiveSchnorr;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SchnorrVk(RistrettoPoint);

impl SchnorrVk {
    /// Compare two `SchnorrPk`s. Used for PartialOrd impl, used to order keys. The comparison
    /// function can be anything, as long as it is consistent.
    pub fn cmp_msp_mvk(&self, other: &Self) -> Ordering {
        let lhs_bytes = self.0.compress();
        let rhs_bytes = other.0.compress();

        for (l, r) in lhs_bytes.as_bytes().iter().zip(rhs_bytes.as_bytes().iter()) {
            match l.cmp(r) {
                Ordering::Less => return Ordering::Less,
                Ordering::Equal => continue,
                Ordering::Greater => return Ordering::Greater,
            }
        }
        Ordering::Equal
    }

    pub unsafe fn convert_ed25519(&self) -> Ed25519Vk {
        Ed25519Vk(mul_torsion_safe(&std::mem::transmute(self.0)))
    }
}

impl PartialOrd for SchnorrVk {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp_msp_mvk(other))
    }
}

impl Ord for SchnorrVk {
    fn cmp(&self, other: &Self) -> Ordering {
        self.cmp_msp_mvk(other)
    }
}

/// Helper function for ATMS. This will contain a public key and its associated PoP.
/// The latter is not checked to be valid, and it is left to the creator function to verify
/// the provided proof of possession. This structure allows us to define binary
/// operations over prepared keys.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SchnorrPk {
    vk: SchnorrVk,
    pop: (RistrettoPoint, Scalar),
}

impl SchnorrPk {
    pub fn new<R>(pk: SchnorrVk, sk: Scalar, rng: &mut R) -> Self
    where R: CryptoRng + RngCore
    {
        let randomness = Scalar::random(rng);
        let announcement = randomness * RISTRETTO_BASEPOINT_POINT;

        let challenge = Scalar::from_hash(blake2::Blake2b::new()
            .chain(pk.0.compress().as_bytes())
            .chain(announcement.compress().as_bytes()));

        let response = randomness + challenge * sk;

        Self{
            vk: pk,
            pop: (announcement, response)
        }

    }
    /// Verify PoP
    pub fn verify_pop(&self) -> bool {
        let announcement = self.pop.0;
        let response = self.pop.1;

        let challenge = Scalar::from_hash(blake2::Blake2b::new()
            .chain(self.vk.0.compress().as_bytes())
            .chain(announcement.compress().as_bytes()));

        let lhs = response * RISTRETTO_BASEPOINT_POINT;
        let rhs = announcement + challenge * self.vk.0;

        lhs == rhs
    }
}

impl ToBytes for SchnorrVk {
    fn write<W: Write>(&self, mut writer: W) -> Result<(), Error> {
        writer.write_all(self.0.compress().as_bytes())?;
        Ok(())
    }
}

impl<'a> Sum<&'a Self> for SchnorrVk {
    fn sum<I>(iter: I) -> Self
        where
            I: Iterator<Item = &'a Self>,
    {
        SchnorrVk(iter.map(|x| x.0).sum())
    }
}

#[allow(clippy::derive_hash_xor_eq)]
impl Hash for SchnorrVk {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.compress().as_bytes().hash(state);
    }
}

impl Sub<SchnorrVk> for SchnorrVk {
    type Output = Self;
    fn sub(self, rhs: Self) -> Self {
        SchnorrVk(self.0 - rhs.0)
    }
}

#[derive(Clone)]
pub struct SchnorrSigner {
    sk: Scalar,
    pk: SchnorrPk,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SchnorrSignature {
    announcement: RistrettoPoint,
    response: Scalar,
}

impl PartialOrd for SchnorrSignature {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp_msp_mvk(other))
    }
}

impl Ord for SchnorrSignature {
    fn cmp(&self, other: &Self) -> Ordering {
        self.cmp_msp_mvk(other)
    }
}

impl<'a> Sum<&'a Self> for SchnorrSignature {
    fn sum<I>(iter: I) -> Self
        where
            I: Iterator<Item = &'a Self>,
    {
        // todo: definitely not the way to go
        let mut announcement = RISTRETTO_BASEPOINT_POINT;
        let mut response = Scalar::zero();
        for element in iter {
            announcement = element.announcement;
            response += element.response;
        }
        SchnorrSignature {
            response,
            announcement,
        }
    }
}

impl SchnorrSigner {
    fn new<R>(rng: &mut R) -> Self
        where
            R: CryptoRng + RngCore,
    {
        let sk = Scalar::random(rng);
        let vk = SchnorrVk(sk * RISTRETTO_BASEPOINT_POINT);

        let pk = SchnorrPk::new(vk, sk, rng);

        Self { sk, pk }
    }

    fn commit_randomness<R>(rng: &mut R) -> (Scalar, RistrettoPoint)
        where
            R: CryptoRng + RngCore,
    {
        let randomness = Scalar::random(rng);
        let commitment = randomness * RISTRETTO_BASEPOINT_POINT;

        (randomness, commitment)
    }

    fn partial_signature(
        self,
        pks: &[SchnorrVk],
        committed_randomness: &[RistrettoPoint],
        private_randomness: &Scalar,
        message: &[u8],
    ) -> SchnorrSignature {
        let aggr_key = Self::aggregate_keys(pks);
        let mut announcement = RistrettoPoint::identity();

        for committed_pair in committed_randomness {
            announcement += committed_pair;
        }

        let challenge = Scalar::from_hash(
            Blake2b::new()
                .chain(aggr_key.0.compress().as_bytes())
                .chain(announcement.compress().as_bytes())
                .chain(message),
        );

        let response = challenge * self.sk + private_randomness;

        SchnorrSignature {
            announcement,
            response,
        }
    }

    fn aggregate_signatures(sigs: &[SchnorrSignature]) -> Result<SchnorrSignature, ()> {
        // todo: Maybe we want to check the announcement here as well
        let announcement = sigs[0].announcement;
        let mut aggr_response = Scalar::zero();
        for sig in sigs {
            if announcement != sig.announcement {
                return Err(());
            }
            aggr_response += sig.response;
        }

        Ok(SchnorrSignature {
            announcement,
            response: aggr_response,
        })
    }

    fn aggregate_keys(pks: &[SchnorrVk]) -> SchnorrVk {
        let mut aggr_key = RistrettoPoint::identity();
        for pk in pks {
            aggr_key += pk.0;
        }
        SchnorrVk(aggr_key)
    }
}

impl SchnorrSignature {
    fn verify(&self, pk: &SchnorrVk, message: &[u8]) -> Result<(), ()> {
        let lhs = self.response * RISTRETTO_BASEPOINT_POINT;

        let challenge = Scalar::from_hash(
            Blake2b::new()
                .chain(pk.0.compress().as_bytes())
                .chain(self.announcement.compress().as_bytes())
                .chain(message),
        );
        let rhs = challenge * pk.0 + self.announcement;

        if lhs != rhs {
            return Err(());
        }

        Ok(())
    }

    /// Compare two `SchnorrSignature`s. Used for PartialOrd impl, used to order signatures. The comparison
    /// function can be anything, as long as it is consistent.
    pub fn cmp_msp_mvk(&self, other: &Self) -> Ordering {
        let lhs_bytes = self.response.as_bytes();
        let rhs_bytes = other.response.as_bytes();

        for (l, r) in lhs_bytes.iter().zip(rhs_bytes.iter()) {
            match l.cmp(r) {
                Ordering::Less => return Ordering::Less,
                Ordering::Equal => continue,
                Ordering::Greater => return Ordering::Greater,
            }
        }

        Ordering::Equal
    }

    pub unsafe fn convert_ed25519(&self) -> Ed25519Signature {
        Ed25519Signature { announcement: mul_torsion_safe(&std::mem::transmute(self.announcement)), response: self.response }
    }
}

impl Atms for NaiveSchnorr {
    type PreCheckedPK = SchnorrPk;
    type PreparedPk = SchnorrVk;
    type SIG = SchnorrSignature;

    fn verify(
        msg: &[u8],
        aggregate_key: Self::PreparedPk,
        nonsigners_aggregate_key: Self::PreparedPk,
        sig: &Self::SIG,
    ) -> bool {
        let verification_key = aggregate_key.clone() - nonsigners_aggregate_key;

        sig.verify(&verification_key, msg).is_ok()
    }

    fn prepare_keys(
        keys_pop: &[(Self::PreCheckedPK, Stake)],
    ) -> Option<(Vec<(Self::PreparedPk, Stake)>, Stake)> {
        let mut total_stake = 0u64;
        let mut keys = Vec::with_capacity(keys_pop.len());

        for &(pk, stake) in keys_pop.iter() {
            if !pk.verify_pop() {
                return None;
            }

            total_stake += stake;
            keys.push((
                pk.vk,
                stake,
            ));
        }
        Some((keys, total_stake))
    }
}

use curve25519_dalek::edwards::EdwardsPoint;

#[derive(Debug)]
pub struct NaiveEd25519;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Ed25519Vk(EdwardsPoint);

fn mul_torsion_safe(point: &EdwardsPoint) -> EdwardsPoint {
    point * BASEPOINT_ORDER * Scalar::from(3u8) + point
}

impl Ed25519Vk {
    /// Compare two `iEd25519Vk`s. Used for PartialOrd impl, used to order keys. The comparison
    /// function can be anything, as long as it is consistent.
    pub fn cmp_msp_mvk(&self, other: &Self) -> Ordering {
        let lhs_bytes = self.0.compress();
        let rhs_bytes = other.0.compress();

        for (l, r) in lhs_bytes.as_bytes().iter().zip(rhs_bytes.as_bytes().iter()) {
            match l.cmp(r) {
                Ordering::Less => return Ordering::Less,
                Ordering::Equal => continue,
                Ordering::Greater => return Ordering::Greater,
            }
        }
        Ordering::Equal
    }
}

impl PartialOrd for Ed25519Vk {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp_msp_mvk(other))
    }
}

impl Ord for Ed25519Vk {
    fn cmp(&self, other: &Self) -> Ordering {
        self.cmp_msp_mvk(other)
    }
}

impl ToBytes for Ed25519Vk {
    fn write<W: Write>(&self, mut writer: W) -> Result<(), Error> {
        writer.write_all(self.0.compress().as_bytes())?;
        Ok(())
    }
}

impl<'a> Sum<&'a Self> for Ed25519Vk {
    fn sum<I>(iter: I) -> Self
        where
            I: Iterator<Item = &'a Self>,
    {
        Ed25519Vk(iter.map(|x| x.0).sum())
    }
}

#[allow(clippy::derive_hash_xor_eq)]
impl Hash for Ed25519Vk {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.compress().as_bytes().hash(state);
    }
}

impl Sub<Ed25519Vk> for Ed25519Vk {
    type Output = Self;
    fn sub(self, rhs: Self) -> Self {
        Ed25519Vk(self.0 - rhs.0)
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Ed25519Signature {
    announcement: EdwardsPoint,
    response: Scalar,
}

impl PartialOrd for Ed25519Signature {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp_msp_mvk(other))
    }
}

impl Ord for Ed25519Signature {
    fn cmp(&self, other: &Self) -> Ordering {
        self.cmp_msp_mvk(other)
    }
}

impl<'a> Sum<&'a Self> for Ed25519Signature {
    fn sum<I>(iter: I) -> Self
        where
            I: Iterator<Item = &'a Self>,
    {
        // todo: definitely not the way to go
        let mut announcement = EdwardsPoint::identity();
        let mut response = Scalar::zero();
        for element in iter {
            announcement = element.announcement;
            response += element.response;
        }
        Ed25519Signature {
            response,
            announcement,
        }
    }
}

impl Ed25519Signature {
    fn verify(&self, pk: &Ed25519Vk, message: &[u8]) -> Result<(), ()> {
        let lhs = self.response * EdwardsPoint::identity();

        let challenge = Scalar::from_hash(
            Blake2b::new()
                .chain(pk.0.compress().as_bytes())
                .chain(self.announcement.compress().as_bytes())
                .chain(message),
        );
        let rhs = challenge * pk.0 + self.announcement;

        if lhs != rhs {
            return Err(());
        }

        Ok(())
    }

    /// Compare two `SchnorrSignature`s. Used for PartialOrd impl, used to order signatures. The comparison
    /// function can be anything, as long as it is consistent.
    pub fn cmp_msp_mvk(&self, other: &Self) -> Ordering {
        let lhs_bytes = self.response.as_bytes();
        let rhs_bytes = other.response.as_bytes();

        for (l, r) in lhs_bytes.iter().zip(rhs_bytes.iter()) {
            match l.cmp(r) {
                Ordering::Less => return Ordering::Less,
                Ordering::Equal => continue,
                Ordering::Greater => return Ordering::Greater,
            }
        }

        Ordering::Equal
    }
}

impl Atms for NaiveEd25519 {
    type PreCheckedPK = SchnorrPk;
    type PreparedPk = Ed25519Vk;
    type SIG = Ed25519Signature;

    fn verify(
        msg: &[u8],
        aggregate_key: Self::PreparedPk,
        nonsigners_aggregate_key: Self::PreparedPk,
        sig: &Self::SIG,
    ) -> bool {
        let verification_key = aggregate_key.clone() - nonsigners_aggregate_key;

        sig.verify(&verification_key, msg).is_ok()
    }

    fn prepare_keys(
        keys_pop: &[(Self::PreCheckedPK, Stake)],
    ) -> Option<(Vec<(Self::PreparedPk, Stake)>, Stake)> {
        let mut total_stake = 0u64;
        let mut keys = Vec::with_capacity(keys_pop.len());

        for &(pk, stake) in keys_pop.iter() {
            if !pk.verify_pop() {
                return None;
            }

            total_stake += stake;
            keys.push((
                unsafe {pk.vk.convert_ed25519()},
                stake,
            ));
        }
        Some((keys, total_stake))
    }
}

fn main() {
    use rand::rngs::OsRng;
    let nr_signers = 4;
    // let's test NaiveSchnorr with an ed25519 verifier.
    let msg = b"testing naive Schnorr";
    let signers: Vec<SchnorrSigner> = (0..nr_signers)
        .into_iter()
        .map(|_| SchnorrSigner::new(&mut OsRng))
        .collect();
    let pks: Vec<SchnorrVk> = signers
        .clone()
        .into_iter()
        .map(|signer| signer.pk.vk)
        .collect();

    let commitments: Vec<(Scalar, RistrettoPoint)> = (0..nr_signers)
        .into_iter()
        .map(|_| SchnorrSigner::commit_randomness(&mut OsRng))
        .collect();
    let public_comms: Vec<RistrettoPoint> = commitments
        .clone()
        .into_iter()
        .map(|comm| comm.1)
        .collect();

    let partial_signatures: Vec<SchnorrSignature> = signers
        .into_iter()
        .enumerate()
        .map(|(index, signer)| {
            signer.partial_signature(
                &pks,
                &public_comms,
                &commitments[index].0,
                msg,
            )
        })
        .collect();

    // now we aggregate them
    let aggr_sig = SchnorrSigner::aggregate_signatures(&partial_signatures).expect("Not expecting to fail");
    let aggr_pk = SchnorrSigner::aggregate_keys(&pks);

    assert!(aggr_sig.verify(&aggr_pk, msg).is_ok());

    ////////////////////////////////////////////////////
    // Now lets test the ATMS signatures with MuSig2. //
    ////////////////////////////////////////////////////

    let n = 10u64;
    let threshold = 8u64;
    let mut rng = OsRng;

    let mut pkeys: Vec<SchnorrVk> = Vec::new();
    let mut private_commitments = Vec::new();
    let mut pub_announcements = Vec::new();
    let mut signers: Vec<SchnorrSigner> = Vec::new();
    let mut keys_stake = Vec::new();
    for _ in 0..n {
        let signer = SchnorrSigner::new(&mut rng);
        let (r, c) = SchnorrSigner::commit_randomness(&mut rng);
        private_commitments.push(r);
        pub_announcements.push(c);
        pkeys.push(signer.pk.vk);
        keys_stake.push((signer.pk, 1));
        signers.push(signer);
    }

    let mut signatures: Vec<(SchnorrVk, SchnorrSignature)> = Vec::new();
    for (index, signer) in signers[..threshold as usize].iter().enumerate() {
        let sig = signer.clone().partial_signature(
            &pkeys[..threshold as usize],
            &pub_announcements[..threshold as usize],
            &private_commitments[index],
            msg,
        );
        signatures.push((signer.pk.vk, sig));
    }

    let avk = match Avk::<NaiveSchnorr, Blake2b>::new(&keys_stake, threshold) {
        Ok(key) => key,
        Err(_) => panic!("Shouldn't happen"),
    };

    assert!(avk.check(&keys_stake).is_ok());

    let aggr_sig = Asig::new(&avk, &signatures);
    aggr_sig.verify(msg, &avk).unwrap();
    // match aggr_sig.verify(msg, &avk) {
    //     Ok(()) => { },
    //     _ => {unreachable!()}
    // }


    ////////////////////////////////////////////////////
    //  Now lets test the ATMS signatures with NSMS.  //
    ////////////////////////////////////////////////////

    let n = 10u64;
    let threshold = 8u64;
    let mut rng = OsRng;

    let mut pkeys: Vec<SchnorrVk> = Vec::new();
    let mut private_commitments = Vec::new();
    let mut pub_announcements = Vec::new();
    let mut signers: Vec<SchnorrSigner> = Vec::new();
    let mut keys_stake = Vec::new();
    for _ in 0..n {
        let signer = SchnorrSigner::new(&mut rng);
        let (r, c) = SchnorrSigner::commit_randomness(&mut rng);
        private_commitments.push(r);
        pub_announcements.push(c);
        pkeys.push(signer.pk.vk);
        keys_stake.push((signer.pk, 1));
        signers.push(signer);
    }

    let mut signatures: Vec<(Ed25519Vk, Ed25519Signature)> = Vec::new();
    for (index, signer) in signers[..threshold as usize].iter().enumerate() {
        let sig = signer.clone().partial_signature(
            &pkeys[..threshold as usize],
            &pub_announcements[..threshold as usize],
            &private_commitments[index],
            msg,
        );
        signatures.push(unsafe { (signer.pk.vk.convert_ed25519(), sig.convert_ed25519()) });
    }

    let avk = match Avk::<NaiveEd25519, Blake2b>::new(&keys_stake, threshold) {
        Ok(key) => key,
        Err(_) => panic!("Shouldn't happen"),
    };

    assert!(avk.check(&keys_stake).is_ok());

    let aggr_sig = Asig::new(&avk, &signatures);
    aggr_sig.verify(msg, &avk).unwrap();
    // match aggr_sig.verify(msg, &avk) {
    //     Ok(()) => { },
    //     _ => {unreachable!()}
    // }
}
