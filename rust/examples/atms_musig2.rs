//! In this example we provide a simple instance of MuSig2 using Ristretto, and use it to
//! instantiate ATMS while being compatible with Schnorr-signatures. Contrary to MuSig2, this
//! construction is interactive, as we need signers to know which are the participating signers.
//! In MuSig2, all signers need to participate. However, for ATMS that is not the case. In order
//! to produce a valid signature, they need to know which is the subset of participating signers.
//!
//! # DISCLAIMER
//! This implementation of MuSig2 is not secure. It is a minimal working version to show how ATMS
//! can be instantiated with it, and as a consequence adapt ATMS to be more generic.

#![allow(clippy::type_complexity)]

use ark_ff::ToBytes;
use blake2::{Blake2b, Digest};
use curve25519_dalek::{
    constants::RISTRETTO_BASEPOINT_POINT, ristretto::RistrettoPoint, scalar::Scalar,
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
struct MuSig2;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct MuSigPk(RistrettoPoint);

impl MuSigPk {
    /// Compare two `MuSigPk`s. Used for PartialOrd impl, used to order keys. The comparison
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

impl PartialOrd for MuSigPk {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp_msp_mvk(other))
    }
}

impl Ord for MuSigPk {
    fn cmp(&self, other: &Self) -> Ordering {
        self.cmp_msp_mvk(other)
    }
}


/// Helper function for ATMS. This will contain a public key and its associated exponent.
/// The latter is not checked to be valid, and it is left to the creator function to compute
/// the valid exponent (or verify its validity). This structure allows us to define binary
/// operations over prepared keys.
/// As a result of a binary operation between two keys, the resulting key has 1 as a public
/// exponent.
#[derive(Debug, Clone, PartialEq, Eq)]
struct PreparedPk {
    pk: MuSigPk,
    public_exponent: Scalar,
}

impl PreparedPk {
    /// Compare two `PreparedPk`s. Used for PartialOrd impl, used to order signatures. The comparison
    /// function can be anything, as long as it is consistent.
    pub fn cmp_msp_mvk(&self, other: &PreparedPk) -> Ordering {
        let lhs_bytes = self.public_exponent.as_bytes();
        let rhs_bytes = other.public_exponent.as_bytes();

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

impl ToBytes for PreparedPk {
    fn write<W: Write>(&self, mut writer: W) -> Result<(), Error> {
        writer.write_all(self.pk.0.compress().as_bytes())?;
        writer.write_all(self.public_exponent.as_bytes())?;
        Ok(())
    }
}

impl PartialOrd for PreparedPk {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp_msp_mvk(other))
    }
}

impl Ord for PreparedPk {
    fn cmp(&self, other: &Self) -> Ordering {
        self.cmp_msp_mvk(other)
    }
}

impl<'a> Sum<&'a Self> for PreparedPk {
    fn sum<I>(iter: I) -> Self
    where
        I: Iterator<Item = &'a Self>,
    {
        PreparedPk {
            pk: MuSigPk(iter.map(|x| x.public_exponent * x.pk.0).sum()),
            public_exponent: Scalar::one(),
        }
    }
}

#[allow(clippy::derive_hash_xor_eq)]
impl Hash for PreparedPk {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.public_exponent.as_bytes().hash(state);
        self.pk.0.compress().as_bytes().hash(state);
    }
}

impl Sub<PreparedPk> for PreparedPk {
    type Output = Self;
    fn sub(self, rhs: Self) -> PreparedPk {
        PreparedPk {
            pk: MuSigPk(self.public_exponent * self.pk.0 - rhs.public_exponent * rhs.pk.0),
            public_exponent: Scalar::one(),
        }
    }
}

#[derive(Clone)]
struct MuSigSigner {
    sk: Scalar,
    pk: MuSigPk,
}

#[derive(Clone, Debug, Eq, PartialEq)]
struct MuSigSignature {
    announcement: RistrettoPoint,
    response: Scalar,
}

impl PartialOrd for MuSigSignature {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp_msp_mvk(other))
    }
}

impl Ord for MuSigSignature {
    fn cmp(&self, other: &Self) -> Ordering {
        self.cmp_msp_mvk(other)
    }
}

impl<'a> Sum<&'a Self> for MuSigSignature {
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
        MuSigSignature {
            response,
            announcement,
        }
    }
}

impl MuSigSigner {
    fn new<R>(rng: &mut R) -> Self
    where
        R: CryptoRng + RngCore,
    {
        let sk = Scalar::random(rng);
        let pk = MuSigPk(sk * RISTRETTO_BASEPOINT_POINT);

        Self { sk, pk }
    }

    fn commit_randomness<R>(rng: &mut R) -> (Scalar, Scalar, RistrettoPoint, RistrettoPoint)
    where
        R: CryptoRng + RngCore,
    {
        let randomness_1 = Scalar::random(rng);
        let commitment_1 = randomness_1 * RISTRETTO_BASEPOINT_POINT;

        let randomness_2 = Scalar::random(rng);
        let commitment_2 = randomness_2 * RISTRETTO_BASEPOINT_POINT;

        (randomness_1, randomness_2, commitment_1, commitment_2)
    }

    fn partial_signature(
        self,
        eligible_pks: &[MuSigPk],
        participating_pks: &[MuSigPk],
        committed_randomness: &[(RistrettoPoint, RistrettoPoint)],
        private_randomness: &(Scalar, Scalar),
        message: &[u8],
    ) -> MuSigSignature {
        let aggr_key = Self::aggregate_keys(eligible_pks, participating_pks);
        let mut added_commit_1 = RistrettoPoint::identity();
        let mut added_commit_2 = RistrettoPoint::identity();

        let mut flatted_commitments = Vec::new();
        for committed_pair in committed_randomness {
            added_commit_1 += committed_pair.0;
            added_commit_2 += committed_pair.1;
            flatted_commitments.extend_from_slice(committed_pair.0.compress().as_bytes());
            flatted_commitments.extend_from_slice(committed_pair.1.compress().as_bytes());
        }

        let _b_1 = Scalar::one(); // we ignore it
        let hash_b_2 = Blake2b::new()
            .chain(b"2")
            .chain(aggr_key.0.compress().as_bytes())
            .chain(flatted_commitments)
            .chain(message);
        let b_2 = Scalar::from_hash(hash_b_2);

        let announcement = added_commit_1 + b_2 * added_commit_2;
        let challenge = Scalar::from_hash(
            Blake2b::new()
                .chain(aggr_key.0.compress().as_bytes())
                .chain(announcement.compress().as_bytes())
                .chain(message),
        );

        let mut hash = Blake2b::new();
        let mut ordered_pks = eligible_pks.to_vec();
        ordered_pks.sort_unstable();
        for pk in ordered_pks {
            hash.update(pk.0.compress().as_bytes());
        }

        hash.update(self.pk.0.compress().as_bytes());

        let ai = Scalar::from_hash(hash);
        let response = challenge * ai * self.sk + private_randomness.0 + private_randomness.1 * b_2;

        MuSigSignature {
            announcement,
            response,
        }
    }

    fn aggregate_signatures(sigs: &[MuSigSignature]) -> MuSigSignature {
        // todo: Maybe we want to check the announcement here as well
        let mut aggr_response = Scalar::zero();
        for sig in sigs {
            aggr_response += sig.response;
        }

        MuSigSignature {
            announcement: sigs[0].announcement,
            response: aggr_response,
        }
    }

    fn aggregate_keys(eligible_pks: &[MuSigPk], participants_pks: &[MuSigPk]) -> MuSigPk {
        assert!(eligible_pks.len() >= participants_pks.len());
        let mut hash = Blake2b::new();
        let mut ordered_pks = eligible_pks.to_vec();
        ordered_pks.sort_unstable();
        for pk in ordered_pks {
            hash.update(pk.0.compress().as_bytes());
        }

        let mut aggr_key = RistrettoPoint::identity();
        for pk in participants_pks {
            let mut hasheri = hash.clone();
            hasheri.update(pk.0.compress().as_bytes());
            aggr_key += Scalar::from_hash(hasheri) * pk.0;
        }
        MuSigPk(aggr_key)
    }

    fn compute_prepared_pk(&self, pks: &[MuSigPk]) -> PreparedPk {
        let mut hasher = Blake2b::new();
        let mut ordered_pks = pks.to_vec();
        ordered_pks.sort_unstable();
        for pk in ordered_pks {
            hasher.update(pk.0.compress().as_bytes());
        }

        hasher.update(self.pk.0.compress().as_bytes());

        PreparedPk {
            pk: self.pk,
            public_exponent: Scalar::from_hash(hasher),
        }
    }
}

impl MuSigSignature {
    fn verify(&self, pk: &MuSigPk, message: &[u8]) -> Result<(), ()> {
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

    /// Compare two `MuSigSignature`s. Used for PartialOrd impl, used to order signatures. The comparison
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

impl Atms for MuSig2 {
    type PreCheckedPK = MuSigPk;
    type PreparedPk = PreparedPk;
    type SIG = MuSigSignature;

    fn verify(
        msg: &[u8],
        aggregate_key: Self::PreparedPk,
        nonsigners_aggregate_key: Self::PreparedPk,
        sig: &Self::SIG,
    ) -> bool {
        let verification_key = aggregate_key.clone() - nonsigners_aggregate_key;

        sig.verify(&verification_key.pk, msg).is_ok()
    }

    fn prepare_keys(
        keys_pop: &[(Self::PreCheckedPK, Stake)],
    ) -> Option<(Vec<(Self::PreparedPk, Stake)>, Stake)> {
        let mut total_stake = 0u64;
        let mut keys = Vec::with_capacity(keys_pop.len());
        let mut ordered_keys: Vec<MuSigPk> = keys_pop.iter().map(|key| key.0).collect();
        ordered_keys.sort_unstable();
        let mut hasher: Blake2b = Blake2b::new();
        for key in ordered_keys.iter() {
            hasher.update(key.0.compress().as_bytes());
        }

        for &(pk, stake) in keys_pop.iter() {
            let mut hasheri = hasher.clone();
            hasheri.update(pk.0.compress().as_bytes());
            total_stake += stake;
            keys.push((
                PreparedPk {
                    pk,
                    public_exponent: Scalar::from_hash(hasheri),
                },
                stake,
            ));
        }
        Some((keys, total_stake))
    }
}

fn main() {
    use rand::rngs::OsRng;
    let nr_signers = 4;
    // let's test MuSig2
    let msg = b"testing dummy MuSig2";
    let signers: Vec<MuSigSigner> = (0..nr_signers)
        .into_iter()
        .map(|_| MuSigSigner::new(&mut OsRng))
        .collect();
    let pks: Vec<MuSigPk> = signers
        .clone()
        .into_iter()
        .map(|signer| signer.pk)
        .collect();

    let commitments: Vec<(Scalar, Scalar, RistrettoPoint, RistrettoPoint)> = (0..nr_signers)
        .into_iter()
        .map(|_| MuSigSigner::commit_randomness(&mut OsRng))
        .collect();
    let public_comms: Vec<(RistrettoPoint, RistrettoPoint)> = commitments
        .clone()
        .into_iter()
        .map(|comm| (comm.2, comm.3))
        .collect();

    let partial_signatures: Vec<MuSigSignature> = signers
        .into_iter()
        .enumerate()
        .map(|(index, signer)| {
            signer.partial_signature(
                &pks,
                &pks,
                &public_comms,
                &(commitments[index].0, commitments[index].1),
                msg,
            )
        })
        .collect();

    // now we aggregate them
    let aggr_sig = MuSigSigner::aggregate_signatures(&partial_signatures);
    let aggr_pk = MuSigSigner::aggregate_keys(&pks, &pks);

    assert!(aggr_sig.verify(&aggr_pk, msg).is_ok());

    ////////////////////////////////////////////////////
    // Now lets test the ATMS signatures with MuSig2. //
    ////////////////////////////////////////////////////

    let n = 10u64;
    let threshold = 8u64;
    let mut rng = OsRng;

    let mut pkeys: Vec<MuSigPk> = Vec::new();
    let mut private_commitments = Vec::new();
    let mut pub_announcements = Vec::new();
    let mut signers: Vec<MuSigSigner> = Vec::new();
    let mut keys_stake = Vec::new();
    for _ in 0..n {
        let signer = MuSigSigner::new(&mut rng);
        let (r_1, r_2, c_1, c_2) = MuSigSigner::commit_randomness(&mut rng);
        private_commitments.push((r_1, r_2));
        pub_announcements.push((c_1, c_2));
        pkeys.push(signer.pk);
        keys_stake.push((signer.pk, 1));
        signers.push(signer);
    }

    let mut signatures: Vec<(PreparedPk, MuSigSignature)> = Vec::new();
    for (index, signer) in signers[..threshold as usize].iter().enumerate() {
        let sig = signer.clone().partial_signature(
            &pkeys,
            &pkeys[..threshold as usize],
            &pub_announcements[..threshold as usize],
            &private_commitments[index],
            msg,
        );
        signatures.push((signer.compute_prepared_pk(&pkeys), sig));
    }

    let avk = match Avk::<MuSig2, Blake2b>::new(&keys_stake, threshold) {
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
