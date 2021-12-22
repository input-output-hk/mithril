//! In this example we provide a simple instance of MuSig2 using Ristretto, and use it to
//! instantiate ATMS while being compatible with Schnorr-signatures. For sake of simplicity,
//! we make this example to be interactive. However, MuSig2 can be made non-interactive with
//! a pre-computation phase.
//!
//! # DISCLAIMER
//! This implementation of MuSig2 is not secure. It is a minimal working version to show how ATMS
//! can be instantiated with it, and as a consequence adapt ATMS to be more generic.

use blake2::{Blake2b, Digest};
use curve25519_dalek::constants::RISTRETTO_BASEPOINT_POINT;
use curve25519_dalek::ristretto::RistrettoPoint;
use curve25519_dalek::scalar::Scalar;
use curve25519_dalek::traits::Identity;
use rand::{CryptoRng, RngCore};

#[derive(Debug)]
struct MuSig2;

type MuSigPk = RistrettoPoint;

#[derive(Clone)]
struct MuSigSigner {
    sk: Scalar,
    pk: MuSigPk,
}

struct MuSigSignature {
    announcement: RistrettoPoint,
    response: Scalar,
}

impl MuSigSigner {
    fn new<R>(rng: &mut R) -> Self
    where
        R: CryptoRng + RngCore,
    {
        let sk = Scalar::random(rng);
        let pk = sk * RISTRETTO_BASEPOINT_POINT;

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
        pks: &[RistrettoPoint],
        committed_randomness: &[(RistrettoPoint, RistrettoPoint)],
        private_randomness: (Scalar, Scalar),
        message: &[u8],
    ) -> MuSigSignature {
        assert_eq!(pks.len(), committed_randomness.len());

        let aggr_key = Self::aggregate_keys(pks);
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
            .chain(aggr_key.compress().as_bytes())
            .chain(flatted_commitments)
            .chain(message);
        let b_2 = Scalar::from_hash(hash_b_2);

        let announcement = added_commit_1 + b_2 * added_commit_2;
        let challenge = Scalar::from_hash(
            Blake2b::new()
                .chain(aggr_key.compress().as_bytes())
                .chain(announcement.compress().as_bytes())
                .chain(message),
        );

        let mut hash = Blake2b::new();
        for pk in pks {
            hash.update(pk.compress().as_bytes());
        }

        hash.update(self.pk.compress().as_bytes());

        let ai = Scalar::from_hash(hash);
        let response = challenge * ai * self.sk + private_randomness.0 + private_randomness.1 * b_2;

        MuSigSignature {
            announcement,
            response,
        }
    }

    fn aggregate_signatures(sigs: &[MuSigSignature]) -> MuSigSignature {
        let mut aggr_response = Scalar::zero();
        for sig in sigs {
            aggr_response += sig.response;
        }

        MuSigSignature {
            announcement: sigs[0].announcement,
            response: aggr_response,
        }
    }

    /// todo: This function MUST ORDER THE PUBLIC KEYS!!!! We don't do it because we are simply performing
    /// a dummy test. But for this to work ONE NEEDS TO DEFINE A DETERMINISTIC MECHANISM TO ORDER
    /// THE PUBLIC KEYS.
    fn aggregate_keys(pks: &[RistrettoPoint]) -> RistrettoPoint {
        let mut hash = Blake2b::new();
        for pk in pks {
            hash.update(pk.compress().as_bytes());
        }

        let mut aggr_key = RistrettoPoint::identity();
        for pk in pks {
            let mut hasheri = hash.clone();
            hasheri.update(pk.compress().as_bytes());
            aggr_key += Scalar::from_hash(hasheri) * pk;
        }
        aggr_key
    }
}

impl MuSigSignature {
    fn verify(&self, pk: &MuSigPk, message: &[u8]) -> Result<(), ()> {
        let lhs = self.response * RISTRETTO_BASEPOINT_POINT;

        let challenge = Scalar::from_hash(
            Blake2b::new()
                .chain(pk.compress().as_bytes())
                .chain(self.announcement.compress().as_bytes())
                .chain(message),
        );
        let rhs = challenge * pk + self.announcement;

        if lhs != rhs {
            return Err(());
        }

        Ok(())
    }
}

/// Now we implement Atms for MuSig2
use super::*;
use crate::msp::{Msp, MspMvk, MspPk, MspSig};
use ark_ec::PairingEngine;
use mithril::atms::Atms;

impl Atms for MuSig2 {
    type PreCheckedPK = MuSigPk;
    type CheckedPK = MuSigPk;
    type SIG = MuSigSignature;

    fn check_key(proof: &Self::PreCheckedPK) -> Option<Self::CheckedPK> {
        Some(*proof)
    }

    fn verify(msg: &[u8], pk: &Self::CheckedPK, sig: &Self::SIG) -> bool {
        sig.verify(pk, msg).is_ok()
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
    let pks: Vec<RistrettoPoint> = signers
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
                &public_comms,
                (commitments[index].0, commitments[index].clone().1),
                msg,
            )
        })
        .collect();

    // now we aggregate them
    let aggr_sig = MuSigSigner::aggregate_signatures(&partial_signatures);
    let aggr_pk = MuSigSigner::aggregate_keys(&pks);

    assert!(aggr_sig.verify(aggr_pk, msg).is_ok());
}
