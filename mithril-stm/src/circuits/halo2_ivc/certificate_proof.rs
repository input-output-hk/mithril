//! Off-circuit verifier for the STM cert SNARK proof.
//!
//! Mirrors the in-circuit verification done by the IVC verifier gadget:
//! reads the cert proof's Poseidon transcript, extracts the verifier's
//! `DualMSM`, and pairing-checks it for early abort. The returned `DualMSM`
//! agrees with what the in-circuit verifier gadget produces on the same
//! proof, so it can be wrapped into a certificate accumulator for IVC
//! aggregation.

use group::Group;
use midnight_circuits::hash::poseidon::PoseidonState;
use midnight_curves::{Bls12, G1Projective};
use midnight_proofs::{
    plonk::{VerifyingKey, prepare},
    poly::kzg::{KZGCommitmentScheme, msm::DualMSM, params::ParamsVerifierKZG},
    transcript::{CircuitTranscript, Transcript},
};
use midnight_zk_stdlib::MidnightVK;

use crate::{
    StmResult,
    circuits::{halo2::types::CircuitBase, halo2_ivc::errors::IvcCircuitError},
};

/// Runs the Poseidon-transcript off-circuit verifier for any PLONK proof
/// that was produced under the same transcript and returns the verifier's
/// intermediate `DualMSM`.
///
/// Used by the IVC prover's preparation step to verify both certificate
/// SNARK proofs and previous IVC self-proofs.  Unlike
/// [`verify_and_prepare_accumulator`], this function accepts a raw
/// [`VerifyingKey`] directly instead of a [`MidnightVK`] wrapper, which
/// matches the key type stored in [`IvcSetup`].
///
/// A pairing check on a clone gives cheap early abort; the original
/// `DualMSM` is returned on success for downstream folding.
pub(crate) fn verify_and_prepare_poseidon(
    proof_bytes: &[u8],
    public_inputs: &[CircuitBase],
    verifying_key: &VerifyingKey<CircuitBase, KZGCommitmentScheme<Bls12>>,
    verifier_params: &ParamsVerifierKZG<Bls12>,
) -> StmResult<DualMSM<Bls12>> {
    let mut transcript =
        CircuitTranscript::<PoseidonState<CircuitBase>>::init_from_bytes(proof_bytes);

    let dual_msm = prepare::<
        CircuitBase,
        KZGCommitmentScheme<Bls12>,
        CircuitTranscript<PoseidonState<CircuitBase>>,
    >(
        verifying_key,
        &[&[G1Projective::identity()]],
        &[&[public_inputs]],
        &mut transcript,
    )
    .map_err(|_| IvcCircuitError::CertificateProofRejected)?;

    transcript
        .assert_empty()
        .map_err(|_| IvcCircuitError::CertificateProofRejected)?;

    if !dual_msm.clone().check(verifier_params) {
        return Err(IvcCircuitError::CertificateProofRejected.into());
    }

    Ok(dual_msm)
}

/// Runs the off-circuit verifier for a cert SNARK proof and returns the
/// verifier's intermediate `DualMSM`.
///
/// The transcript hash and public-input encoding match the in-circuit verifier
/// gadget, so the returned `DualMSM` is the off-circuit twin of the
/// accumulator the IVC circuit computes on the same proof. The pairing check
/// on a clone gives cheap early abort; the original `DualMSM` is returned on
/// success for downstream reuse (e.g. wrapping into a certificate accumulator
/// for recursive aggregation).
// Kept until the IVC prover prepares certificate proof accumulators.
#[allow(dead_code)]
pub(crate) fn verify_and_prepare_accumulator(
    proof_bytes: &[u8],
    public_inputs: &[CircuitBase],
    circuit_verification_key: &MidnightVK,
    verifier_params: &ParamsVerifierKZG<Bls12>,
) -> StmResult<DualMSM<Bls12>> {
    let mut transcript =
        CircuitTranscript::<PoseidonState<CircuitBase>>::init_from_bytes(proof_bytes);

    let dual_msm = prepare::<
        CircuitBase,
        KZGCommitmentScheme<Bls12>,
        CircuitTranscript<PoseidonState<CircuitBase>>,
    >(
        circuit_verification_key.vk(),
        // `committed_instances` slot: identity matches what the in-circuit IVC verifier
        // gadget passes, so the off-circuit `DualMSM` is byte-equivalent to its in-circuit twin.
        &[&[G1Projective::identity()]],
        &[&[public_inputs]],
        &mut transcript,
    )
    .map_err(|_| IvcCircuitError::CertificateProofRejected)?;

    transcript
        .assert_empty()
        .map_err(|_| IvcCircuitError::CertificateProofRejected)?;

    if !dual_msm.clone().check(verifier_params) {
        return Err(IvcCircuitError::CertificateProofRejected.into());
    }

    Ok(dual_msm)
}
