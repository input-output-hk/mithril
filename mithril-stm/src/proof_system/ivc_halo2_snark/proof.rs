//! `IvcProver` and `IvcProof`: the proving-session handle and its emitted IVC proof.

use std::{marker::PhantomData, sync::Arc};

use group::Group;
use midnight_circuits::{
    types::Instantiable,
    verifier::{Accumulator, AssignedAccumulator, BlstrsEmulation},
};
use midnight_curves::{Bls12, G1Projective};
use midnight_proofs::{
    plonk::prepare,
    poly::{commitment::PolynomialCommitmentScheme, kzg::KZGCommitmentScheme},
    transcript::{CircuitTranscript, Hashable, Sampleable, Transcript, TranscriptHash},
};
use rand_core::{CryptoRng, RngCore};

use crate::{
    StmResult,
    circuits::{
        halo2::types::CircuitBase,
        halo2_ivc::{
            errors::IvcCircuitError,
            state::{Global, State},
            types::IvcProofBytes,
        },
    },
    proof_system::ivc_halo2_snark::{setup::IvcSetup, verifier_setup::IvcVerifierSetup},
};

/// Per-session IVC prover handle.
// TODO: remove this allow dead_code directive when the IVC prover is wired into STM
#[allow(dead_code)]
pub(crate) struct IvcProver<R: RngCore + CryptoRng> {
    /// Shared, cached setup (SRS, verifying keys, proving key, fixed-base maps).
    pub(crate) ivc_setup: Arc<IvcSetup>,
    /// Randomness source used during proof generation.
    pub(crate) rng: R,
}

/// IVC proof emitted at the end of a proving step.
///
/// `H` is the transcript hash used to produce this proof and must be used to verify it.
/// It is a zero-cost phantom: no `H`-dependent data is stored, but it prevents accidentally
/// verifying a Poseidon-produced proof via the Blake2b path and vice versa.
// TODO: remove this allow dead_code directive when the IVC prover emits this proof
#[allow(dead_code)]
pub(crate) struct IvcProof<H> {
    /// Externally-verifiable proof bytes.
    pub(crate) proof_bytes: IvcProofBytes,
    /// Chain state the proof commits to.
    pub(crate) state: State,
    /// Folded accumulator the proof commits to.
    pub(crate) accumulator: Accumulator<BlstrsEmulation>,
    /// Phantom marker tying the proof to its transcript hash type.
    pub(crate) _hash: PhantomData<H>,
}

// TODO: remove this allow dead_code directive when IvcProof::verify is called
#[allow(dead_code)]
impl<H> IvcProof<H>
where
    H: TranscriptHash,
    CircuitBase: Sampleable<H> + Hashable<H>,
    <KZGCommitmentScheme<Bls12> as PolynomialCommitmentScheme<CircuitBase>>::Commitment:
        Hashable<H>,
{
    /// Verifies the IVC proof and its folded accumulator using transcript hash `H`.
    ///
    /// Checks:
    /// 1. The KZG opening equations against the IVC verifying key.
    /// 2. The folded accumulator pairing equation.
    ///
    /// # Invariant
    ///
    /// `global` and `verifier_setup` must be built from the same certificate and IVC verifying
    /// keys. If they differ, the public inputs fed to the KZG opening check will not match the
    /// proof transcript and verification will return [`IvcCircuitError::RecursiveProofKzgOpeningFailed`].
    pub(crate) fn verify(
        &self,
        global: &Global,
        verifier_setup: &IvcVerifierSetup,
    ) -> StmResult<()> {
        let public_inputs: Vec<CircuitBase> = [
            global.as_public_input(),
            self.state.as_public_input(),
            AssignedAccumulator::as_public_input(&self.accumulator),
        ]
        .concat();

        let mut transcript = CircuitTranscript::<H>::init_from_bytes(self.proof_bytes.as_bytes());

        let dual_msm = prepare::<CircuitBase, KZGCommitmentScheme<Bls12>, CircuitTranscript<H>>(
            &verifier_setup.ivc_verifying_key,
            &[&[G1Projective::identity()]],
            &[&[&public_inputs]],
            &mut transcript,
        )
        .map_err(|_| IvcCircuitError::RecursiveProofTranscriptPreparationFailed)?;

        transcript
            .assert_empty()
            .map_err(|_| IvcCircuitError::RecursiveProofTranscriptNotFullyConsumed)?;

        if !dual_msm.check(&verifier_setup.verifier_params) {
            return Err(IvcCircuitError::RecursiveProofKzgOpeningFailed.into());
        }

        if !self.accumulator.check(&verifier_setup.tau_g2, &verifier_setup.combined_fixed_bases) {
            return Err(IvcCircuitError::RecursiveProofAccumulatorFailed.into());
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use std::marker::PhantomData;

    use crate::{
        circuits::halo2_ivc::{
            state::Global,
            tests::common::{
                asset_readers::{
                    load_embedded_following_certificate_in_epoch_asset,
                    load_embedded_next_epoch_step_output_asset,
                    load_embedded_recursive_chain_state_asset,
                    load_embedded_verification_context_asset,
                },
                generators::{build_asset_generation_setup, build_recursive_global},
            },
            types::IvcProofBytes,
        },
        proof_system::ivc_halo2_snark::verifier_setup::IvcVerifierSetup,
    };

    use super::IvcProof;

    fn build_proof_verifier_context() -> (Global, IvcVerifierSetup) {
        let ctx = load_embedded_verification_context_asset()
            .expect("verification context asset should load");
        let setup = build_asset_generation_setup();
        let global = build_recursive_global(
            &setup,
            &ctx.certificate_verifying_key,
            &ctx.recursive_verifying_key,
        );
        let verifier_setup = IvcVerifierSetup {
            verifier_params: ctx.verifier_params,
            tau_g2: ctx.verifier_tau_in_g2,
            ivc_verifying_key: ctx.recursive_verifying_key,
            combined_fixed_bases: ctx.combined_fixed_bases,
        };
        (global, verifier_setup)
    }

    #[test]
    fn ivc_proof_verify_accepts_stored_recursive_step_output() {
        // Exercises the `IvcProof::verify` high-level API end-to-end against the
        // stored next-epoch Blake2b proof, confirming that the unified verification
        // path (KZG opening + accumulator pairing) accepts a known-good proof.
        let verification_context = load_embedded_verification_context_asset()
            .expect("verification context asset should load");
        let step_output = load_embedded_next_epoch_step_output_asset()
            .expect("recursive step output asset should load");

        let setup = build_asset_generation_setup();
        let global = build_recursive_global(
            &setup,
            &verification_context.certificate_verifying_key,
            &verification_context.recursive_verifying_key,
        );

        let verifier_setup = IvcVerifierSetup {
            verifier_params: verification_context.verifier_params,
            tau_g2: verification_context.verifier_tau_in_g2,
            ivc_verifying_key: verification_context.recursive_verifying_key,
            combined_fixed_bases: verification_context.combined_fixed_bases,
        };

        let proof = IvcProof::<blake2b_simd::State> {
            proof_bytes: step_output.ivc_proof,
            state: step_output.next_state,
            accumulator: step_output.next_accumulator,
            _hash: PhantomData,
        };

        proof
            .verify(&global, &verifier_setup)
            .expect("stored recursive step output should pass IvcProof::verify");
    }

    #[test]
    fn ivc_proof_verify_rejects_tampered_proof_bytes() {
        // A single flipped byte anywhere in the proof transcript must cause `verify` to
        // return `Err`: the KZG opening equations are computed over the raw bytes, so
        // any corruption propagates to a bad MSM and the `dual_msm.check` fails.
        let (global, verifier_setup) = build_proof_verifier_context();
        let step_output = load_embedded_next_epoch_step_output_asset()
            .expect("recursive step output asset should load");

        let mut tampered_bytes = step_output.ivc_proof.as_bytes().to_vec();
        let mid = tampered_bytes.len() / 2;
        tampered_bytes[mid] ^= 0xff;

        let proof = IvcProof::<blake2b_simd::State> {
            proof_bytes: IvcProofBytes::new(tampered_bytes),
            state: step_output.next_state,
            accumulator: step_output.next_accumulator,
            _hash: PhantomData,
        };

        assert!(
            proof.verify(&global, &verifier_setup).is_err(),
            "tampered proof bytes should be rejected by IvcProof::verify"
        );
    }

    #[test]
    fn ivc_proof_verify_rejects_mismatched_state() {
        // Substituting the state from a different proof step changes the public inputs
        // fed to `prepare`, causing `dual_msm.check` to fail against the unmodified
        // proof bytes.
        let (global, verifier_setup) = build_proof_verifier_context();
        let step_output = load_embedded_next_epoch_step_output_asset()
            .expect("recursive step output asset should load");
        let same_epoch = load_embedded_following_certificate_in_epoch_asset()
            .expect("same-epoch step output asset should load");

        let proof = IvcProof::<blake2b_simd::State> {
            proof_bytes: step_output.ivc_proof,
            state: same_epoch.next_state,
            accumulator: step_output.next_accumulator,
            _hash: PhantomData,
        };

        assert!(
            proof.verify(&global, &verifier_setup).is_err(),
            "state from a different proof should be rejected by IvcProof::verify"
        );
    }

    #[test]
    fn ivc_proof_verify_rejects_mismatched_accumulator() {
        // Substituting the accumulator from a different proof step corrupts the public
        // inputs (accumulator is part of them) and also fails the pairing equation
        // check, so `verify` must return `Err` on both counts.
        let (global, verifier_setup) = build_proof_verifier_context();
        let step_output = load_embedded_next_epoch_step_output_asset()
            .expect("recursive step output asset should load");
        let same_epoch = load_embedded_following_certificate_in_epoch_asset()
            .expect("same-epoch step output asset should load");

        let proof = IvcProof::<blake2b_simd::State> {
            proof_bytes: step_output.ivc_proof,
            state: step_output.next_state,
            accumulator: same_epoch.next_accumulator,
            _hash: PhantomData,
        };

        assert!(
            proof.verify(&global, &verifier_setup).is_err(),
            "accumulator from a different proof should be rejected by IvcProof::verify"
        );
    }

    #[test]
    fn ivc_proof_verify_rejects_poseidon_proof_bytes() {
        // Constructing an `IvcProof<blake2b_simd::State>` with Poseidon-transcript bytes
        // and verifying it with the Blake2b path must fail: the two transcript formats
        // are not interchangeable.
        let (global, verifier_setup) = build_proof_verifier_context();
        let chain_state = load_embedded_recursive_chain_state_asset()
            .expect("recursive chain state asset should load");

        let proof = IvcProof::<blake2b_simd::State> {
            proof_bytes: chain_state.ivc_proof,
            state: chain_state.state,
            accumulator: chain_state.accumulator,
            _hash: PhantomData,
        };

        assert!(
            proof.verify(&global, &verifier_setup).is_err(),
            "Poseidon proof bytes should be rejected by IvcProof::<Blake2b>::verify"
        );
    }
}
