//! Tests the composed off-circuit verification path:
//! `verify_prepare` → `dual_msm.check` → `accumulator.check`
//! for the two step types not covered by `golden/positive.rs`.
//!
//! `golden/positive.rs::recursive_step_output_asset_proof_and_accumulator_are_valid`
//! already covers the next-epoch Blake2b proof with both checks in sequence.
//! These tests add the same combined check for:
//!   - the same-epoch Blake2b proof (`same_epoch_step_output`)
//!   - the chain-state Poseidon proof (`recursive_chain_state`)
//!   - the `IvcProof::verify` high-level API path (next-epoch Blake2b proof)
//!
//! Negative tests for `IvcProof::verify` cover tampered proof bytes, a mismatched
//! state, and a mismatched accumulator — each asserting that `verify()` returns `Err`.

use std::marker::PhantomData;

use midnight_circuits::types::Instantiable;

use crate::circuits::halo2_ivc::{
    AssignedAccumulator,
    state::Global,
    tests::common::{
        asset_readers::{
            load_embedded_following_certificate_in_epoch_asset,
            load_embedded_next_epoch_step_output_asset,
            load_embedded_recursive_chain_state_asset,
            load_embedded_verification_context_asset,
        },
        generators::{build_asset_generation_setup, build_recursive_global},
        helpers::{
            verify_prepare_blake2b_recursive_proof, verify_prepare_poseidon_recursive_proof,
        },
    },
    types::IvcProofBytes,
};
use crate::proof_system::ivc_halo2_snark::{proof::IvcProof, verifier_setup::IvcVerifierSetup};

fn build_proof_verifier_context() -> (Global, IvcVerifierSetup) {
    let ctx =
        load_embedded_verification_context_asset().expect("verification context asset should load");
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
fn same_epoch_proof_passes_dual_msm_check_and_accumulator_check() {
    // The stored same-epoch Blake2b proof must pass both the KZG opening check
    // (`dual_msm.check`) and the pairing equation check (`accumulator.check`),
    // confirming the stored same-epoch asset is fully self-consistent.
    let verification_context =
        load_embedded_verification_context_asset().expect("verification context asset should load");
    let same_epoch_step_output = load_embedded_following_certificate_in_epoch_asset()
        .expect("same-epoch step output asset should load");

    let public_inputs = [
        verification_context.global_field_elements.clone(),
        same_epoch_step_output.next_state.as_public_input(),
        AssignedAccumulator::as_public_input(&same_epoch_step_output.next_accumulator),
    ]
    .concat();

    let dual_msm = verify_prepare_blake2b_recursive_proof(
        &verification_context.recursive_verifying_key,
        same_epoch_step_output.ivc_proof.as_bytes(),
        &public_inputs,
    );

    assert!(
        dual_msm.check(&verification_context.verifier_params),
        "same-epoch Blake2b proof should pass the KZG opening check"
    );
    assert!(
        same_epoch_step_output.next_accumulator.check(
            &verification_context.verifier_tau_in_g2,
            &verification_context.combined_fixed_bases,
        ),
        "same-epoch next accumulator should satisfy the pairing equation"
    );
}

#[test]
fn chain_state_proof_passes_dual_msm_check_and_accumulator_check() {
    // The stored chain-state Poseidon proof must pass both the KZG opening check
    // (`dual_msm.check`) and the pairing equation check (`accumulator.check`),
    // confirming the stored chain-state asset is fully self-consistent.
    let verification_context =
        load_embedded_verification_context_asset().expect("verification context asset should load");
    let recursive_chain_state = load_embedded_recursive_chain_state_asset()
        .expect("recursive chain state asset should load");

    let public_inputs = [
        verification_context.global_field_elements.clone(),
        recursive_chain_state.state.as_public_input(),
        AssignedAccumulator::as_public_input(&recursive_chain_state.accumulator),
    ]
    .concat();

    let dual_msm = verify_prepare_poseidon_recursive_proof(
        &verification_context.recursive_verifying_key,
        recursive_chain_state.ivc_proof.as_bytes(),
        &public_inputs,
    );

    assert!(
        dual_msm.check(&verification_context.verifier_params),
        "chain-state Poseidon proof should pass the KZG opening check"
    );
    assert!(
        recursive_chain_state.accumulator.check(
            &verification_context.verifier_tau_in_g2,
            &verification_context.combined_fixed_bases,
        ),
        "chain-state accumulator should satisfy the pairing equation"
    );
}

#[test]
fn ivc_proof_verify_accepts_stored_recursive_step_output() {
    // Exercises the `IvcProof::verify` high-level API end-to-end against the
    // stored next-epoch Blake2b proof, confirming that the unified verification
    // path (KZG opening + accumulator pairing) accepts a known-good proof.
    let verification_context =
        load_embedded_verification_context_asset().expect("verification context asset should load");
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
