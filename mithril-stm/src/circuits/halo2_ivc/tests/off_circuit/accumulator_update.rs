//! Tests the full off-circuit KZG accumulation pipeline:
//! `extract_fixed_bases` → `collapse` → `Accumulator::accumulate` → `collapse` → `accumulator.check`.
//!
//! Each recursive step folds three contributions into a single accumulated result:
//!   1. The chain-state accumulator carried forward from the previous step.
//!   2. The certificate accumulator derived from the certificate proof for this step.
//!   3. The previous-recursive-proof accumulator derived from the chain-state IVC proof.
//!
//! Both tests use only stored assets — no SRS generation required.
//! Determinism (pipeline result matches stored `next_accumulator` byte-for-byte) and
//! intermediate `dual_msm.check` coverage are provided by
//! `golden/positive.rs::replay_integrity_matches_stored_next_step_output`.

use std::collections::BTreeMap;
use std::ops::Neg;

use ff::Field;
use midnight_circuits::types::Instantiable;
use midnight_curves::pairing::Engine;

use crate::circuits::halo2_ivc::{
    Accumulator, AssignedAccumulator, C, E, Msm, S,
    tests::common::{
        asset_readers::{
            load_embedded_recursive_chain_state_asset, load_embedded_recursive_step_output_asset,
            load_embedded_verification_context_asset,
        },
        generators::{build_recursive_fixed_bases, certificate_public_inputs_for_step},
        helpers::verify_prepare_poseidon_recursive_proof,
    },
};

/// Runs the full off-circuit accumulation pipeline on stored step assets and returns
/// the folded and collapsed next accumulator together with the combined fixed bases
/// and `tau_in_g2` needed to call `accumulator.check`.
///
/// Intermediate `dual_msm.check` calls are intentionally omitted: they require
/// KZG verifier parameters and are covered by
/// `golden/positive.rs::replay_integrity_matches_stored_next_step_output`.
fn build_next_accumulator_from_stored_step_assets()
-> (Accumulator<S>, BTreeMap<String, C>, <E as Engine>::G2Affine) {
    let verification_context =
        load_embedded_verification_context_asset().expect("verification context asset should load");
    let recursive_chain_state = load_embedded_recursive_chain_state_asset()
        .expect("recursive chain state asset should load");
    let recursive_step_output = load_embedded_recursive_step_output_asset()
        .expect("recursive step output asset should load");

    let (certificate_fixed_bases, recursive_fixed_bases, combined_fixed_bases) =
        build_recursive_fixed_bases(
            &verification_context.certificate_verifying_key,
            &verification_context.recursive_verifying_key,
        );

    // Build the certificate accumulator for this step.
    let certificate_public_inputs = certificate_public_inputs_for_step(
        &recursive_chain_state.state,
        &recursive_step_output.next_state,
    );
    let mut certificate_accumulator: Accumulator<S> = verify_prepare_poseidon_recursive_proof(
        verification_context.certificate_verifying_key.vk(),
        &recursive_step_output.certificate_proof,
        &certificate_public_inputs,
    )
    .into();
    certificate_accumulator.extract_fixed_bases(&certificate_fixed_bases);
    certificate_accumulator.collapse();

    // Build the previous-recursive-proof accumulator from the chain-state IVC proof.
    let chain_state_proof_public_inputs = [
        verification_context.global_field_elements.clone(),
        recursive_chain_state.state.as_public_input(),
        AssignedAccumulator::as_public_input(&recursive_chain_state.accumulator),
    ]
    .concat();
    let mut previous_proof_accumulator: Accumulator<S> = verify_prepare_poseidon_recursive_proof(
        &verification_context.recursive_verifying_key,
        &recursive_chain_state.proof,
        &chain_state_proof_public_inputs,
    )
    .into();
    previous_proof_accumulator.extract_fixed_bases(&recursive_fixed_bases);
    previous_proof_accumulator.collapse();

    // Fold all three contributions and collapse.
    let mut next_accumulator = Accumulator::accumulate(&[
        recursive_chain_state.accumulator.clone(),
        certificate_accumulator,
        previous_proof_accumulator,
    ]);
    next_accumulator.collapse();

    (
        next_accumulator,
        combined_fixed_bases,
        verification_context.verifier_tau_in_g2,
    )
}

#[test]
fn stored_step_pipeline_result_passes_accumulator_check() {
    // The full off-circuit pipeline on stored step assets must produce an accumulator
    // that satisfies the pairing equation.
    let (next_accumulator, combined_fixed_bases, tau_in_g2) =
        build_next_accumulator_from_stored_step_assets();

    assert!(
        next_accumulator.check(&tau_in_g2, &combined_fixed_bases),
        "folded next accumulator must satisfy the pairing equation",
    );
}

#[test]
fn pipeline_with_invalid_previous_accumulator_fails_accumulator_check() {
    // Negating one fixed-base scalar in the stored chain-state accumulator makes it
    // mathematically invalid. Folding an invalid accumulator must propagate the
    // invalidity through the random Poseidon challenge: the folded result must fail
    // the pairing equation check.
    //
    // The target scalar must be non-zero and its corresponding base in
    // combined_fixed_bases must be a non-trivial curve point (same conditions as
    // the tamper test in accumulator_verification.rs).
    let verification_context =
        load_embedded_verification_context_asset().expect("verification context asset should load");
    let recursive_chain_state = load_embedded_recursive_chain_state_asset()
        .expect("recursive chain state asset should load");
    let recursive_step_output = load_embedded_recursive_step_output_asset()
        .expect("recursive step output asset should load");

    let (certificate_fixed_bases, recursive_fixed_bases, combined_fixed_bases) =
        build_recursive_fixed_bases(
            &verification_context.certificate_verifying_key,
            &verification_context.recursive_verifying_key,
        );

    // Tamper the chain-state accumulator by negating one fixed-base scalar.
    let right_hand_side = recursive_chain_state.accumulator.rhs();
    let mut tampered_fixed_base_scalars = right_hand_side.fixed_base_scalars();
    let key_to_negate = tampered_fixed_base_scalars
        .iter()
        .find(|(key, scalar)| {
            !scalar.is_zero_vartime()
                && combined_fixed_bases
                    .get(*key)
                    .map(|base| *base != C::default())
                    .unwrap_or(false)
        })
        .map(|(key, _)| key.clone())
        .expect("chain-state accumulator must have at least one non-zero scalar with a non-trivial base");
    tampered_fixed_base_scalars
        .entry(key_to_negate)
        .and_modify(|scalar| *scalar = scalar.neg());
    let tampered_right_hand_side = Msm::<S>::new(
        &right_hand_side.bases(),
        &right_hand_side.scalars(),
        &tampered_fixed_base_scalars,
    );
    let tampered_chain_accumulator =
        Accumulator::<S>::new(recursive_chain_state.accumulator.lhs(), tampered_right_hand_side);

    let certificate_public_inputs = certificate_public_inputs_for_step(
        &recursive_chain_state.state,
        &recursive_step_output.next_state,
    );
    let mut certificate_accumulator: Accumulator<S> = verify_prepare_poseidon_recursive_proof(
        verification_context.certificate_verifying_key.vk(),
        &recursive_step_output.certificate_proof,
        &certificate_public_inputs,
    )
    .into();
    certificate_accumulator.extract_fixed_bases(&certificate_fixed_bases);
    certificate_accumulator.collapse();

    let chain_state_proof_public_inputs = [
        verification_context.global_field_elements.clone(),
        recursive_chain_state.state.as_public_input(),
        AssignedAccumulator::as_public_input(&recursive_chain_state.accumulator),
    ]
    .concat();
    let mut previous_proof_accumulator: Accumulator<S> = verify_prepare_poseidon_recursive_proof(
        &verification_context.recursive_verifying_key,
        &recursive_chain_state.proof,
        &chain_state_proof_public_inputs,
    )
    .into();
    previous_proof_accumulator.extract_fixed_bases(&recursive_fixed_bases);
    previous_proof_accumulator.collapse();

    let mut next_accumulator = Accumulator::accumulate(&[
        tampered_chain_accumulator,
        certificate_accumulator,
        previous_proof_accumulator,
    ]);
    next_accumulator.collapse();

    assert!(
        !next_accumulator.check(
            &verification_context.verifier_tau_in_g2,
            &combined_fixed_bases,
        ),
        "folding an invalid chain-state accumulator must not satisfy the pairing equation",
    );
}
