//! Tests the full off-circuit KZG accumulation pipeline:
//! `from_dual_msm` → `collapse` → `Accumulator::accumulate` → `collapse` → `accumulator.check`.
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
use midnight_proofs::poly::kzg::params::ParamsVerifierKZG;

use crate::circuits::halo2_ivc::{
    Accumulator, EmulatedCurve, Msm, PairingEngine, RecursiveEmulation,
    tests::common::{
        asset_readers::load_embedded_recursive_chain_state_asset,
        helpers::{
            build_certificate_accumulator_from_assets,
            build_recursive_proof_accumulator_from_assets,
        },
    },
};

/// Verifies the stored certificate and chain-state IVC proofs, applies `collapse` to each, and returns
/// `(certificate_accumulator, previous_proof_accumulator, combined_fixed_bases, verif_params)`.
///
/// This is the shared setup for both tests: the positive test folds these with
/// the stored chain-state accumulator; the negative test folds them with a
/// tampered one.
///
/// Intermediate `dual_msm.check` calls are intentionally omitted: they require
/// KZG verifier parameters and are covered by
/// `golden/positive.rs::replay_integrity_matches_stored_next_step_output`.
fn build_proof_accumulators_from_stored_step_assets() -> (
    Accumulator<RecursiveEmulation>,
    Accumulator<RecursiveEmulation>,
    BTreeMap<String, EmulatedCurve>,
    ParamsVerifierKZG<PairingEngine>,
) {
    let (mut certificate_accumulator, certificate_fixed_bases, verif_params) =
        build_certificate_accumulator_from_assets();
    certificate_accumulator.collapse();

    let (mut previous_proof_accumulator, recursive_fixed_bases) =
        build_recursive_proof_accumulator_from_assets();
    previous_proof_accumulator.collapse();

    let combined_fixed_bases: BTreeMap<String, EmulatedCurve> = certificate_fixed_bases
        .into_iter()
        .chain(recursive_fixed_bases)
        .collect();

    (
        certificate_accumulator,
        previous_proof_accumulator,
        combined_fixed_bases,
        verif_params,
    )
}

/// Runs the full off-circuit accumulation pipeline on stored step assets and returns
/// `(next_accumulator, combined_fixed_bases, verif_params)`.
fn build_next_accumulator_from_stored_step_assets() -> (
    Accumulator<RecursiveEmulation>,
    BTreeMap<String, EmulatedCurve>,
    ParamsVerifierKZG<PairingEngine>,
) {
    let recursive_chain_state = load_embedded_recursive_chain_state_asset()
        .expect("recursive chain state asset should load");
    let (certificate_accumulator, previous_proof_accumulator, combined_fixed_bases, verif_params) =
        build_proof_accumulators_from_stored_step_assets();

    let mut next_accumulator = Accumulator::accumulate(&[
        recursive_chain_state.accumulator.clone(),
        certificate_accumulator,
        previous_proof_accumulator,
    ]);
    next_accumulator.collapse();

    (next_accumulator, combined_fixed_bases, verif_params)
}

#[test]
fn stored_step_pipeline_result_passes_accumulator_check() {
    // The full off-circuit pipeline on stored step assets must produce an accumulator
    // that satisfies the pairing equation.
    let (next_accumulator, combined_fixed_bases, verif_params) =
        build_next_accumulator_from_stored_step_assets();

    assert!(
        next_accumulator.check(&verif_params, &combined_fixed_bases),
        "folded next accumulator should satisfy the pairing equation"
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
    let recursive_chain_state = load_embedded_recursive_chain_state_asset()
        .expect("recursive chain state asset should load");
    let (certificate_accumulator, previous_proof_accumulator, combined_fixed_bases, verif_params) =
        build_proof_accumulators_from_stored_step_assets();

    // Tamper the chain-state accumulator by negating one fixed-base scalar.
    let right_hand_side = recursive_chain_state.accumulator.rhs();
    let mut tampered_fixed_base_scalars = right_hand_side.fixed_base_scalars();
    let key_to_negate = tampered_fixed_base_scalars
        .iter()
        .find(|(key, scalar)| {
            !scalar.is_zero_vartime()
                && combined_fixed_bases
                    .get(*key)
                    .map(|base| *base != EmulatedCurve::default())
                    .unwrap_or(false)
        })
        .map(|(key, _)| key.clone())
        .expect("chain-state accumulator should have at least one non-zero scalar with a non-trivial base");
    tampered_fixed_base_scalars
        .entry(key_to_negate)
        .and_modify(|scalar| *scalar = scalar.neg());
    let tampered_right_hand_side = Msm::<RecursiveEmulation>::new(
        &right_hand_side.bases(),
        &right_hand_side.scalars(),
        &tampered_fixed_base_scalars,
    );
    let tampered_chain_accumulator = Accumulator::<RecursiveEmulation>::new(
        recursive_chain_state.accumulator.lhs(),
        tampered_right_hand_side,
    );
    assert!(
        !tampered_chain_accumulator.check(&verif_params, &combined_fixed_bases),
        "tamper precondition: tampered chain accumulator should fail check on its own"
    );

    let mut next_accumulator = Accumulator::accumulate(&[
        tampered_chain_accumulator,
        certificate_accumulator,
        previous_proof_accumulator,
    ]);
    next_accumulator.collapse();

    assert!(
        !next_accumulator.check(&verif_params, &combined_fixed_bases),
        "folding an invalid chain-state accumulator should not satisfy the pairing equation"
    );
}
