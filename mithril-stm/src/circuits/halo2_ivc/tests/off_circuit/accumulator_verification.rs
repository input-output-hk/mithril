//! Tests that `accumulator.check` accepts valid stored folded accumulators and
//! rejects tampered ones.
//!
//! The genesis `next_accumulator` is a `trivial_acc` clone (never folded) and is
//! excluded here — its structure is covered by C2.1 `accumulator_construction`.
//! All accumulators tested here were produced by the full off-circuit folding
//! pipeline and stored in binary assets.

use ff::Field;
use std::ops::Neg;

use crate::circuits::halo2_ivc::{
    Accumulator, C, Msm, S,
    tests::common::asset_readers::{
        load_embedded_recursive_chain_state_asset, load_embedded_recursive_step_output_asset,
        load_embedded_same_epoch_step_output_asset, load_embedded_verification_context_asset,
    },
};

#[test]
fn same_epoch_accumulator_passes_check() {
    // The same-epoch step output carries a folded accumulator produced after
    // one intra-epoch recursive step. It must satisfy the pairing equation.
    let verification_context =
        load_embedded_verification_context_asset().expect("verification context asset should load");
    let same_epoch_step_output = load_embedded_same_epoch_step_output_asset()
        .expect("same-epoch step output asset should load");

    assert!(
        same_epoch_step_output.next_accumulator.check(
            &verification_context.verifier_tau_in_g2,
            &verification_context.combined_fixed_bases,
        ),
        "same-epoch accumulator should satisfy the pairing equation"
    );
}

#[test]
fn next_epoch_accumulator_passes_check() {
    // The next-epoch (recursive) step output carries a folded accumulator
    // produced after an epoch-boundary recursive step. It must satisfy the
    // pairing equation.
    let verification_context =
        load_embedded_verification_context_asset().expect("verification context asset should load");
    let recursive_step_output = load_embedded_recursive_step_output_asset()
        .expect("recursive step output asset should load");

    assert!(
        recursive_step_output.next_accumulator.check(
            &verification_context.verifier_tau_in_g2,
            &verification_context.combined_fixed_bases,
        ),
        "next-epoch accumulator should satisfy the pairing equation"
    );
}

#[test]
fn chain_state_accumulator_passes_check() {
    // The recursive chain state stores the folded accumulator checkpoint used
    // as the starting point for the next recursive step. It must satisfy the
    // pairing equation.
    let verification_context =
        load_embedded_verification_context_asset().expect("verification context asset should load");
    let recursive_chain_state = load_embedded_recursive_chain_state_asset()
        .expect("recursive chain state asset should load");

    assert!(
        recursive_chain_state.accumulator.check(
            &verification_context.verifier_tau_in_g2,
            &verification_context.combined_fixed_bases,
        ),
        "chain-state accumulator should satisfy the pairing equation"
    );
}

#[test]
fn tampered_accumulator_fails_check() {
    // Negating one fixed-base scalar on the right-hand side changes the rhs MSM
    // evaluation and breaks the pairing equation. accumulator.check must return false.
    //
    // The target scalar must satisfy two conditions (detailed in the search below):
    // it must be non-zero, and its corresponding base in combined_fixed_bases must
    // be a non-trivial curve point. Either violation leaves the MSM unchanged.
    let verification_context =
        load_embedded_verification_context_asset().expect("verification context asset should load");
    let recursive_step_output = load_embedded_recursive_step_output_asset()
        .expect("recursive step output asset should load");

    let original_accumulator = &recursive_step_output.next_accumulator;

    let right_hand_side = original_accumulator.rhs();
    let mut tampered_fixed_base_scalars = right_hand_side.fixed_base_scalars();

    // Find a key whose scalar is non-zero AND whose corresponding base in
    // combined_fixed_bases is a non-trivial curve point. Negating a scalar
    // for an identity base would leave the MSM evaluation unchanged.
    let key_to_negate = tampered_fixed_base_scalars
        .iter()
        .find(|(key, scalar)| {
            !scalar.is_zero_vartime()
                && verification_context
                    .combined_fixed_bases
                    .get(*key)
                    .map(|base| *base != C::default())
                    .unwrap_or(false)
        })
        .map(|(key, _)| key.clone())
        .expect("next_accumulator must have at least one non-zero scalar with a non-trivial base");
    tampered_fixed_base_scalars
        .entry(key_to_negate)
        .and_modify(|scalar| *scalar = scalar.neg());

    let tampered_right_hand_side = Msm::<S>::new(
        &right_hand_side.bases(),
        &right_hand_side.scalars(),
        &tampered_fixed_base_scalars,
    );
    let tampered_accumulator =
        Accumulator::<S>::new(original_accumulator.lhs(), tampered_right_hand_side);

    assert!(
        !tampered_accumulator.check(
            &verification_context.verifier_tau_in_g2,
            &verification_context.combined_fixed_bases,
        ),
        "tampered accumulator should not satisfy the pairing equation"
    );
}
