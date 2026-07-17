//! Tests that `resolve_fixed_bases` correctly populates the variable-base part
//! of the accumulator from the known fixed bases, observable via changes in the
//! public-input encoding length and composition.
//!
//! An accumulator built by `Accumulator::from_dual_msm` carries its KZG
//! polynomial-commitment bases as fixed-base scalars on both sides.
//! Calling `resolve_fixed_bases` expands each fixed-base scalar back into a
//! full variable-base term (base point + scalar), growing `as_public_input` by
//! `N * field_elements_per_base_point`. In this application, the provided
//! fixed-bases map only contains keys for rhs (right-hand side) entries, so
//! the lhs (left-hand side) MSM is left unchanged. The call is idempotent:
//! a second call with the same map is a no-op.

use ff::Field;
use midnight_circuits::{types::Instantiable, verifier::AssignedMsm};

use crate::circuits::halo2_ivc::{
    AssignedAccumulator, RecursiveEmulation, accumulator::trivial_accumulator,
    tests::common::helpers::build_recursive_proof_accumulator_from_assets,
};

#[test]
fn resolve_fixed_bases_grows_public_input_length_by_n_times_field_elements_per_base_point() {
    // Each resolved term takes one fixed-base scalar and expands it back into a
    // variable-base term: adds one base point (field_elements_per_base_point field
    // elements) and one scalar, while removing one fixed-base scalar entry.
    // Net growth = field_elements_per_base_point per resolved base.
    //
    // field_elements_per_base_point is derived from trivial_acc, which has exactly one base
    // and one scalar per side (left-hand side and right-hand side) with no fixed-base entries:
    // trivial_acc(&[]).len() == 2 * (field_elements_per_base_point + 1)
    let (mut accumulator, recursive_fixed_bases) = build_recursive_proof_accumulator_from_assets();
    let fixed_base_scalar_count =
        accumulator.lhs().fixed_base_scalars().len() + accumulator.rhs().fixed_base_scalars().len();
    let trivial_accumulator_encoding_length =
        AssignedAccumulator::as_public_input(&trivial_accumulator(&[])).len();
    let field_elements_per_base_point = trivial_accumulator_encoding_length / 2 - 1;

    let encoding_length_before_resolution =
        AssignedAccumulator::as_public_input(&accumulator).len();
    accumulator.resolve_fixed_bases(&recursive_fixed_bases);
    let encoding_length_after_resolution = AssignedAccumulator::as_public_input(&accumulator).len();

    assert_eq!(
        encoding_length_after_resolution - encoding_length_before_resolution,
        fixed_base_scalar_count * field_elements_per_base_point,
        "resolve_fixed_bases should grow as_public_input length by field_elements_per_base_point per resolved base"
    );
}

#[test]
fn resolved_fixed_base_scalars_are_non_trivial() {
    // The fixed-base scalars were populated by `from_dual_msm` and are KZG challenge-derived
    // values; at least one must be non-zero, confirming real IVC computation is committed.
    let (mut accumulator, recursive_fixed_bases) = build_recursive_proof_accumulator_from_assets();
    accumulator.resolve_fixed_bases(&recursive_fixed_bases);

    let public_input_encoding = AssignedAccumulator::as_public_input(&accumulator);
    let fixed_base_count = recursive_fixed_bases.len();

    // The last fixed_base_count entries of as_public_input are rhs.fixed_base_scalars
    // (rhs = right-hand side of the pairing equation, in BTreeMap order).
    assert!(
        public_input_encoding
            .iter()
            .rev()
            .take(fixed_base_count)
            .any(|f| !f.is_zero_vartime()),
        "at least one resolved fixed-base scalar should be non-zero"
    );
}

#[test]
fn resolved_fixed_bases_does_not_affect_lhs() {
    // resolve_fixed_bases only touches rhs (right-hand side); the lhs (left-hand side)
    // MSM encoding must be identical before and after the call.
    let (mut accumulator, recursive_fixed_bases) = build_recursive_proof_accumulator_from_assets();

    let lhs_encoding_before_resolution =
        AssignedMsm::<RecursiveEmulation>::as_public_input(&accumulator.lhs());
    accumulator.resolve_fixed_bases(&recursive_fixed_bases);
    let lhs_encoding_after_resolution =
        AssignedMsm::<RecursiveEmulation>::as_public_input(&accumulator.lhs());

    assert_eq!(
        lhs_encoding_before_resolution, lhs_encoding_after_resolution,
        "lhs MSM encoding should be unchanged by resolve_fixed_bases"
    );
}

#[test]
fn resolving_fixed_bases_twice_is_a_harmless_no_op() {
    // Calling resolve_fixed_bases a second time with the same map must leave the
    // accumulator's public-input encoding unchanged, documenting the idempotency guarantee.
    let (mut accumulator, recursive_fixed_bases) = build_recursive_proof_accumulator_from_assets();
    accumulator.resolve_fixed_bases(&recursive_fixed_bases);
    let after_first = AssignedAccumulator::as_public_input(&accumulator);
    accumulator.resolve_fixed_bases(&recursive_fixed_bases);
    let after_second = AssignedAccumulator::as_public_input(&accumulator);
    assert_eq!(after_first, after_second);
}
