use crate::LotteryIndex;
use crate::circuits::halo2::gadgets::{
    assert_lottery_index_in_bounds, assert_lottery_won, assert_strictly_increasing_lottery_index,
};
use crate::circuits::halo2::tests::unit::helpers::{
    assert_relation_rejected, comparison_used_chips, impl_unit_relation,
    jubjub_poseidon_used_chips, prove_and_verify_relation,
};
use crate::circuits::halo2::types::{CircuitBase, CircuitCurve};
use crate::circuits::halo2::witness::LotteryTargetValue as CircuitLotteryTargetValue;
use crate::signature_scheme::PrimeOrderProjectivePoint;
use midnight_circuits::instructions::AssignmentInstructions;
use midnight_circuits::types::{AssignedNative, AssignedNativePoint};

type LotteryWonWitness = (
    CircuitLotteryTargetValue,
    PrimeOrderProjectivePoint,
    LotteryIndex,
    CircuitLotteryTargetValue,
);

impl_unit_relation!(
    LotteryWonRelation,
    LotteryWonWitness,
    jubjub_poseidon_used_chips(),
    |std_lib, layouter, witness| {
        let lottery_prefix: AssignedNative<CircuitBase> = std_lib.assign(
            layouter,
            witness.clone().map(|(prefix, _, _, _)| prefix.into()),
        )?;
        let commitment_point: AssignedNativePoint<CircuitCurve> = std_lib
            .jubjub()
            .assign(layouter, witness.clone().map(|(_, point, _, _)| point.0))?;
        let lottery_index: AssignedNative<CircuitBase> = std_lib.assign(
            layouter,
            witness.clone().map(|(_, _, index, _)| CircuitBase::from(index)),
        )?;
        let lottery_target_value: AssignedNative<CircuitBase> =
            std_lib.assign(layouter, witness.map(|(_, _, _, target)| target.into()))?;

        assert_lottery_won(
            std_lib,
            layouter,
            &lottery_prefix,
            &commitment_point,
            &lottery_index,
            &lottery_target_value,
        )
    }
);

impl_unit_relation!(
    IncreasingIndexRelation,
    (LotteryIndex, LotteryIndex),
    comparison_used_chips(),
    |std_lib, layouter, witness| {
        let previous_lottery_index: AssignedNative<CircuitBase> = std_lib.assign(
            layouter,
            witness.clone().map(|(previous, _)| CircuitBase::from(previous)),
        )?;
        let lottery_index: AssignedNative<CircuitBase> = std_lib.assign(
            layouter,
            witness.map(|(_, current)| CircuitBase::from(current)),
        )?;

        assert_strictly_increasing_lottery_index(
            std_lib,
            layouter,
            &previous_lottery_index,
            &lottery_index,
        )
    }
);

impl_unit_relation!(
    InBoundsIndexRelation,
    (LotteryIndex, u32),
    comparison_used_chips(),
    |std_lib, layouter, witness| {
        let lottery_index: AssignedNative<CircuitBase> = std_lib.assign(
            layouter,
            witness.clone().map(|(index, _)| CircuitBase::from(index)),
        )?;
        let m: AssignedNative<CircuitBase> = std_lib.assign(
            layouter,
            witness.map(|(_, m)| CircuitBase::from(u64::from(m))),
        )?;

        assert_lottery_index_in_bounds(std_lib, layouter, &lottery_index, &m)
    }
);

#[test]
fn lottery_won_accepts_max_target_value() {
    let relation = LotteryWonRelation;
    let witness = (
        CircuitLotteryTargetValue::from(7u64),
        PrimeOrderProjectivePoint::create_generator(),
        3u64,
        -CircuitLotteryTargetValue::ONE,
    );

    prove_and_verify_relation(&relation, &(), witness)
        .expect("lottery_won_accepts_max_target_value should succeed");
}

#[test]
fn lottery_won_rejects_small_target_value() {
    let relation = LotteryWonRelation;
    let witness = (
        CircuitLotteryTargetValue::from(7u64),
        PrimeOrderProjectivePoint::create_generator(),
        3u64,
        CircuitLotteryTargetValue::ZERO,
    );

    assert_relation_rejected(prove_and_verify_relation(&relation, &(), witness));
}

#[test]
fn index_constraints_accept_strict_order_below_m() {
    // Keep one positive path for each extracted index helper without duplicating golden coverage.
    let relation = InBoundsIndexRelation;
    prove_and_verify_relation(&relation, &(), (2u64, 3u32))
        .expect("index_constraints_accept_strict_order_below_m should succeed");

    let relation = IncreasingIndexRelation;
    prove_and_verify_relation(&relation, &(), (1u64, 2u64))
        .expect("index_constraints_accept_strict_order_below_m should succeed");
}

#[test]
fn index_constraints_reject_index_at_upper_bound() {
    let relation = InBoundsIndexRelation;
    assert_relation_rejected(prove_and_verify_relation(&relation, &(), (3u64, 3u32)));
}
