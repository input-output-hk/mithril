use midnight_circuits::instructions::{AssignmentInstructions, ConversionInstructions};
use midnight_circuits::types::{AssignedBit, AssignedNative, AssignedNativePoint};
use midnight_proofs::plonk::Error;

use crate::circuits::halo2::gadgets::{MerklePathInputs, verify_merkle_path};
use crate::circuits::halo2::tests::unit::helpers::{
    TEST_MERKLE_TREE_DEPTH, assert_relation_rejected, impl_unit_relation,
    jubjub_poseidon_used_chips, prove_and_verify_relation, sample_valid_circuit_witness_entry,
};
use crate::circuits::halo2::types::{CircuitBase, CircuitCurve};
use crate::circuits::halo2::witness::{CircuitWitnessEntry, MerkleRoot};

impl_unit_relation!(
    MerkleRelation,
    (CircuitWitnessEntry, MerkleRoot),
    jubjub_poseidon_used_chips(),
    |std_lib, layouter, witness| {
        let verification_key: AssignedNativePoint<CircuitCurve> = std_lib.jubjub().assign(
            layouter,
            witness
                .clone()
                .map(|(entry, _)| entry.leaf.verification_key_point().0),
        )?;
        let lottery_target_value: AssignedNative<CircuitBase> = std_lib.assign(
            layouter,
            witness
                .clone()
                .map(|(entry, _)| entry.leaf.lottery_target_value().into()),
        )?;
        let merkle_tree_commitment: AssignedNative<CircuitBase> = std_lib.assign(
            layouter,
            witness.clone().map(|(_, commitment)| commitment.into()),
        )?;
        let merkle_siblings = std_lib.assign_many(
            layouter,
            witness
                .clone()
                .map(|(entry, _)| {
                    entry
                        .merkle_path
                        .siblings
                        .iter()
                        .map(|(_, sibling)| (*sibling).into())
                        .collect::<Vec<_>>()
                })
                .transpose_vec(TEST_MERKLE_TREE_DEPTH)
                .as_slice(),
        )?;
        let merkle_positions = std_lib.assign_many(
            layouter,
            witness
                .map(|(entry, _)| {
                    entry
                        .merkle_path
                        .siblings
                        .iter()
                        .map(|(position, _)| CircuitBase::from(*position))
                        .collect::<Vec<_>>()
                })
                .transpose_vec(TEST_MERKLE_TREE_DEPTH)
                .as_slice(),
        )?;
        let merkle_positions: Vec<AssignedBit<CircuitBase>> = merkle_positions
            .iter()
            .map(|position| std_lib.convert(layouter, position))
            .collect::<Result<_, Error>>()?;

        verify_merkle_path(
            std_lib,
            layouter,
            MerklePathInputs {
                verification_key: &verification_key,
                lottery_target_value: &lottery_target_value,
                merkle_tree_commitment: &merkle_tree_commitment,
                merkle_siblings: &merkle_siblings,
                merkle_positions: &merkle_positions,
            },
        )
    }
);

#[test]
fn merkle_path_accepts_valid_witness_entry() {
    let relation = MerkleRelation;
    let (entry, merkle_tree_commitment, _) = sample_valid_circuit_witness_entry()
        .expect("merkle_path_accepts_valid_witness_entry should build fixture");

    prove_and_verify_relation(&relation, &(), (entry, merkle_tree_commitment))
        .expect("merkle_path_accepts_valid_witness_entry should succeed");
}

#[test]
fn merkle_path_rejects_wrong_merkle_tree_commitment() {
    let relation = MerkleRelation;
    let (entry, _, _) = sample_valid_circuit_witness_entry()
        .expect("merkle_path_rejects_wrong_merkle_tree_commitment should build fixture");

    assert_relation_rejected(prove_and_verify_relation(
        &relation,
        &(),
        (entry, MerkleRoot::from(999u64)),
    ));
}
