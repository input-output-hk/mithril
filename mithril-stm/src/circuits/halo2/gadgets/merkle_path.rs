use anyhow::anyhow;
use midnight_circuits::instructions::{
    AssertionInstructions, ControlFlowInstructions, EccInstructions, ZeroInstructions,
};
use midnight_circuits::types::{AssignedBit, AssignedNative, AssignedNativePoint};
use midnight_proofs::circuit::Layouter;
use midnight_proofs::plonk::Error;
use midnight_zk_stdlib::ZkStdLib;

use crate::circuits::halo2::errors::to_synthesis_error;
use crate::circuits::halo2::types::{CircuitBase, CircuitCurve};

/// Assigned inputs required to verify one Merkle authentication path inside the circuit.
pub(crate) struct MerklePathInputs<'a> {
    /// Assigned verification key stored in the Merkle leaf.
    pub(crate) verification_key: &'a AssignedNativePoint<CircuitCurve>,
    /// Assigned lottery target value stored in the Merkle leaf.
    pub(crate) lottery_target_value: &'a AssignedNative<CircuitBase>,
    /// Assigned public Merkle root committed by the statement.
    pub(crate) merkle_tree_commitment: &'a AssignedNative<CircuitBase>,
    /// Assigned sibling hashes for each Merkle path level.
    pub(crate) merkle_siblings: &'a [AssignedNative<CircuitBase>],
    /// Assigned sibling positions for each Merkle path level.
    pub(crate) merkle_positions: &'a [AssignedBit<CircuitBase>],
}

/// Verifies that the assigned Merkle path opens the witness leaf to the public commitment.
pub(crate) fn verify_merkle_path(
    std_lib: &ZkStdLib,
    layouter: &mut impl Layouter<CircuitBase>,
    inputs: MerklePathInputs<'_>,
) -> Result<(), Error> {
    let verification_key_x = std_lib.jubjub().x_coordinate(inputs.verification_key);
    let verification_key_y = std_lib.jubjub().y_coordinate(inputs.verification_key);
    let leaf = std_lib.poseidon(
        layouter,
        &[
            verification_key_x.clone(),
            verification_key_y.clone(),
            inputs.lottery_target_value.clone(),
        ],
    )?;
    // Compute the first node after the leaves outside the loop
    // to not ignore a potential padding zero leaf
    // other zero leaf are ignored
    let first_sibling = inputs
        .merkle_siblings
        .first()
        .ok_or(anyhow!("Missing siblings"))
        .map_err(to_synthesis_error)?;
    let first_position = inputs
        .merkle_positions
        .first()
        .ok_or(anyhow!("Missing position!"))
        .map_err(to_synthesis_error)?;
    let first_left = std_lib.select(layouter, first_position, &leaf, first_sibling)?;
    let first_right = std_lib.select(layouter, first_position, first_sibling, &leaf)?;
    let first_node = std_lib.poseidon(layouter, &[first_left, first_right])?;

    let root = inputs
        .merkle_siblings
        .iter()
        .skip(1)
        .zip(inputs.merkle_positions.iter().skip(1))
        .try_fold(first_node, |acc, (x, pos)| {
            let left = std_lib.select(layouter, pos, &acc, x)?;
            let right = std_lib.select(layouter, pos, x, &acc)?;
            let current_node = std_lib.poseidon(layouter, &[left, right])?;

            let is_zero = std_lib.is_zero(layouter, x)?;
            std_lib.select(layouter, &is_zero, &acc, &current_node)
        })?;

    std_lib.assert_equal(layouter, &root, inputs.merkle_tree_commitment)
}

#[cfg(test)]
mod tests {
    use midnight_circuits::instructions::{AssignmentInstructions, ConversionInstructions};
    use midnight_proofs::plonk::Error;

    use crate::circuits::halo2::tests::test_helpers::{
        TEST_MERKLE_TREE_DEPTH, assert_relation_rejected, impl_focused_test_relation,
        jubjub_poseidon_used_chips, prove_and_verify_relation, sample_valid_circuit_witness_entry,
    };
    use crate::circuits::halo2::types::CircuitBase;
    use crate::circuits::halo2::witness::{CircuitWitnessEntry, MerkleRoot};

    use super::{MerklePathInputs, verify_merkle_path};

    impl_focused_test_relation!(
        MerkleRelation,
        (CircuitWitnessEntry, MerkleRoot),
        jubjub_poseidon_used_chips(),
        |std_lib, layouter, witness| {
            let verification_key = std_lib.jubjub().assign(
                layouter,
                witness
                    .clone()
                    .map(|(entry, _)| entry.leaf.verification_key_point().0),
            )?;
            let lottery_target_value = std_lib.assign(
                layouter,
                witness
                    .clone()
                    .map(|(entry, _)| entry.leaf.lottery_target_value().into()),
            )?;
            let merkle_tree_commitment = std_lib.assign(
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
            let merkle_positions = merkle_positions
                .iter()
                .map(|position| std_lib.convert(layouter, position))
                .collect::<Result<Vec<_>, Error>>()?;

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
}
