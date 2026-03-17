//! Assignment helpers that turn circuit witness values into Halo2 layouter values.
//!
//! This module stays intentionally separate from `gadgets`: it prepares assigned inputs for the
//! circuit while the gadget layer owns constraint logic.

use midnight_circuits::instructions::{AssignmentInstructions, ConversionInstructions};
use midnight_circuits::types::{
    AssignedBit, AssignedNative, AssignedNativePoint, AssignedScalarOfNativeCurve,
};
use midnight_proofs::circuit::{Layouter, Value};
use midnight_proofs::plonk::Error;
use midnight_zk_stdlib::ZkStdLib;

use crate::StmResult;
use crate::circuits::halo2::circuit::StmCircuit;
use crate::circuits::halo2::errors::to_synthesis_error;
use crate::circuits::halo2::types::{CircuitBase, CircuitCurve};
use crate::circuits::halo2::witness::CircuitWitnessEntry;
use crate::signature_scheme::PrimeOrderProjectivePoint;

pub(crate) struct AssignedMerklePath {
    pub(crate) siblings: Vec<AssignedNative<CircuitBase>>,
    pub(crate) positions: Vec<AssignedBit<CircuitBase>>,
}

pub(crate) struct AssignedWitnessEntry {
    pub(crate) verification_key: AssignedNativePoint<CircuitCurve>,
    pub(crate) lottery_target_value: AssignedNative<CircuitBase>,
    pub(crate) merkle_path: AssignedMerklePath,
    pub(crate) lottery_index: AssignedNative<CircuitBase>,
}

pub(crate) struct AssignedSignatureComponents {
    pub(crate) commitment_point: AssignedNativePoint<CircuitCurve>,
    pub(crate) response: AssignedScalarOfNativeCurve<CircuitCurve>,
    pub(crate) challenge_in_base_field: AssignedNative<CircuitBase>,
    pub(crate) challenge_as_scalar: AssignedScalarOfNativeCurve<CircuitCurve>,
}

impl StmCircuit {
    pub(crate) fn assign_witness_entry(
        &self,
        std_lib: &ZkStdLib,
        layouter: &mut impl Layouter<CircuitBase>,
        witness_entry: Value<CircuitWitnessEntry>,
    ) -> Result<AssignedWitnessEntry, Error> {
        let verification_key =
            Self::assign_verification_key(std_lib, layouter, witness_entry.clone())?;

        let lottery_target_value = std_lib.assign(
            layouter,
            witness_entry
                .clone()
                .map(|entry| entry.leaf.lottery_target_value().into()),
        )?;
        let merkle_path = self.assign_merkle_path(std_lib, layouter, witness_entry.clone())?;

        let lottery_index = std_lib.assign(
            layouter,
            witness_entry.map(|entry| CircuitBase::from(entry.lottery_index)),
        )?;

        Ok(AssignedWitnessEntry {
            verification_key,
            lottery_target_value,
            merkle_path,
            lottery_index,
        })
    }

    fn assign_verification_key(
        std_lib: &ZkStdLib,
        layouter: &mut impl Layouter<CircuitBase>,
        witness_entry: Value<CircuitWitnessEntry>,
    ) -> Result<AssignedNativePoint<CircuitCurve>, Error> {
        std_lib.jubjub().assign(
            layouter,
            witness_entry.map(|entry| entry.leaf.verification_key_point().0),
        )
    }

    fn assign_merkle_path(
        &self,
        std_lib: &ZkStdLib,
        layouter: &mut impl Layouter<CircuitBase>,
        witness_entry: Value<CircuitWitnessEntry>,
    ) -> Result<AssignedMerklePath, Error> {
        let siblings = std_lib.assign_many(
            layouter,
            witness_entry
                .clone()
                .map_with_result(|entry| -> StmResult<_> {
                    let merkle_path = entry.merkle_path;
                    self.validate_merkle_sibling_length(merkle_path.siblings.len())?;
                    Ok(merkle_path
                        .siblings
                        .iter()
                        .map(|sibling| sibling.1.into())
                        .collect::<Vec<_>>())
                })
                .map_err(to_synthesis_error)?
                .transpose_vec(self.merkle_tree_depth() as usize)
                .as_slice(),
        )?;

        let positions = std_lib.assign_many(
            layouter,
            witness_entry
                .clone()
                .map_with_result(|entry| -> StmResult<_> {
                    let merkle_path = entry.merkle_path;
                    self.validate_merkle_position_length(merkle_path.siblings.len())?;
                    Ok(merkle_path
                        .siblings
                        .iter()
                        .map(|sibling| CircuitBase::from(sibling.0))
                        .collect::<Vec<_>>())
                })
                .map_err(to_synthesis_error)?
                .transpose_vec(self.merkle_tree_depth() as usize)
                .as_slice(),
        )?;

        let positions = positions
            .iter()
            .map(|position| std_lib.convert(layouter, position))
            .collect::<Result<Vec<AssignedBit<CircuitBase>>, Error>>()?;

        Ok(AssignedMerklePath {
            siblings,
            positions,
        })
    }

    pub(crate) fn assign_signature_components(
        std_lib: &ZkStdLib,
        layouter: &mut impl Layouter<CircuitBase>,
        witness_entry: Value<CircuitWitnessEntry>,
    ) -> Result<AssignedSignatureComponents, Error> {
        let commitment_point_value = witness_entry
            .clone()
            .map_with_result(|entry| {
                let signature = entry.unique_schnorr_signature;
                let (u, v) = signature.commitment_point.get_coordinates();
                PrimeOrderProjectivePoint::from_coordinates(u, v).map(|point| point.0)
            })
            .map_err(to_synthesis_error)?;
        let commitment_point = std_lib.jubjub().assign(layouter, commitment_point_value)?;
        let response = std_lib.jubjub().assign(
            layouter,
            witness_entry
                .clone()
                .map(|entry| entry.unique_schnorr_signature.response.0),
        )?;
        let challenge_in_base_field = std_lib.assign(
            layouter,
            witness_entry.map(|entry| CircuitBase::from(entry.unique_schnorr_signature.challenge)),
        )?;
        let challenge_as_scalar = std_lib.jubjub().convert(layouter, &challenge_in_base_field)?;

        Ok(AssignedSignatureComponents {
            commitment_point,
            response,
            challenge_in_base_field,
            challenge_as_scalar,
        })
    }
}
