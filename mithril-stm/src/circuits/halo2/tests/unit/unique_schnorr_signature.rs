use group::Group;
use midnight_circuits::ecc::curves::CircuitCurve as CircuitCurveTrait;
use midnight_circuits::instructions::{AssignmentInstructions, ConversionInstructions};
use midnight_circuits::types::{AssignedNative, AssignedNativePoint, AssignedScalarOfNativeCurve};

use crate::circuits::halo2::gadgets::{UniqueSchnorrSignatureInputs, verify_unique_signature};
use crate::circuits::halo2::tests::unit::helpers::{
    assert_relation_rejected, impl_unit_relation, jubjub_poseidon_used_chips,
    prove_and_verify_relation, sample_valid_circuit_witness_entry,
};
use crate::circuits::halo2::types::{CircuitBase, CircuitCurve};
use crate::circuits::halo2::witness::{
    CircuitWitnessEntry, MerkleRoot, SignedMessageWithoutPrefix,
};
use crate::signature_scheme::{
    BaseFieldElement, DOMAIN_SEPARATION_TAG_SIGNATURE, PrimeOrderProjectivePoint,
};

impl_unit_relation!(
    UniqueSchnorrSignatureRelation,
    (CircuitWitnessEntry, MerkleRoot, SignedMessageWithoutPrefix),
    jubjub_poseidon_used_chips(),
    |std_lib, layouter, witness| {
        let merkle_tree_commitment: AssignedNative<CircuitBase> = std_lib.assign(
            layouter,
            witness.clone().map(|(_, commitment, _)| commitment.into()),
        )?;
        let message: AssignedNative<CircuitBase> = std_lib.assign(
            layouter,
            witness.clone().map(|(_, _, message)| message.into()),
        )?;
        let hash =
            std_lib.hash_to_curve(layouter, &[merkle_tree_commitment.clone(), message.clone()])?;
        let generator: AssignedNativePoint<CircuitCurve> = std_lib.jubjub().assign_fixed(
            layouter,
            <CircuitCurve as CircuitCurveTrait>::CryptographicGroup::generator(),
        )?;
        let dst_signature =
            std_lib.assign_fixed(layouter, CircuitBase::from(DOMAIN_SEPARATION_TAG_SIGNATURE))?;
        let verification_key = std_lib.jubjub().assign(
            layouter,
            witness
                .clone()
                .map(|(entry, _, _)| entry.leaf.verification_key_point().0),
        )?;
        let response: AssignedScalarOfNativeCurve<CircuitCurve> = std_lib.jubjub().assign(
            layouter,
            witness
                .clone()
                .map(|(entry, _, _)| entry.unique_schnorr_signature.response.0),
        )?;
        let challenge_in_base_field: AssignedNative<CircuitBase> = std_lib.assign(
            layouter,
            witness
                .clone()
                .map(|(entry, _, _)| CircuitBase::from(entry.unique_schnorr_signature.challenge)),
        )?;
        let challenge_as_scalar = std_lib.jubjub().convert(layouter, &challenge_in_base_field)?;
        let commitment_point = std_lib.jubjub().assign(
            layouter,
            witness.map(|(entry, _, _)| {
                let (u, v) = entry.unique_schnorr_signature.commitment_point.get_coordinates();
                PrimeOrderProjectivePoint::from_coordinates(u, v)
                    .expect("unit fixture commitment point should stay prime-order")
                    .0
            }),
        )?;

        verify_unique_signature(
            std_lib,
            layouter,
            UniqueSchnorrSignatureInputs {
                dst_signature: &dst_signature,
                generator: &generator,
                verification_key: &verification_key,
                response: &response,
                challenge_in_base_field: &challenge_in_base_field,
                challenge_as_scalar: &challenge_as_scalar,
                hash: &hash,
                commitment_point: &commitment_point,
            },
        )
    }
);

#[test]
fn unique_schnorr_signature_accepts_valid_witness_entry() {
    let relation = UniqueSchnorrSignatureRelation;
    let (entry, merkle_tree_commitment, message) = sample_valid_circuit_witness_entry()
        .expect("unique_schnorr_signature_accepts_valid_witness_entry should build fixture");

    prove_and_verify_relation(&relation, &(), (entry, merkle_tree_commitment, message))
        .expect("unique_schnorr_signature_accepts_valid_witness_entry should succeed");
}

#[test]
fn unique_schnorr_signature_rejects_wrong_challenge() {
    let relation = UniqueSchnorrSignatureRelation;
    let (mut entry, merkle_tree_commitment, message) = sample_valid_circuit_witness_entry()
        .expect("unique_schnorr_signature_rejects_wrong_challenge should build fixture");
    entry.unique_schnorr_signature.challenge =
        entry.unique_schnorr_signature.challenge + BaseFieldElement::from(1u64);

    assert_relation_rejected(prove_and_verify_relation(
        &relation,
        &(),
        (entry, merkle_tree_commitment, message),
    ));
}
