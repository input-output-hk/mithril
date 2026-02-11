//! Adapters between STM signatures and Halo2 off-circuit signature types.

use group::GroupEncoding;

use crate::circuits::halo2::off_circuit::error::SignatureError;
use crate::circuits::halo2::off_circuit::unique_signature::Signature;
use crate::circuits::halo2::types::{JubjubBase, JubjubScalar, JubjubSubgroup};
use crate::signature_scheme::UniqueSchnorrSignature;

/// Convert an STM unique Schnorr signature into the Halo2 off-circuit signature shape.
#[allow(dead_code)]
pub(crate) fn stm_signature_to_off_circuit(
    signature: &UniqueSchnorrSignature,
) -> Result<Signature, SignatureError> {
    // Layout matches UniqueSchnorrSignature::to_bytes(): commitment(32) | response(32) | challenge(32).
    // BaseFieldElement::to_bytes is little-endian, so challenge is decoded with from_bytes_le.
    let bytes = signature.to_bytes();
    let commitment_bytes: [u8; 32] = bytes[0..32]
        .try_into()
        .map_err(|_| SignatureError::SerializationError)?;
    let response_bytes: [u8; 32] = bytes[32..64]
        .try_into()
        .map_err(|_| SignatureError::SerializationError)?;
    let challenge_bytes: [u8; 32] = bytes[64..96]
        .try_into()
        .map_err(|_| SignatureError::SerializationError)?;

    let sigma = JubjubSubgroup::from_bytes(&commitment_bytes)
        .into_option()
        .ok_or(SignatureError::SerializationError)?;
    let s = JubjubScalar::from_bytes(&response_bytes)
        .into_option()
        .ok_or(SignatureError::SerializationError)?;
    let c = JubjubBase::from_bytes_le(&challenge_bytes)
        .into_option()
        .ok_or(SignatureError::SerializationError)?;

    Ok(Signature { sigma, s, c })
}
