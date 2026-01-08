use super::BaseFieldElement;
use midnight_circuits::{hash::poseidon::PoseidonChip, instructions::hash::HashCPU};
use midnight_curves::Fq as JubjubBase;

/// Domain Separation Tag (DST) for the Poseidon hash used in signature contexts.
const DST_SIGNATURE: JubjubBase = JubjubBase::from_raw([
    0x5349_474E_5F44_5354, // "SIGN_DST" (ASCII), little-endian u64
    0,
    0,
    0,
]);

/// Computes a Poseidon digest over the provided base field elements
/// Returns a scalar field element as the digest
pub(crate) fn compute_poseidon_digest(input: &[BaseFieldElement]) -> BaseFieldElement {
    let mut poseidon_input = vec![DST_SIGNATURE];
    poseidon_input.extend(input.iter().map(|i| i.0).collect::<Vec<JubjubBase>>());

    BaseFieldElement(PoseidonChip::<JubjubBase>::hash(&poseidon_input))
}
