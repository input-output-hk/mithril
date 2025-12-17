use dusk_jubjub::Fq as JubjubBase;
use dusk_poseidon::{Domain, Hash};

use super::{BaseFieldElement, ScalarFieldElement};

/// A DST (Domain Separation Tag) to distinguish between use of Poseidon hash
const DST_SIGNATURE: JubjubBase = JubjubBase::from_raw([0u64, 0, 0, 0]);

/// Computes a truncated Poseidon digest over the provided base field elements
/// Returns a scalar field element as the digest
pub(crate) fn compute_truncated_digest(input: &[BaseFieldElement]) -> ScalarFieldElement {
    let mut poseidon_input = vec![DST_SIGNATURE];
    poseidon_input.extend(input.iter().map(|i| i.0).collect::<Vec<JubjubBase>>());

    ScalarFieldElement(Hash::digest_truncated(Domain::Other, &poseidon_input)[0])
}
