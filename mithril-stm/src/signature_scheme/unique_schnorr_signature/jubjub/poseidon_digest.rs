use midnight_circuits::{hash::poseidon::PoseidonChip, instructions::hash::HashCPU};
use midnight_curves::Fq as JubjubBase;

use super::BaseFieldElement;

/// Domain Separation Tag (DST) for the Poseidon hash used in signature contexts.
pub(crate) const DOMAIN_SEPARATION_TAG_SIGNATURE: JubjubBase = JubjubBase::from_raw([
    0x5349_474E_5F44_5354, // "SIGN_DST" (ASCII), little-endian u64
    0,
    0,
    0,
]);

#[cfg(feature = "future_snark")]
/// Domain Separation Tag (DST) for the lottery check. It is used as a prefix when computing
/// the eligibility value of a signature.
// TODO: remove this allow dead_code directive when function is called or future_snark is activated
#[allow(dead_code)]
pub const DOMAIN_SEPARATION_TAG_LOTTERY: BaseFieldElement =
    BaseFieldElement(JubjubBase::from_raw([
        0x4C4F_5454_5F44_5354, // "LOTT_DST" (ASCII), little-endian u64
        0,
        0,
        0,
    ]));

/// Computes a Poseidon digest over the provided base field elements
/// Returns a scalar field element as the digest
pub(crate) fn compute_poseidon_digest(input: &[BaseFieldElement]) -> BaseFieldElement {
    let mut poseidon_input = vec![DOMAIN_SEPARATION_TAG_SIGNATURE];
    poseidon_input.extend(input.iter().map(|i| i.0).collect::<Vec<JubjubBase>>());

    BaseFieldElement(PoseidonChip::<JubjubBase>::hash(&poseidon_input))
}

#[cfg(test)]
mod test {

    mod golden {
        use ff::Field;
        use midnight_curves::Fq as JubjubBase;
        use rand_chacha::ChaCha20Rng;
        use rand_core::SeedableRng;

        use crate::signature_scheme::{BaseFieldElement, compute_poseidon_digest};

        const GOLDEN_BYTES: &[u8; 32] = &[
            101, 79, 70, 206, 167, 240, 180, 227, 96, 239, 132, 120, 73, 204, 173, 158, 7, 70, 226,
            165, 250, 3, 119, 186, 80, 22, 132, 135, 21, 128, 227, 98,
        ];

        fn golden_value() -> BaseFieldElement {
            let rng = ChaCha20Rng::from_seed([0u8; 32]);
            let input = JubjubBase::random(rng);
            compute_poseidon_digest(&[BaseFieldElement(input)])
        }

        #[test]
        fn golden_hash() {
            let value = BaseFieldElement::from_bytes(GOLDEN_BYTES)
                .expect("This from bytes should not fail");
            assert_eq!(golden_value(), value);
        }
    }
}
