use digest::generic_array::typenum::U32;
use digest::{FixedOutput, HashMarker, Output, OutputSizeUser, Reset, Update};
// use midnight_circuits::hash::poseidon::{NativeSpec, PoseidonNative};
use midnight_circuits::{hash::poseidon::PoseidonChip, instructions::hash::HashCPU};
use midnight_curves::Fq as JubjubBase;

use super::BaseFieldElement;

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

#[derive(Debug, Clone, Default)]
pub struct MidnightPoseidonDigest {
    buffer: Vec<u8>,
}

impl MidnightPoseidonDigest {
    pub fn new() -> Self {
        Self { buffer: Vec::new() }
    }
}

impl Update for MidnightPoseidonDigest {
    fn update(&mut self, data: &[u8]) {
        // Collect bytes. In a production version, you'd chunk these
        // into 31-byte or 32-byte field elements immediately.
        self.buffer.extend_from_slice(data);
    }
}

impl OutputSizeUser for MidnightPoseidonDigest {
    type OutputSize = U32;
}

impl FixedOutput for MidnightPoseidonDigest {
    fn finalize_into(self, out: &mut Output<Self>) {
        // 1. Convert buffered bytes to Scalar elements
        // This is where you follow Midnight's specific padding/chunking
        let poseidon_input = self
            .buffer
            .chunks_exact(32)
            .map(|c| {
                JubjubBase::from_raw([
                    u64::from_le_bytes(c[0..8].try_into().unwrap()),
                    u64::from_le_bytes(c[8..16].try_into().unwrap()),
                    u64::from_le_bytes(c[16..24].try_into().unwrap()),
                    u64::from_le_bytes(c[24..32].try_into().unwrap()),
                ])
            })
            .collect::<Vec<JubjubBase>>();
        // let poseidon_chip = PoseidonChip::from(self.chip_config);
        let result: JubjubBase = PoseidonChip::<JubjubBase>::hash(&poseidon_input);

        // 4. Output as bytes
        out.copy_from_slice(&result.to_bytes_le());
    }
}

impl Reset for MidnightPoseidonDigest {
    fn reset(&mut self) {
        self.buffer.clear();
        // If your sponge has internal state, reset that here too.
    }
}

impl HashMarker for MidnightPoseidonDigest {}

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
