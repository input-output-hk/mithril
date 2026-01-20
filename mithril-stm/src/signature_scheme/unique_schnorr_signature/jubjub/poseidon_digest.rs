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

/// Wrapper to implement the Digest trait for the Poseidon hash function
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
        // Computes the next multiple of 32 as target length
        let target_len = (data.len() + 31) & !31;
        let mut padded_data = Vec::with_capacity(target_len);
        padded_data.extend_from_slice(data);
        // Pad the data with zeros on the right to match the target length
        padded_data.resize(target_len, 0);
        self.buffer.extend_from_slice(&padded_data);
    }
}

impl OutputSizeUser for MidnightPoseidonDigest {
    type OutputSize = U32;
}

impl FixedOutput for MidnightPoseidonDigest {
    fn finalize_into(self, out: &mut Output<Self>) {
        // The data is padded during the call to the update function
        // so there should always be a multiple of 32 bytes in the buffer
        // We are taking chuncks of 32 u8 so it should be fine to unwrap
        let poseidon_input = self
            .buffer
            .chunks_exact(32)
            .map(|chunk| {
                JubjubBase::from_raw([
                    u64::from_le_bytes(chunk[0..8].try_into().unwrap()),
                    u64::from_le_bytes(chunk[8..16].try_into().unwrap()),
                    u64::from_le_bytes(chunk[16..24].try_into().unwrap()),
                    u64::from_le_bytes(chunk[24..32].try_into().unwrap()),
                ])
            })
            .collect::<Vec<JubjubBase>>();
        let result: JubjubBase = PoseidonChip::<JubjubBase>::hash(&poseidon_input);
        out.copy_from_slice(&result.to_bytes_le());
    }
}

impl Reset for MidnightPoseidonDigest {
    fn reset(&mut self) {
        self.buffer.clear();
    }
}

impl HashMarker for MidnightPoseidonDigest {}

#[cfg(test)]
mod test {
    use crate::signature_scheme::MidnightPoseidonDigest;
    use midnight_circuits::{hash::poseidon::PoseidonChip, instructions::hash::HashCPU};
    use midnight_curves::Fq;
    use midnight_curves::Fq as JubjubBase;
    use sha2::Digest;

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

    #[test]
    fn test_digest_impl_single_element() {
        let bytes = [0u8; 32];
        let elem = JubjubBase::from_raw([
            u64::from_le_bytes(bytes[0..8].try_into().unwrap()),
            u64::from_le_bytes(bytes[8..16].try_into().unwrap()),
            u64::from_le_bytes(bytes[16..24].try_into().unwrap()),
            u64::from_le_bytes(bytes[24..32].try_into().unwrap()),
        ]);

        let digest_result = MidnightPoseidonDigest::digest(bytes).to_vec();
        let mut digest_result_bytes = [0u8; 32];
        digest_result_bytes.copy_from_slice(&digest_result);
        let digest_result_elem = JubjubBase::from_bytes_le(&digest_result_bytes).unwrap();
        let digest_result_poseidon = PoseidonChip::<Fq>::hash(&[elem]);

        assert_eq!(digest_result_elem, digest_result_poseidon);
    }

    #[test]
    fn test_digest_impl_chain_update() {
        let bytes = [0u8; 32];
        let elem = JubjubBase::from_raw([
            u64::from_le_bytes(bytes[0..8].try_into().unwrap()),
            u64::from_le_bytes(bytes[8..16].try_into().unwrap()),
            u64::from_le_bytes(bytes[16..24].try_into().unwrap()),
            u64::from_le_bytes(bytes[24..32].try_into().unwrap()),
        ]);

        let digest_result = MidnightPoseidonDigest::new()
            .chain_update(bytes)
            .chain_update(bytes)
            .finalize()
            .to_vec();
        let mut digest_result_bytes = [0u8; 32];
        digest_result_bytes.copy_from_slice(&digest_result);
        let digest_result_elem = JubjubBase::from_bytes_le(&digest_result_bytes).unwrap();
        let digest_result_poseidon = PoseidonChip::<Fq>::hash(&[elem, elem]);

        assert_eq!(digest_result_elem, digest_result_poseidon);
    }

    #[test]
    fn digest_impl_single_byte() {
        let byte = 2u8;
        let elem = JubjubBase::from(byte as u64);

        let digest_result = MidnightPoseidonDigest::digest([byte]).to_vec();
        let mut digest_result_bytes = [0u8; 32];
        digest_result_bytes.copy_from_slice(&digest_result);
        let digest_result_elem = JubjubBase::from_bytes_le(&digest_result_bytes).unwrap();
        let poseidon_result = PoseidonChip::<Fq>::hash(&[elem]);

        assert_eq!(digest_result_elem, poseidon_result);
        println!("{:?}", poseidon_result);
        println!("{:?}", digest_result_elem);
    }

    #[test]
    fn digest_impl_input_not_multiple_32() {
        let bytes = [1u8; 48];
        let zero_bytes = [0u8; 16];
        let elem1 = JubjubBase::from_raw([
            u64::from_le_bytes(bytes[0..8].try_into().unwrap()),
            u64::from_le_bytes(bytes[8..16].try_into().unwrap()),
            u64::from_le_bytes(bytes[16..24].try_into().unwrap()),
            u64::from_le_bytes(bytes[24..32].try_into().unwrap()),
        ]);
        let elem2 = JubjubBase::from_raw([
            u64::from_le_bytes(bytes[32..40].try_into().unwrap()),
            u64::from_le_bytes(bytes[40..48].try_into().unwrap()),
            u64::from_le_bytes(zero_bytes[0..8].try_into().unwrap()),
            u64::from_le_bytes(zero_bytes[8..16].try_into().unwrap()),
        ]);

        let digest_result = MidnightPoseidonDigest::digest(bytes).to_vec();
        let mut digest_result_bytes = [0u8; 32];
        digest_result_bytes.copy_from_slice(&digest_result);
        let digest_result_elem = JubjubBase::from_bytes_le(&digest_result_bytes).unwrap();
        let poseidon_result = PoseidonChip::<Fq>::hash(&[elem1, elem2]);

        assert_eq!(digest_result_elem, poseidon_result);
    }

    #[test]
    fn digest_impl_chain_update_order() {
        let one = JubjubBase::from(1u64);
        let two = JubjubBase::from(2u64);
        let three = JubjubBase::from(3u64);

        let digest_result = MidnightPoseidonDigest::new()
            .chain_update([1u8])
            .chain_update([3u8])
            .chain_update([2u8])
            .finalize()
            .to_vec();
        let mut digest_result_bytes = [0u8; 32];
        digest_result_bytes.copy_from_slice(&digest_result);
        let digest_result_elem = JubjubBase::from_bytes_le(&digest_result_bytes).unwrap();
        let poseidon_result = PoseidonChip::<Fq>::hash(&[one, three, two]);

        assert_eq!(digest_result_elem, poseidon_result);
    }
}
