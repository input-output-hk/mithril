//! Abstractions for working with ark curves
use ark_ec::AffineCurve;
use ark_ff::to_bytes;
use blake2::{Blake2b, Digest};
use num_bigint::BigUint;

/// Implements
/// https://datatracker.ietf.org/doc/draft-irtf-cfrg-hash-to-curve/03/
///
/// Note that while this implementation has side channel vulnerabilities,
/// the hashed value is not secret in our case, so this is not an issue.
pub(crate) fn hash_to_curve<C: AffineCurve>(bytes: &[u8]) -> C {
    // ctr = 0
    let mut ctr = 0_u32;
    loop {
        //  CTR = I20SP(ctr, 4)
        let ctr_string = ctr.to_be_bytes();
        //  ctr = ctr + 1
        ctr += 1;
        //  attempted_hash = Hash(m || CTR)
        let attempted_hash = Blake2b::digest(&[bytes, &ctr_string].concat());

        // h = RS2ECP(attempted_hash)
        if let Some(mut h) = C::from_random_bytes(&attempted_hash[..]) {
            // if H is not "INVALID" ...
            let cofactor = BigUint::from_bytes_be(&to_bytes!(C::COFACTOR).unwrap());
            if cofactor > BigUint::from(1u8) {
                // ... and cofactor > 1
                // set h = h * cofactor
                h = h.mul_by_cofactor();
            }
            if !h.is_zero() {
                // ... if not point at infinity, we're done
                return h;
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ark_bls12_377::Bls12_377;
    use ark_ec::{AffineCurve, PairingEngine};
    use ark_ff::biginteger::BigInteger;
    use ark_ff::{Field, PrimeField};
    use proptest::collection::hash_set;
    use proptest::prelude::*;
    use std::collections::HashSet;
    type C = <Bls12_377 as PairingEngine>::G1Affine;

    fn curve_bytes(pt: &C) -> (Vec<u8>, Vec<u8>) {
        (
            pt.x.into_repr().to_bytes_le(),
            pt.y.into_repr().to_bytes_le(),
        )
    }

    fn diff_bits(x: u8, y: u8) -> u64 {
        (x ^ y).count_ones() as u64
    }

    fn diff_bits_bytes(x: &[u8], y: &[u8]) -> u64 {
        x.iter().zip(y).map(|(bx, by)| diff_bits(*bx, *by)).sum()
    }

    fn diff_bits_curves(x: (&[u8], &[u8]), y: (&[u8], &[u8])) -> u64 {
        diff_bits_bytes(x.0, y.0) + diff_bits_bytes(x.1, y.1)
    }

    proptest! {
        /// Test collision resistance
        #[test]
        fn test_hash_to_curve_distribution(input in hash_set(any::<u64>(), 100)) {
            let hashes = input
                .into_iter()
                .map(|i| hash_to_curve::<C>(&i.to_le_bytes()))
                .collect::<HashSet<_>>()
                .len();
            assert!(hashes == 100);
        }

        /// Test that 'nearby' inputs hash to 'distant-enough' values
        #[test]
        fn test_hash_to_curve_similar(input in any::<u64>()) {
            let n_trials = 100_u64;

            let c0 = hash_to_curve::<C>(&input.to_le_bytes());
            let c0_bytes = curve_bytes(&c0);
            let n_bits =
                2*<<C as AffineCurve>::BaseField as Field>::BasePrimeField::size_in_bits() as u64;
            let mut bit_diffs = 0;

            for d in 1..=n_trials {
                let x = input + d;
                let c_bytes = curve_bytes(&hash_to_curve::<C>(&x.to_le_bytes()));
                assert!(c_bytes.0.len() == c0_bytes.0.len());
                assert!(c_bytes.1.len() == c0_bytes.1.len());
                let diff = diff_bits_curves((&c0_bytes.0, &c0_bytes.1),
                                              (&c_bytes.0, &c_bytes.1));
                bit_diffs += diff;
            }

            // We expect on each trial, about half the bits should change
            let half_bits = n_trials * (n_bits / 2);
            // Test that about half the bits changed between hashes of similar values,
            // +/- 2% for test robustness
            let target_low = 98*half_bits/100;
            let target_high = 102*half_bits/100;
            assert!(target_low <= bit_diffs && bit_diffs <= target_high);
        }
    }
}
