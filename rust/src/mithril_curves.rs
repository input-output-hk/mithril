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
