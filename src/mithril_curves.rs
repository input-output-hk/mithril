//! Abstractions for working with ark curves
use crate::hashutils::hash_message;

use ark_ec::AffineCurve;
use ark_ff::{Field, FpParameters, PrimeField};

pub fn hash_to_curve<C: AffineCurve>(bytes: &[u8]) -> C {
    let needed =
        <<<C::BaseField as Field>::BasePrimeField as PrimeField>::Params as FpParameters>::MODULUS_BITS / 8;
    let x: &[u8] = &hash_message(bytes, needed as usize);
    let mut q = num_bigint::BigUint::from_bytes_le(x);
    loop {
        if let Some(elt) = C::from_random_bytes(&q.to_bytes_le()) {
            return elt;
        } else {
            q += 1u8;
        }
    }
}
