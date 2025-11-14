// TODO: Remove
#![allow(dead_code)]

mod signature;
mod signing_key;
mod utils;
mod verification_key;

pub use signature::*;
pub use signing_key::*;
pub use utils::*;
pub use verification_key::*;

use dusk_jubjub::Fq as JubjubBase;

/// A DST (Domain Separation Tag) to distinguish between use of Poseidon hash
const DST_SIGNATURE: JubjubBase = JubjubBase::from_raw([0u64, 0, 0, 0]);

#[cfg(test)]
mod tests {

    use super::*;
    use dusk_jubjub::SubgroupPoint as JubjubSubgroup;
    use ff::Field;
    use rand_chacha::ChaCha20Rng;
    use rand_core::SeedableRng;

    use crate::schnorr_signature::{SchnorrSigningKey, SchnorrVerificationKey};

    #[test]
    fn sign_and_verify() {
        let msg = vec![0, 0, 0, 1];
        let seed = [0u8; 32];
        let mut rng = ChaCha20Rng::from_seed(seed);
        let sk = SchnorrSigningKey::generate(&mut rng).unwrap();
        let vk = SchnorrVerificationKey::from(&sk);

        let sig = sk.sign(&msg, &mut rng).unwrap();

        sig.verify(&msg, &vk).unwrap();
    }

    #[test]
    fn invalid_sig() {
        let msg = vec![0, 0, 0, 1];
        let msg2 = vec![0, 0, 0, 2];
        let seed = [0u8; 32];
        let mut rng = ChaCha20Rng::from_seed(seed);
        let sk = SchnorrSigningKey::generate(&mut rng).unwrap();
        let vk = SchnorrVerificationKey::from(&sk);
        let sk2 = SchnorrSigningKey::generate(&mut rng).unwrap();
        let vk2 = SchnorrVerificationKey::from(&sk2);

        let sig = sk.sign(&msg, &mut rng).unwrap();
        let sig2 = sk.sign(&msg2, &mut rng).unwrap();

        // Wrong verification key is used
        let result1 = sig.verify(&msg, &vk2);
        let result2 = sig2.verify(&msg, &vk);

        assert!(
            result1.is_err(),
            "Wrong verfication key used, test should fail."
        );
        // Wrong message is verified
        assert!(result2.is_err(), "Wrong message used, test should fail.");
    }

    #[test]
    fn verify_fail_verification_key_not_on_curve() {
        let msg = vec![0, 0, 0, 1];
        let seed = [0u8; 32];
        let mut rng = ChaCha20Rng::from_seed(seed);
        let sk = SchnorrSigningKey::generate(&mut rng).unwrap();
        let vk1 = SchnorrVerificationKey::from(&sk);
        let sig = sk.sign(&msg, &mut rng).unwrap();
        let vk2 = SchnorrVerificationKey(JubjubSubgroup::from_raw_unchecked(
            JubjubBase::ONE,
            JubjubBase::ONE,
        ));

        let result1 = sig.verify(&msg, &vk1);
        let result2 = sig.verify(&msg, &vk2);

        assert!(
            result1.is_ok(),
            "Correct verification key used, test should pass."
        );
        assert!(
            result2.is_err(),
            "Invalid verification key used, test should fail."
        );
    }
}
