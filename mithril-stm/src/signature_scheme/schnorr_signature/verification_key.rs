use anyhow::{Context, anyhow};
use dusk_jubjub::SubgroupPoint as JubjubSubgroup;
use group::{Group, GroupEncoding};

use crate::StmResult;

use super::{SchnorrSignatureError, SchnorrSigningKey};

/// Schnorr verification key, it consists of a point on the Jubjub curve
/// vk = g * sk, where g is a generator
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct SchnorrVerificationKey(pub(crate) JubjubSubgroup);

impl SchnorrVerificationKey {
    /// Convert a `SchnorrVerificationKey` into bytes.
    pub fn to_bytes(self) -> [u8; 32] {
        self.0.to_bytes()
    }

    /// Convert bytes into a `SchnorrVerificationKey`.
    ///
    /// The bytes must represent a Jubjub Subgroup point or the conversion will fail
    pub fn from_bytes(bytes: &[u8]) -> StmResult<Self> {
        if bytes.len() < 32 {
            return Err(anyhow!(SchnorrSignatureError::SerializationError)).with_context(
                || "Not enough bytes provided to create a Schnorr verification key.",
            );
        }
        let verification_key_bytes = bytes[0..32]
            .try_into()
            .map_err(|_| anyhow!(SchnorrSignatureError::SerializationError))
            .with_context(|| "Failed to obtain the Schnorr verification key's bytes.")?;
        let point = JubjubSubgroup::from_bytes(&verification_key_bytes)
            .into_option()
            .ok_or(anyhow!(SchnorrSignatureError::SerializationError))
            .with_context(|| "Failed to create a JubjubSubgroup point from the given bytes.")?;

        Ok(SchnorrVerificationKey(point))
    }
}

impl From<&SchnorrSigningKey> for SchnorrVerificationKey {
    /// Convert a Schnorr secret key into a verification key
    ///
    /// This is done by computing `vk = g * sk` where g is the generator
    /// of the subgroup and sk is the schnorr secret key
    fn from(signing_key: &SchnorrSigningKey) -> Self {
        let generator = JubjubSubgroup::generator();

        SchnorrVerificationKey(generator * signing_key.0)
    }
}

#[cfg(test)]
mod tests {
    use dusk_jubjub::Fq as JubjubBase;
    use dusk_jubjub::SubgroupPoint as JubjubSubgroup;
    use ff::Field;
    use group::Group;
    use rand_chacha::ChaCha20Rng;
    use rand_core::SeedableRng;

    use crate::signature_scheme::{SchnorrSigningKey, SchnorrVerificationKey};

    #[test]
    fn generate_verification_key() {
        let mut rng = ChaCha20Rng::from_seed([0u8; 32]);
        let sk = SchnorrSigningKey::try_generate(&mut rng).unwrap();
        let g = JubjubSubgroup::generator();
        let vk = g * sk.0;

        let vk_from_sk = SchnorrVerificationKey::from(&sk);

        assert_eq!(vk, vk_from_sk.0);
    }

    #[test]
    fn verify_fail_verification_key_not_on_curve() {
        let msg = vec![0, 0, 0, 1];
        let seed = [0u8; 32];
        let mut rng = ChaCha20Rng::from_seed(seed);
        let sk = SchnorrSigningKey::try_generate(&mut rng).unwrap();
        let vk1 = SchnorrVerificationKey::from(&sk);
        let sig = sk.sign(&msg, &mut rng).unwrap();
        let vk2 = SchnorrVerificationKey(JubjubSubgroup::from_raw_unchecked(
            JubjubBase::ONE,
            JubjubBase::ONE,
        ));

        let result1 = sig.verify(&msg, &vk1);
        let result2 = sig.verify(&msg, &vk2);

        result1.expect("Correct verification key used, test should pass.");

        result2.expect_err("Invalid verification key used, test should fail.");
    }

    #[test]
    fn serialize_deserialize_vk() {
        let seed = 0;
        let mut rng = rand_chacha::ChaCha8Rng::seed_from_u64(seed);
        let sk = SchnorrSigningKey::try_generate(&mut rng).unwrap();
        let vk = SchnorrVerificationKey::from(&sk);

        let vk_bytes = vk.to_bytes();
        let vk2 = SchnorrVerificationKey::from_bytes(&vk_bytes).unwrap();

        assert_eq!(vk.0, vk2.0);
    }

    mod golden {

        use rand_chacha::ChaCha20Rng;
        use rand_core::SeedableRng;

        use crate::signature_scheme::{SchnorrSigningKey, SchnorrVerificationKey};

        const GOLDEN_BYTES: &[u8; 32] = &[
            144, 52, 95, 161, 127, 253, 49, 32, 140, 217, 231, 207, 32, 238, 244, 196, 97, 241, 47,
            95, 101, 9, 70, 136, 194, 66, 187, 253, 200, 32, 218, 43,
        ];

        fn golden_value() -> SchnorrVerificationKey {
            let mut rng = ChaCha20Rng::from_seed([0u8; 32]);
            let sk = SchnorrSigningKey::try_generate(&mut rng).unwrap();
            SchnorrVerificationKey::from(&sk)
        }

        #[test]
        fn golden_conversions() {
            let value = SchnorrVerificationKey::from_bytes(GOLDEN_BYTES)
                .expect("This from bytes should not fail");
            assert_eq!(golden_value(), value);

            let serialized = SchnorrVerificationKey::to_bytes(value);
            let golden_serialized = SchnorrVerificationKey::to_bytes(golden_value());
            assert_eq!(golden_serialized, serialized);
        }
    }
}
