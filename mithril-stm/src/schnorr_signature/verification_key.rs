use anyhow::anyhow;
use dusk_jubjub::SubgroupPoint as JubjubSubgroup;
use group::{Group, GroupEncoding};

pub(crate) use crate::schnorr_signature::signing_key::SchnorrSigningKey;
use crate::{StmResult, error::SchnorrSignatureError};

/// Schnorr verification key, it consists of a point on the Jubjub curve
/// vk = g * sk, where g is a generator
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct SchnorrVerificationKey(pub(crate) JubjubSubgroup);

impl SchnorrVerificationKey {
    /// Convert a `SchnorrVerificationKey` into bytes.
    pub(crate) fn to_bytes(self) -> [u8; 32] {
        self.0.to_bytes()
    }

    /// Convert a string of bytes into a `SchnorrVerificationKey`.
    ///
    /// The bytes must represent a Jubjub Subgroup point or the conversion will fail
    pub(crate) fn from_bytes(bytes: &[u8]) -> StmResult<Self> {
        let mut verification_key_bytes: [u8; 32] = [0u8; 32];
        verification_key_bytes.copy_from_slice(
            bytes
                .get(0..32)
                .ok_or(anyhow!(SchnorrSignatureError::SerializationError))?,
        );
        let point = JubjubSubgroup::from_bytes(&verification_key_bytes)
            .into_option()
            .ok_or(anyhow!(SchnorrSignatureError::SerializationError))?;

        Ok(SchnorrVerificationKey(point))
    }
}

impl From<&SchnorrSigningKey> for SchnorrVerificationKey {
    /// Convert a Shnorr secret key into a verification key
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

    use dusk_jubjub::SubgroupPoint as JubjubSubgroup;
    use group::Group;
    use rand_chacha::ChaCha20Rng;
    use rand_core::SeedableRng;

    use crate::schnorr_signature::{SchnorrSigningKey, SchnorrVerificationKey};

    #[test]
    fn generate_verification_key() {
        let mut rng = ChaCha20Rng::from_seed([0u8; 32]);
        let sk = SchnorrSigningKey::generate(&mut rng).unwrap();
        let g = JubjubSubgroup::generator();
        let vk = g * sk.0;

        let vk_from_sk = SchnorrVerificationKey::from(&sk);

        assert_eq!(vk, vk_from_sk.0);
    }

    #[test]
    fn serialize_deserialize_vk() {
        let seed = 0;
        let mut rng = rand_chacha::ChaCha8Rng::seed_from_u64(seed);
        let sk = SchnorrSigningKey::generate(&mut rng).unwrap();
        let vk = SchnorrVerificationKey::from(&sk);

        let vk_bytes = vk.to_bytes();
        let vk2 = SchnorrVerificationKey::from_bytes(&vk_bytes).unwrap();

        assert_eq!(vk.0, vk2.0);
    }
}
