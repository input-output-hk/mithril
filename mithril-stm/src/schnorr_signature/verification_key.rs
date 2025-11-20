use anyhow::anyhow;
use dusk_jubjub::SubgroupPoint as JubjubSubgroup;
use group::{Group, GroupEncoding};

pub(crate) use crate::schnorr_signature::signing_key::SchnorrSigningKey;
use crate::{StmResult, error::SchnorrSignatureError};

/// Schnorr verification key, it consists of a point on the Jubjub curve
/// vk = g * sk, where g is a generator
#[derive(Debug, Clone, Copy, Default)]
pub struct SchnorrVerificationKey(pub(crate) JubjubSubgroup);

impl SchnorrVerificationKey {
    /// Convert a `SchnorrVerificationKey` into a string of bytes.
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
