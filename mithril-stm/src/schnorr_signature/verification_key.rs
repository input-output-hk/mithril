use anyhow::{Result, anyhow};
use group::{Group, GroupEncoding};
pub use midnight_curves::JubjubSubgroup;

pub(crate) use crate::schnorr_signature::signing_key::SchnorrSigningKey;

/// Schnorr verification key, it consists of a point on the Jubjub curve
/// vk = g * sk, where g is a generator
#[derive(Debug, Clone, Copy, Default)]
pub struct SchnorrVerificationKey(pub(crate) JubjubSubgroup);

impl SchnorrVerificationKey {
    pub(crate) fn to_bytes(self) -> [u8; 32] {
        self.0.to_bytes()
    }

    pub(crate) fn from_bytes(bytes: &[u8]) -> Result<Self> {
        let bytes = bytes
            .get(0..32)
            .ok_or(anyhow!("Not enough bytes to create a verification key."))?;

        let point = JubjubSubgroup::from_bytes(bytes.try_into()?)
            .into_option()
            .ok_or(anyhow!("Failed to create the JubjubSubGroup element."))?;

        Ok(SchnorrVerificationKey(point))
    }
}

impl From<&SchnorrSigningKey> for SchnorrVerificationKey {
    /// Convert a Shnorr secret key into a verification key
    ///
    /// This is done by computing `vk = g * sk` where g is the generator
    /// of the subgroup and sk is the schnorr secret key
    fn from(sk: &SchnorrSigningKey) -> Self {
        let g = JubjubSubgroup::generator();

        SchnorrVerificationKey(g * sk.0)
    }
}
