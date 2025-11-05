
pub use midnight_curves::{
    Fq as JubjubBase, JubjubExtended, JubjubSubgroup,
};


use crate::schnorr_signatures::helper::{get_coordinates, is_on_curve};
use crate::schnorr_signatures::SignatureError;

#[derive(Debug, Clone, Copy, Default)]
pub struct SchnorrVerificationKey(pub JubjubSubgroup);

impl SchnorrVerificationKey {
    pub fn to_field(&self) -> [JubjubBase; 2] {
        let (x, y) = get_coordinates(self.0);
        [x, y]
    }

    pub fn to_bytes(&self) -> [u8; 64] {
        let (x, y) = get_coordinates(self.0);
        let mut bytes = [0u8; 64];
        bytes[0..32].copy_from_slice(&x.to_bytes_le());
        bytes[32..64].copy_from_slice(&y.to_bytes_le());
        bytes
    }

    /// Do we really need to separate the coordinates?
    pub fn from_bytes(bytes: &[u8]) -> Result<Self, SignatureError> {
        let bytes = bytes.get(0..64).ok_or(SignatureError::SerializationError)?;
        let mut u_bytes = [0u8; 32];
        u_bytes.copy_from_slice(&bytes[0..32]);
        let mut v_bytes = [0u8; 32];
        v_bytes.copy_from_slice(&bytes[32..64]);

        let u = JubjubBase::from_bytes_le(&u_bytes)
            .into_option()
            .ok_or(SignatureError::SerializationError)?;
        let v = JubjubBase::from_bytes_le(&v_bytes)
            .into_option()
            .ok_or(SignatureError::SerializationError)?;
        if !bool::from(is_on_curve(u, v)) {
            return Err(SignatureError::SerializationError);
        }

        let point = JubjubSubgroup::from_raw_unchecked(u, v);
        if !bool::from(JubjubExtended::from(point).is_prime_order()) {
            return Err(SignatureError::SerializationError);
        }

        Ok(SchnorrVerificationKey(point))
    }
}
