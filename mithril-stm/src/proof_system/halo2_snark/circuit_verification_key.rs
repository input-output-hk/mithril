use midnight_zk_stdlib::MidnightVK;
use serde::{Deserialize, Serialize};

#[cfg(test)]
use crate::codec::TryToBytes;
use crate::{StmResult, codec::TryFromBytes};

/// Wrapper type of MidnightVK, the circuit verification key
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct CircuitVerificationKey(
    #[serde(with = "midnight_certificate_verification_key_serde")] MidnightVK,
);

impl CircuitVerificationKey {
    /// Creates a new CircuitVerificationKey from a MidnightVK
    pub fn new(midnight_vk: MidnightVK) -> Self {
        CircuitVerificationKey(midnight_vk)
    }

    /// Returns a copy of the wrapped MidnightVK
    pub fn get_midnight_vk(&self) -> MidnightVK {
        self.0.clone()
    }

    /// Convert the CircuitVerificationKey into bytes using the underlying
    /// MidnightVK functionalities
    #[cfg(test)]
    pub fn to_bytes(&self) -> StmResult<Vec<u8>> {
        self.0.to_bytes_vec()
    }

    /// Converts bytes into a CircuitVerificationKey by using the MidnightVK
    /// read function
    pub fn from_bytes(bytes: &[u8]) -> StmResult<Self> {
        Ok(Self(MidnightVK::try_from_bytes(bytes)?))
    }
}

/// Serialize and deserialize functions for the certificate circuit [MidnightVK].
///
/// [MidnightVK] serialization carries the circuit architecture, so deserialization rebuilds the
/// correct constraint system and round-trips byte-for-byte.
pub(crate) mod midnight_certificate_verification_key_serde {
    use midnight_proofs::utils::SerdeFormat;
    use midnight_zk_stdlib::MidnightVK;
    use serde::{Deserializer, Serializer};

    /// Serialization function based on the write function of the MidnightVK
    pub fn serialize<S: Serializer>(
        verification_key: &MidnightVK,
        serializer: S,
    ) -> Result<S::Ok, S::Error> {
        let mut buf = Vec::new();
        verification_key
            .write(&mut buf, SerdeFormat::RawBytes)
            .map_err(serde::ser::Error::custom)?;
        serializer.serialize_bytes(&buf)
    }

    /// Deserialization function based on the read function of the MidnightVK
    pub fn deserialize<'de, D: Deserializer<'de>>(deserializer: D) -> Result<MidnightVK, D::Error> {
        let bytes: Vec<u8> = serde::Deserialize::deserialize(deserializer)?;
        MidnightVK::read(&mut bytes.as_slice(), SerdeFormat::RawBytes)
            .map_err(serde::de::Error::custom)
    }
}
