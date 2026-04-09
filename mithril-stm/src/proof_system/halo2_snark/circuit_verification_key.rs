use midnight_proofs::utils::SerdeFormat;
use midnight_zk_stdlib::MidnightVK;
use serde::{Deserialize, Serialize};

use crate::StmResult;

/// Wrapper type of MidnightVK, the circuit verification key
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct CircuitVerificationKey(#[serde(with = "midnight_vk_serde")] MidnightVK);

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
    // TODO: remove this once the 'to_bytes' function is used.
    #[allow(dead_code)]
    pub fn to_bytes(&self) -> StmResult<Vec<u8>> {
        let mut buf_cvk = vec![];
        self.0.write(&mut buf_cvk, SerdeFormat::RawBytes)?;
        Ok(buf_cvk)
    }

    /// Converts bytes into a CircuitVerificationKey by using the MidnightVK
    /// read function
    pub fn from_bytes(bytes: &[u8]) -> StmResult<Self> {
        let mut circuit_vk_bytes = bytes;
        let circuit_verification_key =
            MidnightVK::read(&mut circuit_vk_bytes, SerdeFormat::RawBytes)?;
        Ok(Self(circuit_verification_key))
    }
}

/// Module implementing serialize and deserialize functions
/// for the MidnightVK struct
pub mod midnight_vk_serde {
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
