use midnight_proofs::utils::SerdeFormat;
use midnight_zk_stdlib::MidnightVK;
use serde::{Deserialize, Serialize};

use crate::StmResult;

/// Wrapper type of MidnightVK, the circuit verification key
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct CircuitVerificationKey(#[serde(with = "midnight_vk_serde")] MidnightVK);

impl CircuitVerificationKey {
    /// Convert the CircuitVerificationKey into bytes using the underlying
    /// MidnightVK functionalities
    pub fn to_bytes(self) -> StmResult<Vec<u8>> {
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

    pub fn serialize<S: Serializer>(vk: &MidnightVK, serializer: S) -> Result<S::Ok, S::Error> {
        let mut buf = Vec::new();
        vk.write(&mut buf, SerdeFormat::RawBytes)
            .map_err(serde::ser::Error::custom)?;
        serializer.serialize_bytes(&buf)
    }

    pub fn deserialize<'de, D: Deserializer<'de>>(deserializer: D) -> Result<MidnightVK, D::Error> {
        let bytes: Vec<u8> = serde::Deserialize::deserialize(deserializer)?;
        MidnightVK::read(&mut bytes.as_slice(), SerdeFormat::RawBytes)
            .map_err(serde::de::Error::custom)
    }
}
