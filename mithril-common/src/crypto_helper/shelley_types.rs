use ed25519_dalek::{PublicKey as EdPublicKey, Signature as EdSignature, Verifier};
use hex::FromHex;
use kes_summed_ed25519::common::PublicKey as KesPublicKey;
use mithril::RegisterError;
use serde::de::DeserializeOwned;
use serde::{Deserialize, Serialize};
use std::fs;
use std::path::Path;

/// Parse error
#[derive(Debug)]
pub enum ParseError {
    Path(std::io::Error),
    JsonFormat(serde_json::Error),
    CborData,
}

/// Fields for a shelley formatted file (holds for vkeys, skeys or certs)
#[derive(Clone, Debug, Default, Serialize, Deserialize)]
struct ShelleyFileFormat {
    #[serde(rename = "type")]
    file_type: String,
    description: String,
    #[serde(rename = "cborHex")]
    cbor_hex: String,
}

pub(crate) trait FromShelleyFile {
    fn from_file<R: DeserializeOwned, P: AsRef<Path>>(path: P) -> Result<R, ParseError> {
        let data = fs::read_to_string(path).map_err(ParseError::Path)?;

        let file: ShelleyFileFormat =
            serde_json::from_str(&data).map_err(ParseError::JsonFormat)?;

        let hex_vector = Vec::from_hex(file.cbor_hex).map_err(|_| ParseError::CborData)?;

        let a: R = serde_cbor::from_slice(&hex_vector).map_err(|_| ParseError::CborData)?;
        Ok(a)
    }
}

/// Raw Fields of the operational certificates (without incluiding the cold VK)
#[derive(Clone, Debug, Deserialize, PartialEq, Eq, Serialize)]
struct RawFields(
    #[serde(with = "serde_bytes")] Vec<u8>,
    u64,
    u64,
    #[serde(with = "serde_bytes")] Vec<u8>,
);

/// Raw Operational Certificate
#[derive(Clone, Debug, Deserialize, PartialEq, Eq, Serialize)]
struct RawOpCert(RawFields, EdPublicKey);

impl FromShelleyFile for RawOpCert {}

/// Parsed Operational Certificate
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct OpCert {
    pub(crate) kes_vk: KesPublicKey,
    pub(crate) issue_number: u64,
    pub(crate) start_kes_period: u64, // this is not the kes period used in signing/verifying
    pub(crate) cert_sig: EdSignature,
    pub(crate) cold_vk: EdPublicKey,
}

impl OpCert {
    /// Parse raw bytes into an Operational Certificate
    pub fn from_file<P: AsRef<Path>>(path: P) -> Result<Self, RegisterError> {
        let a: RawOpCert =
            RawOpCert::from_file(path).map_err(|_| RegisterError::SerializationError)?;

        Ok(Self {
            kes_vk: KesPublicKey::from_bytes(&a.0 .0)
                .map_err(|_| RegisterError::SerializationError)?,
            issue_number: a.0 .1,
            start_kes_period: a.0 .2,
            cert_sig: EdSignature::from_bytes(&a.0 .3)
                .map_err(|_| RegisterError::SerializationError)?,
            cold_vk: a.1,
        })
    }

    /// Validate a certificate
    pub fn validate(&self) -> Result<(), RegisterError> {
        let mut msg = [0u8; 48];
        msg[..32].copy_from_slice(self.kes_vk.as_bytes());
        msg[32..40].copy_from_slice(&self.issue_number.to_be_bytes());
        msg[40..48].copy_from_slice(&self.start_kes_period.to_be_bytes());

        if self.cold_vk.verify(&msg, &self.cert_sig).is_ok() {
            return Ok(());
        }

        Err(RegisterError::InvalidOpCert)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_vector_op_cert() {
        let cert = OpCert::from_file("./test-data/node1.cert").unwrap();

        assert!(cert.validate().is_ok())
    }
}
