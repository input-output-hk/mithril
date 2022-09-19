use super::FromShelleyFile;

use crate::crypto_helper::cardano::{ParseError, ProtocolRegistrationError};
use ed25519_dalek::{PublicKey as EdPublicKey, Signature as EdSignature, Verifier};
use kes_summed_ed25519::common::PublicKey as KesPublicKey;
use mithril::RegisterError;
use serde::{Deserialize, Serialize};
use std::path::Path;

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

impl FromShelleyFile for RawOpCert {
    const TYPE: &'static str = "NodeOperationalCertificate";
    const DESCRIPTION: &'static str = "";
}

/// Parsed Operational Certificate
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
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

    /// Creates a file in the path provided with the OpCert
    pub fn to_file<P: AsRef<Path>>(&self, path: P) -> Result<(), ParseError> {
        let raw_cert = RawOpCert(
            RawFields(
                self.kes_vk.as_bytes().to_vec(),
                self.issue_number,
                self.start_kes_period,
                self.cert_sig.to_bytes().to_vec(),
            ),
            self.cold_vk,
        );
        raw_cert.to_file(path)
    }

    /// Validate a certificate
    pub fn validate(&self) -> Result<(), ProtocolRegistrationError> {
        let mut msg = [0u8; 48];
        msg[..32].copy_from_slice(self.kes_vk.as_bytes());
        msg[32..40].copy_from_slice(&self.issue_number.to_be_bytes());
        msg[40..48].copy_from_slice(&self.start_kes_period.to_be_bytes());

        if self.cold_vk.verify(&msg, &self.cert_sig).is_ok() {
            return Ok(());
        }

        Err(ProtocolRegistrationError::OpCertInvalid)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_vector_op_cert() {
        let cert = OpCert::from_file("./test-data/node1.cert").unwrap();

        assert!(cert.validate().is_ok());

        assert!(cert.to_file("./test-data/node_test.cert").is_ok());

        let cert_test = OpCert::from_file("./test-data/node_test.cert").unwrap();
        assert!(cert_test.validate().is_ok());
    }
}
