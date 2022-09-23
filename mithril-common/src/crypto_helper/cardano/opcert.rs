use super::FromShelleyFile;

use crate::crypto_helper::cardano::ProtocolRegistrationErrorWrapper;
use ed25519_dalek::{PublicKey as EdPublicKey, Signature as EdSignature, Verifier};
use kes_summed_ed25519::common::PublicKey as KesPublicKey;
use serde::de::Error;
use serde::{Deserialize, Deserializer, Serialize, Serializer};

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

/// Parsed Operational Certificate
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct OpCert {
    pub(crate) kes_vk: KesPublicKey,
    pub(crate) issue_number: u64,
    pub(crate) start_kes_period: u64, // this is not the kes period used in signing/verifying
    pub(crate) cert_sig: EdSignature,
    pub(crate) cold_vk: EdPublicKey,
}

impl FromShelleyFile for OpCert {
    const TYPE: &'static str = "NodeOperationalCertificate";
    const DESCRIPTION: &'static str = "";
}

impl OpCert {
    /// Validate a certificate
    pub fn validate(&self) -> Result<(), ProtocolRegistrationErrorWrapper> {
        let mut msg = [0u8; 48];
        msg[..32].copy_from_slice(self.kes_vk.as_bytes());
        msg[32..40].copy_from_slice(&self.issue_number.to_be_bytes());
        msg[40..48].copy_from_slice(&self.start_kes_period.to_be_bytes());

        if self.cold_vk.verify(&msg, &self.cert_sig).is_ok() {
            return Ok(());
        }

        Err(ProtocolRegistrationErrorWrapper::OpCertInvalid)
    }
}

impl Serialize for OpCert {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let raw_cert = RawOpCert(
            RawFields(
                self.kes_vk.as_bytes().to_vec(),
                self.issue_number,
                self.start_kes_period,
                self.cert_sig.to_bytes().to_vec(),
            ),
            self.cold_vk,
        );

        raw_cert.serialize(serializer)
    }
}

impl<'de> Deserialize<'de> for OpCert {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let raw_cert = RawOpCert::deserialize(deserializer)?;
        Ok(Self {
            kes_vk: KesPublicKey::from_bytes(&raw_cert.0 .0)
                .map_err(|_| Error::custom("KES vk serialisation error"))?,
            issue_number: raw_cert.0 .1,
            start_kes_period: raw_cert.0 .2,
            cert_sig: EdSignature::from_bytes(&raw_cert.0 .3)
                .map_err(|_| Error::custom("ed25519 signature serialisation error"))?,
            cold_vk: raw_cert.1,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_vector_op_cert() {
        let cert: OpCert = OpCert::from_file("./test-data/node1.cert").unwrap();

        assert!(cert.validate().is_ok());

        assert!(cert.to_file("./test-data/node_test.cert").is_ok());

        let cert_test: OpCert = OpCert::from_file("./test-data/node_test.cert").unwrap();
        assert!(cert_test.validate().is_ok());
    }
}
