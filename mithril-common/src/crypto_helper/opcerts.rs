use ed25519_dalek::{PublicKey as EdPublicKey, Signature as EdSignature, Verifier};
use kes_summed_ed25519::common::PublicKey as KesPublicKey;
use mithril::RegisterError;
use serde::{Deserialize, Serialize};

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

impl OpCert {
    /// Parse raw bytes into an Operational Certificate
    pub fn parse(bytes: &[u8]) -> Result<Self, RegisterError> {
        let a: RawOpCert =
            serde_cbor::from_slice(bytes).map_err(|_| RegisterError::SerializationError)?;

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
    use hex::FromHex;

    #[test]
    fn test_vector_op_cert() {
        let cbor_bytes = Vec::from_hex("8284582067fd5ccf770c0182a34d2b3d2011ca3a853ba947e17cae7543e668bc7687eb6a0000584050592bef1c630f2df499161d78bfadb44cc76cfd24048993ace4a45dade37b4f29e95172fde4e63581a93552f6986985616b70f61062a1db2ee0d3d8e671440e58202abf3ff537a2080f53fa38615906fa6094d44860902f2b2dffdbb41b811ff39f").expect("Invalid Hex String");
        let cert = OpCert::parse(&cbor_bytes).unwrap();

        assert!(cert.validate().is_ok())
    }
}
