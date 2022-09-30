use super::FromShelleyFile;
use crate::crypto_helper::cardano::ProtocolRegistrationErrorWrapper;
use crate::crypto_helper::ProtocolPartyId;

use bech32::{self, ToBase32, Variant};
use blake2::{digest::consts::U28, Blake2b, Digest};
use ed25519_dalek::{
    ExpandedSecretKey as EdExpandedSecretKey, PublicKey as EdPublicKey, SecretKey as EdSecretKey,
    Signature as EdSignature, Verifier,
};
use kes_summed_ed25519::common::PublicKey as KesPublicKey;
use serde::de::Error;
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use sha2::Sha256;
use thiserror::Error;

/// Operational certificate error
#[derive(Error, Debug, PartialEq, Eq)]
pub enum OpCertError {
    /// Error raised when a pool address encoding fails
    #[error("pool address encoding error")]
    PoolAddressEncoding,
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
    /// OpCert factory / test only
    pub fn new(
        kes_vk: KesPublicKey,
        issue_number: u64,
        start_kes_period: u64,
        cold_sk: EdSecretKey,
    ) -> Self {
        let cold_sk_expanded = EdExpandedSecretKey::from(&cold_sk);
        let cold_vk: EdPublicKey = (&cold_sk_expanded).into();
        let cert_sig = cold_sk_expanded.sign(
            &Self::compute_message_to_sign(&kes_vk, issue_number, start_kes_period),
            &cold_vk,
        );
        Self {
            kes_vk,
            issue_number,
            start_kes_period,
            cert_sig,
            cold_vk,
        }
    }

    /// Compute message to sign
    pub(crate) fn compute_message_to_sign(
        kes_vk: &KesPublicKey,
        issue_number: u64,
        start_kes_period: u64,
    ) -> [u8; 48] {
        let mut msg = [0u8; 48];
        msg[..32].copy_from_slice(kes_vk.as_bytes());
        msg[32..40].copy_from_slice(&issue_number.to_be_bytes());
        msg[40..48].copy_from_slice(&start_kes_period.to_be_bytes());
        msg
    }

    /// Validate a certificate
    pub fn validate(&self) -> Result<(), ProtocolRegistrationErrorWrapper> {
        if self
            .cold_vk
            .verify(
                &Self::compute_message_to_sign(
                    &self.kes_vk,
                    self.issue_number,
                    self.start_kes_period,
                ),
                &self.cert_sig,
            )
            .is_ok()
        {
            return Ok(());
        }

        Err(ProtocolRegistrationErrorWrapper::OpCertInvalid)
    }

    /// Compute protocol party id as pool id bech 32
    pub fn compute_protocol_party_id(&self) -> Result<ProtocolPartyId, OpCertError> {
        let mut hasher = Blake2b::<U28>::new();
        hasher.update(&self.cold_vk.as_bytes());
        let mut pool_id = [0u8; 28];
        pool_id.copy_from_slice(hasher.finalize().as_slice());
        bech32::encode("pool", pool_id.to_base32(), Variant::Bech32)
            .map_err(|_| OpCertError::PoolAddressEncoding)
    }

    /// Compute the hash of an OpCert
    pub fn compute_hash(&self) -> String {
        let mut hasher = Sha256::new();
        hasher.update(self.kes_vk.as_bytes());
        hasher.update(self.issue_number.to_be_bytes());
        hasher.update(self.start_kes_period.to_be_bytes());
        hasher.update(self.cert_sig.to_bytes());
        hasher.update(self.cold_vk.as_bytes());
        hex::encode(hasher.finalize())
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
    use crate::crypto_helper::cardano::ColdKeyGenerator;

    use kes_summed_ed25519::{kes::Sum6Kes, traits::KesSk};
    use std::{fs, path::PathBuf};

    fn setup_temp_directory() -> PathBuf {
        let temp_dir = std::env::temp_dir().join("mithril_cardano_opcert");
        fs::create_dir_all(&temp_dir).expect("temp dir creation should not fail");
        temp_dir
    }

    #[test]
    fn test_vector_opcert() {
        let temp_dir = setup_temp_directory();
        let (cold_secret_key, _) = ColdKeyGenerator::create_deterministic_keypair([0u8; 32]);
        let (_, kes_verification_key) = Sum6Kes::keygen(&mut [0u8; 32]);
        let operational_certificate = OpCert::new(kes_verification_key, 0, 0, cold_secret_key);
        assert!(operational_certificate.validate().is_ok());

        let operation_certificate_file = temp_dir.join("node.cert");
        operational_certificate
            .to_file(&operation_certificate_file)
            .expect("operational certificate file export should not fail");

        let operational_certificate: OpCert = OpCert::from_file(&operation_certificate_file)
            .expect("operational certificate file import should not fail");
        assert!(operational_certificate.validate().is_ok());

        let party_id = operational_certificate
            .compute_protocol_party_id()
            .expect("compute protocol party_id should not fail");
        assert_eq!(
            "pool1mxyec46067n3querj9cxkk0g0zlag93pf3ya9vuyr3wgkq2e6t7".to_string(),
            party_id
        );
    }
}
