//! A module used to create a Genesis Certificate
//!
use chrono::prelude::*;
use hex::ToHex;
use thiserror::Error;

use crate::{
    crypto_helper::{
        key_encode_hex, ProtocolAggregateVerificationKey, ProtocolGenesisSigner, PROTOCOL_VERSION,
    },
    entities::{
        Beacon, Certificate, CertificateMetadata, ProtocolMessage, ProtocolMessagePartKey,
        ProtocolParameters,
    },
};

/// [CertificateGenesisProducer] related errors.
#[derive(Error, Debug)]
pub enum CertificateGenesisProducerError {
    /// Error raised when a Codec error occurs
    #[error("codec error: '{0}'")]
    Codec(String),
}

/// CertificateGenesisProducer is in charge of producing a Genesis Certificate
#[derive(Debug)]
pub struct CertificateGenesisProducer {
    genesis_signer: ProtocolGenesisSigner,
}

impl CertificateGenesisProducer {
    /// CertificateGenesisProducer factory
    pub fn new(genesis_signer: ProtocolGenesisSigner) -> Self {
        Self { genesis_signer }
    }

    /// Create a Genesis Certificate
    pub fn create_genesis_certificate(
        &self,
        protocol_parameters: ProtocolParameters,
        beacon: Beacon,
        genesis_avk: ProtocolAggregateVerificationKey,
    ) -> Result<Certificate, CertificateGenesisProducerError> {
        let protocol_version = PROTOCOL_VERSION.to_string();
        let initiated_at = format!("{:?}", Utc::now());
        let sealed_at = format!("{:?}", Utc::now());
        let signers = vec![];
        let metadata = CertificateMetadata::new(
            protocol_version,
            protocol_parameters,
            initiated_at,
            sealed_at,
            signers,
        );
        let genesis_avk =
            key_encode_hex(genesis_avk).map_err(CertificateGenesisProducerError::Codec)?;
        let mut protocol_message = ProtocolMessage::new();
        protocol_message.set_message_part(
            ProtocolMessagePartKey::NextAggregateVerificationKey,
            genesis_avk.clone(),
        );
        let previous_hash = "".to_string();
        let multi_signature = "".to_string();
        let genesis_signature = self
            .genesis_signer
            .sign(protocol_message.compute_hash().as_bytes());
        let genesis_signature = genesis_signature.to_bytes().encode_hex::<String>();
        Ok(Certificate::new(
            previous_hash,
            beacon,
            metadata,
            protocol_message,
            genesis_avk,
            multi_signature,
            genesis_signature,
        ))
    }
}
