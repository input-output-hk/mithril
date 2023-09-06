//! A module used to create a Genesis Certificate
//!
use std::sync::Arc;

use chrono::prelude::*;
use thiserror::Error;

use crate::{
    crypto_helper::{
        ProtocolAggregateVerificationKey, ProtocolGenesisSignature, ProtocolGenesisSigner,
        PROTOCOL_VERSION,
    },
    entities::{
        Beacon, Certificate, CertificateMetadata, CertificateSignature, ProtocolMessage,
        ProtocolMessagePartKey, ProtocolParameters,
    },
    StdResult,
};

/// [CertificateGenesisProducer] related errors.
#[derive(Error, Debug)]
pub enum CertificateGenesisProducerError {
    /// Error raised when there is no genesis signer available
    #[error("missing genesis signer error")]
    MissingGenesisSigner(),
}

/// CertificateGenesisProducer is in charge of producing a Genesis Certificate
#[derive(Debug)]
pub struct CertificateGenesisProducer {
    genesis_signer: Option<Arc<ProtocolGenesisSigner>>,
}

impl CertificateGenesisProducer {
    /// CertificateGenesisProducer factory
    pub fn new(genesis_signer: Option<Arc<ProtocolGenesisSigner>>) -> Self {
        Self { genesis_signer }
    }

    /// Create the Genesis protocol message
    pub fn create_genesis_protocol_message(
        genesis_avk: &ProtocolAggregateVerificationKey,
    ) -> StdResult<ProtocolMessage> {
        let genesis_avk = genesis_avk.to_json_hex()?;
        let mut protocol_message = ProtocolMessage::new();
        protocol_message.set_message_part(
            ProtocolMessagePartKey::NextAggregateVerificationKey,
            genesis_avk,
        );
        Ok(protocol_message)
    }

    /// Sign the Genesis protocol message (test only)
    pub fn sign_genesis_protocol_message(
        &self,
        genesis_protocol_message: ProtocolMessage,
    ) -> Result<ProtocolGenesisSignature, CertificateGenesisProducerError> {
        Ok(self
            .genesis_signer
            .as_ref()
            .ok_or_else(CertificateGenesisProducerError::MissingGenesisSigner)?
            .sign(genesis_protocol_message.compute_hash().as_bytes()))
    }

    /// Create a Genesis Certificate
    pub fn create_genesis_certificate(
        protocol_parameters: ProtocolParameters,
        beacon: Beacon,
        genesis_avk: ProtocolAggregateVerificationKey,
        genesis_signature: ProtocolGenesisSignature,
    ) -> StdResult<Certificate> {
        let protocol_version = PROTOCOL_VERSION.to_string();
        let initiated_at = Utc::now();
        let sealed_at = Utc::now();
        let signers = vec![];
        let metadata = CertificateMetadata::new(
            protocol_version,
            protocol_parameters,
            initiated_at,
            sealed_at,
            signers,
        );
        let previous_hash = "".to_string();
        let genesis_protocol_message = Self::create_genesis_protocol_message(&genesis_avk)?;
        Ok(Certificate::new(
            previous_hash,
            beacon,
            metadata,
            genesis_protocol_message,
            genesis_avk,
            CertificateSignature::GenesisSignature(genesis_signature),
        ))
    }
}
