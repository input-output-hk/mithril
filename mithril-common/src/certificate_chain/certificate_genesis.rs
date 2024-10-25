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
        Certificate, CertificateMetadata, CertificateSignature, Epoch, ProtocolMessage,
        ProtocolMessagePartKey, ProtocolParameters,
    },
    protocol::ToMessage,
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
        genesis_protocol_parameters: &ProtocolParameters,
        genesis_avk: &ProtocolAggregateVerificationKey,
        genesis_epoch: &Epoch,
    ) -> StdResult<ProtocolMessage> {
        let genesis_avk = genesis_avk.to_json_hex()?;
        let mut protocol_message = ProtocolMessage::new();
        protocol_message.set_message_part(
            ProtocolMessagePartKey::NextAggregateVerificationKey,
            genesis_avk,
        );
        protocol_message.set_message_part(
            ProtocolMessagePartKey::NextProtocolParameters,
            genesis_protocol_parameters.compute_hash(),
        );
        protocol_message.set_message_part(
            ProtocolMessagePartKey::CurrentEpoch,
            genesis_epoch.to_string(),
        );
        Ok(protocol_message)
    }

    /// Sign the Genesis protocol message (test only)
    pub fn sign_genesis_protocol_message<T: ToMessage>(
        &self,
        genesis_message: T,
    ) -> Result<ProtocolGenesisSignature, CertificateGenesisProducerError> {
        Ok(self
            .genesis_signer
            .as_ref()
            .ok_or_else(CertificateGenesisProducerError::MissingGenesisSigner)?
            .sign(genesis_message.to_message().as_bytes()))
    }

    /// Create a Genesis Certificate
    pub fn create_genesis_certificate<T: Into<String>>(
        protocol_parameters: ProtocolParameters,
        network: T,
        epoch: Epoch,
        genesis_avk: ProtocolAggregateVerificationKey,
        genesis_signature: ProtocolGenesisSignature,
    ) -> StdResult<Certificate> {
        let protocol_version = PROTOCOL_VERSION.to_string();
        let initiated_at = Utc::now();
        let sealed_at = Utc::now();
        let signers = vec![];
        let metadata = CertificateMetadata::new(
            network,
            protocol_version,
            protocol_parameters.clone(),
            initiated_at,
            sealed_at,
            signers,
        );
        let previous_hash = "".to_string();
        let genesis_protocol_message =
            Self::create_genesis_protocol_message(&protocol_parameters, &genesis_avk, &epoch)?;
        Ok(Certificate::new(
            previous_hash,
            epoch,
            metadata,
            genesis_protocol_message,
            genesis_avk,
            CertificateSignature::GenesisSignature(genesis_signature),
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::{entities::ProtocolMessagePartKey, test_utils::MithrilFixtureBuilder};

    #[test]
    fn test_create_genesis_protocol_message_has_expected_keys_and_values() {
        let fixture = MithrilFixtureBuilder::default().with_signers(5).build();
        let genesis_protocol_parameters = fixture.protocol_parameters();
        let genesis_avk = fixture.compute_avk();
        let genesis_epoch = Epoch(123);
        let protocol_message = CertificateGenesisProducer::create_genesis_protocol_message(
            &genesis_protocol_parameters,
            &genesis_avk,
            &genesis_epoch,
        )
        .unwrap();

        let expected_genesis_avk_value = fixture.compute_and_encode_avk();
        assert_eq!(
            protocol_message
                .get_message_part(&ProtocolMessagePartKey::NextAggregateVerificationKey),
            Some(&expected_genesis_avk_value)
        );

        let expected_genesis_protocol_parameters_value = genesis_protocol_parameters.compute_hash();
        assert_eq!(
            protocol_message.get_message_part(&ProtocolMessagePartKey::NextProtocolParameters),
            Some(&expected_genesis_protocol_parameters_value)
        );

        let expected_genesis_epoch = genesis_epoch.to_string();
        assert_eq!(
            protocol_message.get_message_part(&ProtocolMessagePartKey::CurrentEpoch),
            Some(&expected_genesis_epoch)
        );
    }
}
