//! A module used to create a Genesis Certificate
//!
use std::sync::Arc;

use chrono::prelude::*;
use thiserror::Error;

use crate::{
    StdResult,
    crypto_helper::{
        PROTOCOL_VERSION, ProtocolAggregateVerificationKey, ProtocolGenesisSignature,
        ProtocolGenesisSigner, ProtocolKey,
    },
    entities::{
        Certificate, CertificateMetadata, CertificateSignature, Epoch, ProtocolMessage,
        ProtocolMessagePartKey, ProtocolParameters, SupportedEra,
    },
    protocol::ToMessage,
};

#[cfg(feature = "future_snark")]
use crate::crypto_helper::ProtocolAggregateVerificationKeyForSnark;

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
        mithril_era: SupportedEra,
    ) -> StdResult<ProtocolMessage> {
        let genesis_aggregate_verification_key_for_concatenation =
            ProtocolKey::new(genesis_avk.to_concatenation_aggregate_verification_key().to_owned());
        let genesis_concatenation_avk =
            genesis_aggregate_verification_key_for_concatenation.to_json_hex()?;
        let mut protocol_message = ProtocolMessage::new();
        protocol_message.set_message_part(
            ProtocolMessagePartKey::NextAggregateVerificationKey,
            genesis_concatenation_avk,
        );

        if mithril_era != SupportedEra::Pythagoras {
            #[cfg(feature = "future_snark")]
            match genesis_avk.to_snark_aggregate_verification_key() {
                Some(snark_avk) => {
                    let genesis_snark_avk: ProtocolAggregateVerificationKeyForSnark =
                        ProtocolKey::new(snark_avk.to_owned());
                    protocol_message.set_message_part(
                        ProtocolMessagePartKey::NextSnarkAggregateVerificationKey,
                        genesis_snark_avk.to_bytes_hex()?,
                    );
                }
                None => {
                    eprintln!(
                        "WARNING: SNARK aggregate verification key is unavailable, genesis certificate will not include SNARK AVK"
                    );
                }
            }
        }

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
        mithril_era: SupportedEra,
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
        let genesis_protocol_message = Self::create_genesis_protocol_message(
            &protocol_parameters,
            &genesis_avk,
            &epoch,
            mithril_era,
        )?;
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

    use crate::{entities::ProtocolMessagePartKey, test::builder::MithrilFixtureBuilder};

    #[test]
    fn test_create_genesis_protocol_message_has_expected_keys_and_values() {
        let fixture = MithrilFixtureBuilder::default().with_signers(5).build();
        let genesis_protocol_parameters = fixture.protocol_parameters();
        let genesis_avk = fixture.compute_aggregate_verification_key();
        let genesis_epoch = Epoch(123);
        let protocol_message = CertificateGenesisProducer::create_genesis_protocol_message(
            &genesis_protocol_parameters,
            &genesis_avk,
            &genesis_epoch,
            SupportedEra::Pythagoras,
        )
        .unwrap();

        let expected_genesis_avk_value =
            fixture.compute_and_encode_concatenation_aggregate_verification_key();
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

    #[cfg(feature = "future_snark")]
    #[test]
    fn genesis_protocol_message_includes_snark_aggregate_verification_key() {
        let fixture = MithrilFixtureBuilder::default().with_signers(5).build();
        let genesis_protocol_parameters = fixture.protocol_parameters();
        let genesis_avk = fixture.compute_aggregate_verification_key();
        let genesis_epoch = Epoch(123);
        let protocol_message = CertificateGenesisProducer::create_genesis_protocol_message(
            &genesis_protocol_parameters,
            &genesis_avk,
            &genesis_epoch,
            SupportedEra::Lagrange,
        )
        .unwrap();

        let expected_snark_avk_value = fixture
            .compute_and_encode_snark_aggregate_verification_key()
            .expect("SNARK AVK should be available");
        assert_eq!(
            protocol_message
                .get_message_part(&ProtocolMessagePartKey::NextSnarkAggregateVerificationKey),
            Some(&expected_snark_avk_value)
        );
    }
}
