use anyhow::{anyhow, Context};
use hex::ToHex;
use slog_scope::{info, trace, warn};
use std::path::PathBuf;
use thiserror::Error;

use mithril_common::crypto_helper::{KESPeriod, ProtocolInitializer};
use mithril_common::entities::{
    PartyId, ProtocolMessage, ProtocolParameters, SignerWithStake, SingleSignatures, Stake,
};
use mithril_common::protocol::SignerBuilder;
use mithril_common::{StdError, StdResult};

#[cfg(test)]
use mockall::automock;

/// This is responsible of creating new instances of ProtocolInitializer.
pub struct MithrilProtocolInitializerBuilder {}

impl MithrilProtocolInitializerBuilder {
    /// Create a ProtocolInitializer instance.
    pub fn build(
        stake: &Stake,
        protocol_parameters: &ProtocolParameters,
        kes_secret_key_path: Option<PathBuf>,
        kes_period: Option<KESPeriod>,
    ) -> StdResult<ProtocolInitializer> {
        let mut rng = rand_core::OsRng;
        let protocol_initializer = ProtocolInitializer::setup(
            protocol_parameters.to_owned().into(),
            kes_secret_key_path,
            kes_period,
            stake.to_owned(),
            &mut rng,
        )?;

        Ok(protocol_initializer)
    }
}

/// The SingleSigner is the structure responsible of issuing SingleSignatures.
#[cfg_attr(test, automock)]
pub trait SingleSigner: Sync + Send {
    /// Computes single signatures
    fn compute_single_signatures(
        &self,
        protocol_message: &ProtocolMessage,
        signers_with_stake: &[SignerWithStake],
        protocol_initializer: &ProtocolInitializer,
    ) -> StdResult<Option<SingleSignatures>>;

    /// Compute aggregate verification key from stake distribution
    fn compute_aggregate_verification_key(
        &self,
        signers_with_stake: &[SignerWithStake],
        protocol_initializer: &ProtocolInitializer,
    ) -> StdResult<Option<String>>;

    /// Get party id
    fn get_party_id(&self) -> PartyId;
}

/// SingleSigner error structure.
#[derive(Error, Debug)]
pub enum SingleSignerError {
    /// Cryptographic Signer creation error.
    #[error("the protocol signer creation failed: `{0}`")]
    ProtocolSignerCreationFailure(StdError),

    /// Signature Error
    #[error("Signature Error: {0:?}")]
    SignatureFailed(StdError),

    /// Avk computation Error
    #[error("Aggregate verification key computation Error: {0:?}")]
    AggregateVerificationKeyComputationFailed(StdError),
}

/// Implementation of the SingleSigner.
pub struct MithrilSingleSigner {
    party_id: PartyId,
}

impl MithrilSingleSigner {
    /// Create a new instance of the MithrilSingleSigner.
    pub fn new(party_id: PartyId) -> Self {
        Self { party_id }
    }
}

impl SingleSigner for MithrilSingleSigner {
    fn compute_single_signatures(
        &self,
        protocol_message: &ProtocolMessage,
        signers_with_stake: &[SignerWithStake],
        protocol_initializer: &ProtocolInitializer,
    ) -> StdResult<Option<SingleSignatures>> {
        let builder = SignerBuilder::new(
            signers_with_stake,
            &protocol_initializer.get_protocol_parameters().into(),
        )
        .with_context(|| "Mithril Single Signer can not build signer")
        .map_err(|e| SingleSignerError::ProtocolSignerCreationFailure(anyhow!(e)))?;
        info!("Signing protocol message"; "protocol_message" =>  #?protocol_message, "signed message" => protocol_message.compute_hash().encode_hex::<String>());
        let signatures = builder
            .restore_signer_from_initializer(self.party_id.clone(), protocol_initializer.clone())
            .with_context(|| {
                format!(
                    "Mithril Single Signer can not restore signer with party_id: '{}'",
                    self.party_id.clone()
                )
            })
            .map_err(|e| SingleSignerError::ProtocolSignerCreationFailure(anyhow!(e)))?
            .sign(protocol_message)
            .with_context(|| {
                format!(
                    "Mithril Single Signer can not sign protocol_message: '{:?}'",
                    protocol_message
                )
            })
            .map_err(SingleSignerError::SignatureFailed)?;

        match &signatures {
            Some(signature) => {
                trace!(
                    "Party #{}: lottery #{:?} won",
                    signature.party_id,
                    &signature.won_indexes
                );
            }
            None => {
                warn!("no signature computed, all lotteries were lost");
            }
        };

        Ok(signatures)
    }

    /// Compute aggregate verification key from stake distribution
    fn compute_aggregate_verification_key(
        &self,
        signers_with_stake: &[SignerWithStake],
        protocol_initializer: &ProtocolInitializer,
    ) -> StdResult<Option<String>> {
        let signer_builder = SignerBuilder::new(
            signers_with_stake,
            &protocol_initializer.get_protocol_parameters().into(),
        )
        .with_context(|| "Mithril Single Signer can not compute aggregate verification key")
        .map_err(SingleSignerError::AggregateVerificationKeyComputationFailed)?;

        let encoded_avk = signer_builder
            .compute_aggregate_verification_key()
            .to_json_hex()
            .with_context(|| {
                "Mithril Single Signer can not serialize aggregate verification key"
            })?;

        Ok(Some(encoded_avk))
    }

    /// Get party id
    fn get_party_id(&self) -> PartyId {
        self.party_id.clone()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use mithril_common::{
        crypto_helper::ProtocolClerk, entities::ProtocolMessagePartKey,
        test_utils::MithrilFixtureBuilder,
    };

    #[test]
    fn compute_single_signature_success() {
        let snapshot_digest = "digest".to_string();
        let fixture = MithrilFixtureBuilder::default().with_signers(5).build();
        let signers_with_stake = fixture.signers_with_stake();
        let current_signer = &fixture.signers_fixture()[0];
        let single_signer = MithrilSingleSigner::new(current_signer.party_id());
        let clerk = ProtocolClerk::from_signer(&current_signer.protocol_signer);
        let avk = clerk.compute_avk();
        let mut protocol_message = ProtocolMessage::new();
        protocol_message.set_message_part(ProtocolMessagePartKey::SnapshotDigest, snapshot_digest);
        let expected_message = protocol_message.compute_hash().as_bytes().to_vec();

        let sign_result = single_signer
            .compute_single_signatures(
                &protocol_message,
                &signers_with_stake,
                &current_signer.protocol_initializer,
            )
            .expect("single signer should not fail")
            .expect("single signer should produce a signature here");

        let decoded_sig = sign_result.to_protocol_signature();
        assert!(
            decoded_sig
                .verify(
                    &fixture.protocol_parameters().into(),
                    &current_signer.protocol_signer.verification_key(),
                    &current_signer.protocol_signer.get_stake(),
                    &avk,
                    &expected_message
                )
                .is_ok(),
            "produced single signature should be valid"
        );
    }

    #[test]
    fn compute_aggregate_verification_key_success() {
        let fixture = MithrilFixtureBuilder::default().with_signers(5).build();
        let signers_with_stake = fixture.signers_with_stake();
        let current_signer = &fixture.signers_fixture()[0];
        let single_signer =
            MithrilSingleSigner::new(current_signer.signer_with_stake.party_id.to_owned());

        single_signer
            .compute_aggregate_verification_key(
                &signers_with_stake,
                &current_signer.protocol_initializer,
            )
            .expect("compute aggregate verification signature should not fail")
            .expect("aggregate verification signature should not be empty");
    }
}
