use anyhow::{anyhow, Context};
use async_trait::async_trait;
use hex::ToHex;
use slog_scope::{info, trace, warn};
use std::path::PathBuf;
use thiserror::Error;

use mithril_common::crypto_helper::{KESPeriod, ProtocolInitializer};
use mithril_common::entities::{
    PartyId, ProtocolMessage, ProtocolParameters, SingleSignatures, Stake,
};
use mithril_common::protocol::{SignerBuilder, SingleSigner as ProtocolSingleSigner};
use mithril_common::{StdError, StdResult};

use crate::dependency_injection::EpochServiceWrapper;

/// This is responsible for creating new instances of ProtocolInitializer.
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

/// The SingleSigner is the structure responsible for issuing SingleSignatures.
#[cfg_attr(test, mockall::automock)]
#[async_trait]
pub trait SingleSigner: Sync + Send {
    /// Computes single signatures
    async fn compute_single_signatures(
        &self,
        protocol_message: &ProtocolMessage,
    ) -> StdResult<Option<SingleSignatures>>;

    /// Get party id
    fn get_party_id(&self) -> PartyId;
}

/// SingleSigner error structure.
#[derive(Error, Debug)]
pub enum SingleSignerError {
    /// Cryptographic Signer creation error.
    #[error("the protocol signer creation failed")]
    ProtocolSignerCreationFailure(#[source] StdError),

    /// Signature Error
    #[error("Signature Error")]
    SignatureFailed(#[source] StdError),

    /// Avk computation Error
    #[error("Aggregate verification key computation Error")]
    AggregateVerificationKeyComputationFailed(#[source] StdError),
}

/// Implementation of the SingleSigner.
pub struct MithrilSingleSigner {
    party_id: PartyId,
    epoch_service: EpochServiceWrapper,
}

impl MithrilSingleSigner {
    /// Create a new instance of the MithrilSingleSigner.
    pub fn new(party_id: PartyId, epoch_service: EpochServiceWrapper) -> Self {
        Self {
            party_id,
            epoch_service,
        }
    }

    async fn build_protocol_single_signer(&self) -> StdResult<ProtocolSingleSigner> {
        let epoch_service = self.epoch_service.read().await;
        let protocol_initializer =
            epoch_service
                .protocol_initializer()?
                .as_ref()
                .ok_or(anyhow!(
                    "Can not Sign or Compute AVK, No protocol initializer found for party_id: '{}'",
                    self.party_id.clone()
                ))?;

        let builder = SignerBuilder::new(
            &epoch_service.current_signers_with_stake().await?,
            &protocol_initializer.get_protocol_parameters().into(),
        )
        .with_context(|| "Mithril Single Signer can not build signer")
        .map_err(SingleSignerError::ProtocolSignerCreationFailure)?;

        let single_signer = builder
            .restore_signer_from_initializer(self.party_id.clone(), protocol_initializer.clone())
            .with_context(|| {
                format!(
                    "Mithril Single Signer can not restore signer with party_id: '{}'",
                    self.party_id.clone()
                )
            })
            .map_err(SingleSignerError::ProtocolSignerCreationFailure)?;

        Ok(single_signer)
    }
}

#[async_trait]
impl SingleSigner for MithrilSingleSigner {
    async fn compute_single_signatures(
        &self,
        protocol_message: &ProtocolMessage,
    ) -> StdResult<Option<SingleSignatures>> {
        let protocol_single_signer = self.build_protocol_single_signer().await?;

        info!("Signing protocol message"; "protocol_message" =>  #?protocol_message, "signed message" => protocol_message.compute_hash().encode_hex::<String>());
        let signatures = protocol_single_signer
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

    /// Get party id
    fn get_party_id(&self) -> PartyId {
        self.party_id.clone()
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;
    use tokio::sync::RwLock;

    use mithril_common::crypto_helper::ProtocolClerk;
    use mithril_common::entities::{Epoch, ProtocolMessagePartKey};
    use mithril_common::test_utils::MithrilFixtureBuilder;
    use mithril_persistence::store::adapter::{DumbStoreAdapter, MemoryAdapter};
    use mithril_persistence::store::StakeStore;

    use crate::services::MithrilEpochService;
    use crate::store::ProtocolInitializerStore;

    use super::*;

    #[tokio::test]
    async fn compute_single_signature_success() {
        let snapshot_digest = "digest".to_string();
        let fixture = MithrilFixtureBuilder::default().with_signers(5).build();
        let current_signer = &fixture.signers_fixture()[0];
        let clerk = ProtocolClerk::from_signer(&current_signer.protocol_signer);
        let avk = clerk.compute_avk();
        let stake_store = Arc::new(StakeStore::new(
            Box::new(
                MemoryAdapter::new(Some(vec![(
                    Epoch(10).offset_to_signer_retrieval_epoch().unwrap(),
                    fixture.stake_distribution(),
                )]))
                .unwrap(),
            ),
            None,
        ));
        let protocol_initializer_store = Arc::new(ProtocolInitializerStore::new(
            Box::new(DumbStoreAdapter::new()),
            None,
        ));
        let epoch_service = MithrilEpochService::new(stake_store, protocol_initializer_store)
            .set_data_to_default_or_fake(Epoch(10))
            .alter_data(|data| {
                data.protocol_initializer = Some(current_signer.protocol_initializer.clone());
                data.current_signers = fixture.signers();
            });

        let single_signer = MithrilSingleSigner::new(
            current_signer.party_id(),
            Arc::new(RwLock::new(epoch_service)),
        );

        let mut protocol_message = ProtocolMessage::new();
        protocol_message.set_message_part(ProtocolMessagePartKey::SnapshotDigest, snapshot_digest);
        let sign_result = single_signer
            .compute_single_signatures(&protocol_message)
            .await
            .expect("single signer should not fail")
            .expect("single signer should produce a signature here");

        let expected_message = protocol_message.compute_hash().as_bytes().to_vec();
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
}
