use anyhow::Context;
use mithril_common::protocol::SignerBuilder;
#[cfg(feature = "unstable")]
use mithril_common::signable_builder::CardanoStakeDistributionSignableBuilder;
#[cfg(feature = "fs")]
use mithril_common::{
    digesters::{CardanoImmutableDigester, ImmutableDigester},
    entities::SignedEntityType,
};
use slog::{o, Logger};
#[cfg(feature = "fs")]
use std::path::Path;
#[cfg(feature = "fs")]
use std::sync::Arc;

use crate::common::{ProtocolMessage, ProtocolMessagePartKey};
#[cfg(feature = "unstable")]
use crate::CardanoStakeDistribution;
#[cfg(any(feature = "fs", feature = "unstable"))]
use crate::MithrilCertificate;
#[cfg(feature = "unstable")]
use crate::VerifiedCardanoTransactions;
use crate::{MithrilResult, MithrilSigner, MithrilStakeDistribution};

/// A [MessageBuilder] can be used to compute the message of Mithril artifacts.
pub struct MessageBuilder {
    #[cfg(feature = "fs")]
    immutable_digester: Option<Arc<dyn ImmutableDigester>>,
    logger: Logger,
}

impl MessageBuilder {
    /// Constructs a new `MessageBuilder`.
    pub fn new() -> MessageBuilder {
        let logger = Logger::root(slog::Discard, o!());
        Self {
            #[cfg(feature = "fs")]
            immutable_digester: None,
            logger,
        }
    }

    /// Set the [Logger] to use.
    pub fn with_logger(mut self, logger: Logger) -> Self {
        self.logger = logger;
        self
    }

    cfg_fs! {
        fn get_immutable_digester(&self) -> Arc<dyn ImmutableDigester> {
            match self.immutable_digester.as_ref() {
                None => Arc::new(CardanoImmutableDigester::new(None, self.logger.clone())),
                Some(digester) => digester.clone(),
            }
        }

        /// Set the [ImmutableDigester] to be used for the message computation for snapshot.
        ///
        /// If not set a default implementation will be used.
        pub fn with_immutable_digester(
            mut self,
            immutable_digester: Arc<dyn ImmutableDigester>,
        ) -> Self {
            self.immutable_digester = Some(immutable_digester);
            self
        }

        /// Compute message for a snapshot (based on the directory where it was unpacked).
        ///
        /// Warning: this operation can be quite long depending on the snapshot size.
        pub async fn compute_snapshot_message(
            &self,
            snapshot_certificate: &MithrilCertificate,
            unpacked_snapshot_directory: &Path,
        ) -> MithrilResult<ProtocolMessage> {
            let digester = self.get_immutable_digester();
            let beacon =
                match &snapshot_certificate.signed_entity_type {
                SignedEntityType::CardanoImmutableFilesFull(beacon) => {Ok(beacon)},
                other => {
                    Err(anyhow::anyhow!(
                            "Can't compute message: Given certificate `{}` does not certify a snapshot, certificate signed entity: {:?}",
                            snapshot_certificate.hash,
                            other
                        )
                    )},
            }?;

            let mut message = snapshot_certificate.protocol_message.clone();

            let digest = digester
                .compute_digest(unpacked_snapshot_directory, beacon)
                .await
                .with_context(|| {
                    format!(
                        "Snapshot digest computation failed: unpacked_dir: '{}'",
                        unpacked_snapshot_directory.display()
                    )
                })?;
            message.set_message_part(ProtocolMessagePartKey::SnapshotDigest, digest);

            Ok(message)
        }
    }

    /// Compute message for a Mithril stake distribution.
    pub fn compute_mithril_stake_distribution_message(
        &self,
        mithril_stake_distribution: &MithrilStakeDistribution,
    ) -> MithrilResult<ProtocolMessage> {
        let signers =
            MithrilSigner::try_into_signers(mithril_stake_distribution.signers_with_stake.clone())
                .with_context(|| "Could not compute message: conversion failure")?;

        let signer_builder =
            SignerBuilder::new(&signers, &mithril_stake_distribution.protocol_parameters)
                .with_context(|| {
                    "Could not compute message: aggregate verification key computation failed"
                })?;

        let avk = signer_builder
            .compute_aggregate_verification_key()
            .to_json_hex()
            .with_context(|| {
                "Could not compute message: aggregate verification key encoding failed"
            })?;

        let mut message = ProtocolMessage::new();
        message.set_message_part(ProtocolMessagePartKey::NextAggregateVerificationKey, avk);

        Ok(message)
    }

    cfg_unstable! {
        /// Compute message for a Cardano Transactions Proofs.
        pub fn compute_cardano_transactions_proofs_message(
            &self,
            transactions_proofs_certificate: &MithrilCertificate,
            verified_transactions: &VerifiedCardanoTransactions,
        ) -> ProtocolMessage {
            let mut message = transactions_proofs_certificate.protocol_message.clone();
            verified_transactions.fill_protocol_message(&mut message);
            message
        }

        /// Compute message for a Cardano stake distribution.
        pub fn compute_cardano_stake_distribution_message(
            &self,
            certificate: &MithrilCertificate,
            cardano_stake_distribution: &CardanoStakeDistribution,
        ) -> MithrilResult<ProtocolMessage> {
            let mk_tree = CardanoStakeDistributionSignableBuilder::compute_merkle_tree_from_stake_distribution(
                cardano_stake_distribution.stake_distribution.clone(),
            )?;

            let mut message = certificate.protocol_message.clone();
            message.set_message_part(
                ProtocolMessagePartKey::CardanoStakeDistributionEpoch,
                cardano_stake_distribution.epoch.to_string(),
            );
            message.set_message_part(
                ProtocolMessagePartKey::CardanoStakeDistributionMerkleRoot,
                mk_tree.compute_root()?.to_hex(),
            );

            Ok(message)
        }
    }
}

impl Default for MessageBuilder {
    fn default() -> Self {
        Self::new()
    }
}
