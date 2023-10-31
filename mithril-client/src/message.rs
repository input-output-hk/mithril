use crate::{MithrilResult, MithrilStakeDistribution, Snapshot};
use anyhow::Context;
use mithril_common::digesters::{CardanoImmutableDigester, ImmutableDigester};
use mithril_common::entities::{ProtocolMessage, ProtocolMessagePartKey};
use mithril_common::messages::SignerWithStakeMessagePart;
use mithril_common::protocol::SignerBuilder;
use slog::{o, Logger};
use std::path::Path;
use std::sync::Arc;

pub struct MessageBuilder {
    immutable_digester: Option<Arc<dyn ImmutableDigester>>,
    logger: Logger,
}

impl MessageBuilder {
    pub fn new() -> MessageBuilder {
        let logger = Logger::root(slog::Discard, o!());
        Self {
            immutable_digester: None,
            logger,
        }
    }

    pub fn with_immutable_digester(
        mut self,
        immutable_digester: Arc<dyn ImmutableDigester>,
    ) -> Self {
        self.immutable_digester = Some(immutable_digester);
        self
    }

    pub fn with_logger(mut self, logger: Logger) -> Self {
        self.logger = logger;
        self
    }

    fn get_immutable_digester(&self) -> Arc<dyn ImmutableDigester> {
        match self.immutable_digester.as_ref() {
            None => Arc::new(CardanoImmutableDigester::new(None, self.logger.clone())),
            Some(digester) => digester.clone(),
        }
    }

    pub async fn compute_snapshot_message(
        &self,
        snapshot: &Snapshot,
        unpacked_snapshot_directory: &Path,
    ) -> MithrilResult<ProtocolMessage> {
        todo!()
    }

    pub fn compute_mithril_stake_distribution_message(
        &self,
        mithril_stake_distribution: &MithrilStakeDistribution,
    ) -> MithrilResult<ProtocolMessage> {
        let signers = SignerWithStakeMessagePart::try_into_signers(
            mithril_stake_distribution.signers_with_stake.clone(),
        )
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
}
