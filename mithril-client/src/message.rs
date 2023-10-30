use crate::{MithrilResult, MithrilStakeDistribution};
use anyhow::Context;
use mithril_common::entities::{ProtocolMessage, ProtocolMessagePartKey, SignerWithStake};
use mithril_common::messages::SignerWithStakeMessagePart;
use mithril_common::protocol::SignerBuilder;

pub struct Message;

impl Message {
    pub fn compute_mithril_stake_distribution_message(
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
