use mithril_common::entities::{Epoch, PartyId, Stake, StakeDistribution};
use serde::{Deserialize, Serialize};

/// Message structure of signer registrations for an epoch.
#[derive(Clone, Debug, PartialEq, Eq, Default, Serialize, Deserialize)]
pub struct SignerRegistrationsMessage {
    /// The epoch at which the registration was sent.
    pub registered_at: Epoch,

    /// The epoch at which the registration was able to send signatures.
    pub signing_at: Epoch,

    /// The signer registrations
    pub registrations: Vec<SignerRegistrationsListItemMessage>,
}

/// Message structure of a signer registration
#[derive(Clone, Debug, PartialEq, Eq, Default, Serialize, Deserialize)]
pub struct SignerRegistrationsListItemMessage {
    /// The registered signer party id
    pub party_id: PartyId,

    /// The registered signer stake
    pub stake: Stake,
}

impl SignerRegistrationsMessage {
    /// Build a [SignerRegistrationsMessage] from a [stake distribution][StakeDistribution].
    pub fn new(registered_at: Epoch, stake_distribution: StakeDistribution) -> Self {
        let registrations: Vec<SignerRegistrationsListItemMessage> = stake_distribution
            .into_iter()
            .map(|(party_id, stake)| SignerRegistrationsListItemMessage { party_id, stake })
            .collect();

        Self {
            registered_at,
            signing_at: registered_at.offset_to_signer_signing_offset(),
            registrations,
        }
    }
}
