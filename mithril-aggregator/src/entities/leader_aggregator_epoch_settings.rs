use mithril_common::entities::{
    CardanoTransactionsSigningConfig, Epoch, ProtocolParameters, Signer,
};

/// LeaderAggregatorEpochSettings represents the settings of an epoch
#[derive(Clone, Debug, PartialEq)]
pub struct LeaderAggregatorEpochSettings {
    /// Current Epoch
    pub epoch: Epoch,

    /// Registration protocol parameters
    pub registration_protocol_parameters: ProtocolParameters,

    /// Current Signers
    pub current_signers: Vec<Signer>,

    /// Signers that will be able to sign on the next epoch
    pub next_signers: Vec<Signer>,

    /// Cardano transactions signing configuration for the current epoch
    pub cardano_transactions_signing_config: Option<CardanoTransactionsSigningConfig>,

    /// Cardano transactions signing configuration for the next epoch
    pub next_cardano_transactions_signing_config: Option<CardanoTransactionsSigningConfig>,
}
