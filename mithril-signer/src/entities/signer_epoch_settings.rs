use mithril_common::entities::{
    CardanoTransactionsSigningConfig, Epoch, ProtocolParameters, Signer,
};

/// SignerEpochSettings represents the settings of an epoch
#[derive(Clone, Debug, PartialEq)]
pub struct SignerEpochSettings {
    /// Current Epoch
    pub epoch: Epoch,

    /// Current Protocol parameters
    pub protocol_parameters: ProtocolParameters,

    /// Next Protocol parameters
    pub next_protocol_parameters: ProtocolParameters,

    /// Current Signers
    pub current_signers: Vec<Signer>,

    /// Signers that will be able to sign on the next epoch
    pub next_signers: Vec<Signer>,

    /// Cardano transactions signing configuration for the current epoch
    pub cardano_transactions_signing_config: Option<CardanoTransactionsSigningConfig>,

    /// Cardano transactions signing configuration for the next epoch
    pub next_cardano_transactions_signing_config: Option<CardanoTransactionsSigningConfig>,
}

impl SignerEpochSettings {
    #[cfg(test)]
    /// Create a dummy SignerEpochSettings
    pub fn dummy() -> SignerEpochSettings {
        use mithril_common::test_utils::fake_data;

        // Beacon
        let beacon = fake_data::beacon();

        // Protocol parameters
        let protocol_parameters = fake_data::protocol_parameters();
        let next_protocol_parameters = protocol_parameters.clone();

        // Signers
        let signers = fake_data::signers(5);
        let current_signers = signers[1..3].to_vec();
        let next_signers = signers[2..5].to_vec();

        // Cardano transactions signing configuration
        let cardano_transactions_signing_config = Some(CardanoTransactionsSigningConfig::dummy());
        let next_cardano_transactions_signing_config =
            Some(CardanoTransactionsSigningConfig::dummy());

        // Signer Epoch settings
        SignerEpochSettings {
            epoch: beacon.epoch,
            protocol_parameters,
            next_protocol_parameters,
            current_signers,
            next_signers,
            cardano_transactions_signing_config,
            next_cardano_transactions_signing_config,
        }
    }
}
