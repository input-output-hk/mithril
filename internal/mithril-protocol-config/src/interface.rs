use async_trait::async_trait;
use mithril_common::StdResult;
use mithril_common::entities::{
    CardanoTransactionsSigningConfig, Epoch, ProtocolParameters, SignedEntityTypeDiscriminants,
};
use std::collections::BTreeSet;

/// Signed entity type specific configurations
pub enum SignedEntityTypeConfiguration {
    /// Cardano Transactions
    CardanoTransactions(CardanoTransactionsSigningConfig),
}

/// A Mithril network configuration
pub struct MithrilNetworkConfiguration {
    /// Epoch
    pub epoch: Epoch,

    /// Cryptographic protocol parameters (`k`, `m` and `phi_f`)
    pub signer_registration_protocol_parameters: ProtocolParameters,

    /// List of available types of certifications (`CardanoDatabase`, `CardanoTransactions`, `CardanoStakeDistribution`, ...)
    pub available_signed_entity_types: BTreeSet<SignedEntityTypeDiscriminants>,

    /// Custom configurations for signed entity types (e.g. `cardano_transactions_signing_config` for `CardanoTransactions`)
    pub signed_entity_types_config: Vec<SignedEntityTypeConfiguration>, //or HashMap<SignedEntityTypeDiscriminant, SignedEntityTypeConfiguration>
}

/// Trait to provide the current Mithril network configuration.
#[async_trait]
pub trait MithrilNetworkConfigurationProvider: Sync + Send {
    /// Get the Mithril network configuration for the current epoch.
    async fn get(&self) -> StdResult<MithrilNetworkConfiguration>;
}
