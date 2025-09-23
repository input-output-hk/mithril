use mithril_common::StdResult;
use mithril_common::entities::{
    CardanoDbBeacon, CardanoTransactionsSigningConfig, Epoch, ProtocolParameters,
    SignedEntityTypeDiscriminants,
};
use std::collections::BTreeSet;

/// Signed entity type specific configurations
enum SignedEntityTypeConfiguration {
    /// Mithril stake distribution
    MithrilStakeDistribution(Epoch),

    /// Cardano Stake Distribution
    CardanoStakeDistribution(Epoch),

    /// Full Cardano Immutable Files
    CardanoImmutableFilesFull(CardanoDbBeacon),

    /// Cardano Database
    CardanoDatabase(CardanoDbBeacon),

    /// Cardano Transactions
    CardanoTransactions(CardanoTransactionsSigningConfig),
}

/// A Mithril network configuration
struct MithrilNetworkConfiguration {
    /// Epoch
    epoch: Epoch,

    /// Cryptographic protocol parameters (`k`, `m` and `phi_f`)
    signer_registration_protocol_parameters: ProtocolParameters,

    /// List of available types of certifications (`CardanoDatabase`, `CardanoTransactions`, `CardanoStakeDistribution`, ...)
    available_signed_entity_types: BTreeSet<SignedEntityTypeDiscriminants>,

    /// Custom configurations for signed entity types (e.g. `cardano_transactions_signing_config` for `CardanoTransactions`)
    signed_entity_types_config: Vec<SignedEntityTypeConfiguration>, //or HashMap<SignedEntityTypeDiscriminant, SignedEntityTypeConfiguration>
}

/// Trait to provide the current Mithril network configuration.
pub trait MithrilNetworkConfigurationProvider: Sync + Send {
    /// Get the Mithril network configuration for the current epoch.
    async fn get(&self) -> StdResult<MithrilNetworkConfiguration>;
}
