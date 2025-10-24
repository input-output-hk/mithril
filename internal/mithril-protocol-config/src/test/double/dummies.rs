use std::collections::BTreeSet;

use mithril_common::{
    entities::{CardanoTransactionsSigningConfig, SignedEntityTypeDiscriminants},
    test::double::{Dummy, fake_data},
};

use crate::model::{
    EpochConfiguration, MithrilNetworkConfiguration, SignedEntityTypeConfiguration,
};

impl Dummy for MithrilNetworkConfiguration {
    /// Return a dummy [MithrilNetworkConfiguration] (test-only).
    fn dummy() -> Self {
        let beacon = fake_data::beacon();

        Self {
            epoch: beacon.epoch,
            configuration_for_aggregation: EpochConfiguration::dummy(),
            configuration_for_next_aggregation: EpochConfiguration::dummy(),
            configuration_for_registration: EpochConfiguration::dummy(),
        }
    }
}

impl Dummy for EpochConfiguration {
    /// Return a dummy for [EpochConfiguration] (test-only).
    fn dummy() -> Self {
        Self {
            protocol_parameters: fake_data::protocol_parameters(),
            enabled_signed_entity_types: BTreeSet::from([
                SignedEntityTypeDiscriminants::CardanoTransactions,
            ]),
            signed_entity_types_config: SignedEntityTypeConfiguration {
                cardano_transactions: Some(CardanoTransactionsSigningConfig::dummy()),
            },
        }
    }
}
