use std::collections::{BTreeSet, HashMap};

use mithril_common::{
    entities::{CardanoTransactionsSigningConfig, SignedEntityTypeDiscriminants},
    test::double::{Dummy, fake_data},
};

use crate::model::{MithrilNetworkConfiguration, SignedEntityTypeConfiguration};

impl Dummy for MithrilNetworkConfiguration {
    /// Return a dummy [MithrilNetworkConfiguration] (test-only).
    fn dummy() -> Self {
        let beacon = fake_data::beacon();
        let signer_registration_protocol_parameters = fake_data::protocol_parameters();
        let mut available_signed_entity_types = BTreeSet::new();

        available_signed_entity_types.insert(SignedEntityTypeDiscriminants::CardanoTransactions);
        let mut signed_entity_types_config = HashMap::new();
        signed_entity_types_config.insert(
            SignedEntityTypeDiscriminants::CardanoTransactions,
            SignedEntityTypeConfiguration::CardanoTransactions(
                CardanoTransactionsSigningConfig::dummy(),
            ),
        );

        Self {
            epoch: beacon.epoch,
            signer_registration_protocol_parameters,
            available_signed_entity_types,
            signed_entity_types_config,
        }
    }
}
