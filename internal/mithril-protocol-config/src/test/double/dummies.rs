use std::collections::BTreeSet;

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
        let available_signed_entity_types =
            BTreeSet::from([SignedEntityTypeDiscriminants::CardanoTransactions]);
        let signed_entity_types_config = SignedEntityTypeConfiguration {
            cardano_transactions: Some(CardanoTransactionsSigningConfig::dummy()),
        };

        Self {
            epoch: beacon.epoch,
            signer_registration_protocol_parameters,
            available_signed_entity_types,
            signed_entity_types_config,
        }
    }
}
