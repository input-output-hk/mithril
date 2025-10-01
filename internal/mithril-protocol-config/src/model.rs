use std::collections::{BTreeSet, HashMap};

use mithril_common::entities::{
    CardanoTransactionsSigningConfig, Epoch, ProtocolParameters, SignedEntityTypeDiscriminants,
};

#[derive(PartialEq, Clone, Debug)]
pub enum SignedEntityTypeConfiguration {
    /// Cardano Transactions
    CardanoTransactions(CardanoTransactionsSigningConfig),
}

/// A Mithril network configuration
#[derive(PartialEq, Clone, Debug)]
pub struct MithrilNetworkConfiguration {
    /// Epoch
    pub epoch: Epoch,

    /// Cryptographic protocol parameters (`k`, `m` and `phi_f`)
    pub signer_registration_protocol_parameters: ProtocolParameters,

    /// List of available types of certifications (`CardanoDatabase`, `CardanoTransactions`, `CardanoStakeDistribution`, ...)
    pub available_signed_entity_types: BTreeSet<SignedEntityTypeDiscriminants>,

    /// Custom configurations for signed entity types (e.g. `cardano_transactions_signing_config` for `CardanoTransactions`)
    pub signed_entity_types_config:
        HashMap<SignedEntityTypeDiscriminants, SignedEntityTypeConfiguration>,
}

impl MithrilNetworkConfiguration {
    /// Get the Cardano Transactions signing configuration
    pub fn get_cardano_transactions_signing_config(
        &self,
    ) -> Option<CardanoTransactionsSigningConfig> {
        match self
            .signed_entity_types_config
            .get(&SignedEntityTypeDiscriminants::CardanoTransactions)
        {
            Some(SignedEntityTypeConfiguration::CardanoTransactions(config)) => {
                Some(config.clone())
            }
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use mithril_common::entities::{
        BlockNumber, CardanoTransactionsSigningConfig, SignedEntityTypeDiscriminants,
    };

    use crate::model::{MithrilNetworkConfiguration, SignedEntityTypeConfiguration};

    fn default() -> MithrilNetworkConfiguration {
        MithrilNetworkConfiguration {
            epoch: Default::default(),
            signer_registration_protocol_parameters: Default::default(),
            available_signed_entity_types: Default::default(),
            signed_entity_types_config: Default::default(),
        }
    }

    #[test]
    fn test_get_cardano_transactions_signing_config_should_return_config_if_cardano_transactions_exist()
     {
        let config = MithrilNetworkConfiguration {
            signed_entity_types_config: HashMap::from([(
                SignedEntityTypeDiscriminants::CardanoTransactions,
                SignedEntityTypeConfiguration::CardanoTransactions(
                    CardanoTransactionsSigningConfig {
                        security_parameter: BlockNumber(42),
                        step: BlockNumber(26),
                    },
                ),
            )]),
            ..default()
        };

        let result = config.get_cardano_transactions_signing_config();
        assert!(result.is_some());
        let unwrapped_result = result.unwrap();
        assert_eq!(unwrapped_result.security_parameter, BlockNumber(42));
        assert_eq!(unwrapped_result.step, BlockNumber(26));
    }

    #[test]
    fn test_get_cardano_transactions_signing_config_should_return_none_if_there_is_no_cardano_transactions()
     {
        let config = MithrilNetworkConfiguration {
            signed_entity_types_config: HashMap::new(),
            ..default()
        };

        let result = config.get_cardano_transactions_signing_config();
        assert!(result.is_none());
    }
}
