use crate::{interface::MithrilNetworkConfigurationProvider, model::MithrilNetworkConfiguration};
use async_trait::async_trait;
use mithril_common::StdResult;

/// A fake [MithrilNetworkConfigurationProvider] that return [MithrilNetworkConfiguration]
pub struct FakeMithrilNetworkConfigurationProvider {
    configuration: MithrilNetworkConfiguration,
}

impl FakeMithrilNetworkConfigurationProvider {
    /// Create a new [FakeMithrilNetworkConfigurationProvider]
    pub fn from_mithril_network_configuration(configuration: MithrilNetworkConfiguration) -> Self {
        Self { configuration }
    }
}

impl Default for FakeMithrilNetworkConfigurationProvider {
    fn default() -> Self {
        Self {
            configuration: MithrilNetworkConfiguration {
                epoch: Default::default(),
                signer_registration_protocol_parameters: Default::default(),
                available_signed_entity_types: Default::default(),
                signed_entity_types_config: Default::default(),
            },
        }
    }
}

#[cfg_attr(target_family = "wasm", async_trait(?Send))]
#[cfg_attr(not(target_family = "wasm"), async_trait)]
impl MithrilNetworkConfigurationProvider for FakeMithrilNetworkConfigurationProvider {
    async fn get(&self) -> StdResult<MithrilNetworkConfiguration> {
        Ok(self.configuration.clone())
    }
}

#[cfg(test)]
mod tests {
    use std::collections::{BTreeSet, HashMap};

    use mithril_common::{
        StdResult,
        entities::{BlockNumber, CardanoTransactionsSigningConfig, SignedEntityTypeDiscriminants},
    };

    use super::*;
    use crate::{model::SignedEntityTypeConfiguration, test::double::fake_data};

    #[tokio::test]
    async fn fake_mithril_network_configuration_provider_returns_configuration() -> StdResult<()> {
        let mut available_signed_entity_types = BTreeSet::new();
        available_signed_entity_types.insert(SignedEntityTypeDiscriminants::CardanoTransactions);

        let mut signed_entity_types_config = HashMap::new();
        signed_entity_types_config.insert(
            SignedEntityTypeDiscriminants::CardanoTransactions,
            SignedEntityTypeConfiguration::CardanoTransactions(CardanoTransactionsSigningConfig {
                security_parameter: BlockNumber(13),
                step: BlockNumber(26),
            }),
        );

        let configuration = fake_data::mithril_network_configuration(
            42,
            1,
            2,
            0.123,
            available_signed_entity_types,
            signed_entity_types_config,
        );
        let provider = FakeMithrilNetworkConfigurationProvider::from_mithril_network_configuration(
            configuration.clone(),
        );

        let retrieved_configuration = provider.get().await?;

        assert_eq!(configuration, retrieved_configuration);
        Ok(())
    }
}
