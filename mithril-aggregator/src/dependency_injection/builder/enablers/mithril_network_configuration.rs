use std::collections::BTreeSet;

use crate::entities::AggregatorEpochSettings;
use async_trait::async_trait;
use mithril_common::{StdResult, entities::SignedEntityTypeDiscriminants, test::double::Dummy};
use mithril_protocol_config::{
    interface::MithrilNetworkConfigurationProvider, model::MithrilNetworkConfiguration,
};

pub struct LocalMithrilNetworkConfigurationProvider {
    epoch_settings: AggregatorEpochSettings,
    allowed_discriminants: BTreeSet<SignedEntityTypeDiscriminants>,
}

impl LocalMithrilNetworkConfigurationProvider {
    pub fn new(
        epoch_settings: AggregatorEpochSettings,
        allowed_discriminants: BTreeSet<SignedEntityTypeDiscriminants>,
    ) -> Self {
        Self {
            epoch_settings,
            allowed_discriminants,
        }
    }
}

#[async_trait]
impl MithrilNetworkConfigurationProvider for LocalMithrilNetworkConfigurationProvider {
    async fn get(&self) -> StdResult<MithrilNetworkConfiguration> {
        Ok(MithrilNetworkConfiguration::dummy())
    }
}
