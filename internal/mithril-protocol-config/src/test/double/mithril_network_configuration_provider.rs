use std::{
    collections::{BTreeSet, HashMap},
    sync::Arc,
};

use tokio::sync::RwLock;

use crate::{
    interface::MithrilNetworkConfigurationProvider,
    model::{MithrilNetworkConfiguration, SignedEntityTypeConfiguration},
};
use anyhow::Error;
use async_trait::async_trait;
use mithril_common::{
    StdResult,
    entities::{ProtocolParameters, SignedEntityTypeDiscriminants, TimePoint},
};
use mithril_ticker::{MithrilTickerService, TickerService};

/// A fake [MithrilNetworkConfigurationProvider] that return [MithrilNetworkConfiguration]
pub struct FakeMithrilNetworkConfigurationProvider {
    pub signer_registration_protocol_parameters: ProtocolParameters,

    pub available_signed_entity_types: RwLock<BTreeSet<SignedEntityTypeDiscriminants>>,

    pub signed_entity_types_config:
        HashMap<SignedEntityTypeDiscriminants, SignedEntityTypeConfiguration>,

    ticker_service: Arc<MithrilTickerService>,
}

impl FakeMithrilNetworkConfigurationProvider {
    pub fn new(
        signer_registration_protocol_parameters: ProtocolParameters,
        available_signed_entity_types: BTreeSet<SignedEntityTypeDiscriminants>,
        signed_entity_types_config: HashMap<
            SignedEntityTypeDiscriminants,
            SignedEntityTypeConfiguration,
        >,
        ticker_service: Arc<MithrilTickerService>,
    ) -> Self {
        Self {
            signer_registration_protocol_parameters,
            available_signed_entity_types: RwLock::new(available_signed_entity_types),
            signed_entity_types_config,
            ticker_service,
        }
    }

    async fn get_time_point(&self) -> Result<TimePoint, Error> {
        let time_point = self.ticker_service.get_current_time_point().await?;

        Ok(time_point)
    }

    pub async fn change_allowed_discriminants(
        &self,
        discriminants: &BTreeSet<SignedEntityTypeDiscriminants>,
    ) {
        let mut available_signed_entity_types = self.available_signed_entity_types.write().await;
        *available_signed_entity_types = discriminants.clone();
    }
}

#[cfg_attr(target_family = "wasm", async_trait(?Send))]
#[cfg_attr(not(target_family = "wasm"), async_trait)]
impl MithrilNetworkConfigurationProvider for FakeMithrilNetworkConfigurationProvider {
    async fn get(&self) -> StdResult<MithrilNetworkConfiguration> {
        let time_point = self.get_time_point().await?;

        let available_signed_entity_types = self.available_signed_entity_types.read().await;

        Ok(MithrilNetworkConfiguration {
            epoch: time_point.epoch,
            signer_registration_protocol_parameters: self
                .signer_registration_protocol_parameters
                .clone(),
            available_signed_entity_types: available_signed_entity_types.clone(),
            signed_entity_types_config: self.signed_entity_types_config.clone(),
        })
    }
}
