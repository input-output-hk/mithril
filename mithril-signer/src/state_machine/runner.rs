use async_trait::async_trait;
use mithril_common::entities::{Beacon, CertificatePending};
use slog_scope::info;
use std::error::Error;

use crate::Config;

use super::signer_services::SignerServices;

#[async_trait]
pub trait Runner {
    async fn get_pending_certificate(
        &self,
    ) -> Result<Option<CertificatePending>, Box<dyn Error + Sync + Send>>;

    async fn get_current_beacon(&self) -> Result<Beacon, Box<dyn Error + Sync + Send>>;

    async fn register_to_aggregator(&self) -> Result<(), Box<dyn Error + Sync + Send>>;

    async fn update_stake_distribution(&self) -> Result<(), Box<dyn Error + Sync + Send>>;
}

pub struct SignerRunner {
    config: Config,
    services: SignerServices,
}

impl SignerRunner {
    pub fn new(config: Config, services: SignerServices) -> Self {
        Self { services, config }
    }
}

#[async_trait]
impl Runner for SignerRunner {
    async fn get_pending_certificate(
        &self,
    ) -> Result<Option<CertificatePending>, Box<dyn Error + Sync + Send>> {
        self.services
            .certificate_handler
            .retrieve_pending_certificate()
            .await
            .map_err(|e| e.into())
    }

    async fn get_current_beacon(&self) -> Result<Beacon, Box<dyn Error + Sync + Send>> {
        todo!()
    }

    async fn register_to_aggregator(&self) -> Result<(), Box<dyn Error + Sync + Send>> {
        todo!()
    }

    async fn update_stake_distribution(&self) -> Result<(), Box<dyn Error + Sync + Send>> {
        info!("Update stake distribution");
        if let Some(_stake_distribution) = self
            .services
            .chain_observer
            .get_current_stake_distribution()
            .await?
        {}

        /*
        {
            Some(stake_distribution) => {
                match self.chain_observer.read().await.get_current_epoch().await? {
                    Some(epoch) => {
                        let stake_store = self.stake_store.as_ref().write().await;
                        for (party_id, stake) in &stake_distribution {
                            stake_store
                                .save_stake(
                                    (epoch as i64 + SIGNER_EPOCH_RECORDING_OFFSET) as u64,
                                    SignerWithStake::new(
                                        party_id.to_owned(),
                                        "".to_string(),
                                        *stake,
                                    ),
                                )
                                .await?;
                        }
                        Ok(true)
                    }
                    None => Ok(false),
                }
            }
            None => Ok(false),
        } */
        todo!()
    }
}
