use async_trait::async_trait;
#[cfg(test)]
use mockall::automock;
use slog_scope::info;
use std::error::Error as StdError;
use thiserror::Error;

use mithril_common::{
    crypto_helper::key_encode_hex,
    entities::{self, Beacon, CertificatePending, Signer},
    store::StakeStorer,
};

use crate::Config;

use super::signer_services::SignerServices;

#[async_trait]
pub trait Runner {
    async fn get_pending_certificate(
        &self,
    ) -> Result<Option<CertificatePending>, Box<dyn StdError + Sync + Send>>;

    async fn get_current_beacon(&self) -> Result<Beacon, Box<dyn StdError + Sync + Send>>;

    async fn register_signer_to_aggregator(&self) -> Result<(), Box<dyn StdError + Sync + Send>>;

    async fn update_stake_distribution(&self) -> Result<(), Box<dyn StdError + Sync + Send>>;
}

#[derive(Debug, Clone, PartialEq, Error)]
pub enum RuntimeError {
    #[error("No value returned by the subsystem.")]
    NoValueError,
    #[error("No stake associated with this signer.")]
    NoStake,
    #[error("Subsystem unavailable: {0}")]
    SusbsystemUnavailable(String),
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

#[cfg_attr(test, automock)]
#[async_trait]
impl Runner for SignerRunner {
    async fn get_pending_certificate(
        &self,
    ) -> Result<Option<CertificatePending>, Box<dyn StdError + Sync + Send>> {
        info!("runner: get_pending_certificate");
        self.services
            .certificate_handler
            .retrieve_pending_certificate()
            .await
            .map_err(|e| e.into())
    }

    async fn get_current_beacon(&self) -> Result<Beacon, Box<dyn StdError + Sync + Send>> {
        info!("runner: get_current_epoch");
        todo!()
        /*
        self.services
            .chain_observer
            .get_current_beacon()
            .await?
            .ok_or_else(|| RuntimeError::NoValueError.into())
        */
    }

    async fn register_signer_to_aggregator(&self) -> Result<(), Box<dyn StdError + Sync + Send>> {
        let protocol_initializer = self
            .services
            .single_signer
            .get_protocol_initializer()
            .ok_or_else(|| {
                RuntimeError::SusbsystemUnavailable(
                    "could not get protocol initializer".to_string(),
                )
            })?;
        let verification_key = key_encode_hex(protocol_initializer.verification_key())?;
        let signer = Signer::new(self.services.single_signer.get_party_id(), verification_key);
        self.services
            .certificate_handler
            .register_signer(&signer)
            .await?;

        Ok(())
    }

    async fn update_stake_distribution(&self) -> Result<(), Box<dyn StdError + Sync + Send>> {
        info!("runner: update_stake_distribution");
        let current_epoch = self
            .services
            .chain_observer
            .get_current_epoch()
            .await?
            .ok_or_else(|| RuntimeError::NoValueError)?;
        let stake_distribution = self
            .services
            .chain_observer
            .get_current_stake_distribution()
            .await?
            .ok_or_else(|| RuntimeError::NoValueError)?;
        self.services
            .stake_store
            .save_stakes(current_epoch, stake_distribution)
            .await?;

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use std::{path::PathBuf, sync::Arc};

    use mithril_common::digesters::{DumbImmutableDigester, DumbImmutableFileObserver};
    use mithril_common::store::adapter::DumbStoreAdapter;
    use mithril_common::store::{StakeStore, StakeStorer};
    use mithril_common::CardanoNetwork;
    use mithril_common::{chain_observer::FakeObserver, BeaconProviderImpl};

    use crate::{CertificateHandlerHTTPClient, DumbCertificateHandler, MithrilSingleSigner};

    use super::*;

    fn get_current_beacon() -> Beacon {
        Beacon {
            network: "whatever".to_string(),
            epoch: 9,
            immutable_file_number: 999,
        }
    }

    fn init_services() -> SignerServices {
        let chain_observer = Arc::new(FakeObserver::default());
        SignerServices {
            stake_store: Arc::new(StakeStore::new(Box::new(DumbStoreAdapter::new()))),
            certificate_handler: Arc::new(CertificateHandlerHTTPClient::new(
                "whatever".to_string(),
            )),
            chain_observer: chain_observer.clone(),
            digester: Arc::new(DumbImmutableDigester::new("whatever", true)),
            single_signer: Arc::new(MithrilSingleSigner::new("PARTY_01".to_string(), "whatever")),
            beacon_provider: Arc::new(BeaconProviderImpl::new(
                chain_observer,
                Arc::new(DumbImmutableFileObserver::default()),
                CardanoNetwork::TestNet(42),
            )),
        }
    }

    fn init_runner(
        maybe_services: Option<SignerServices>,
        maybe_config: Option<Config>,
    ) -> SignerRunner {
        let services = init_services();
        let config = Config {
            aggregator_endpoint: "http://0.0.0.0:3000".to_string(),
            cardano_cli_path: PathBuf::new(),
            cardano_node_socket_path: PathBuf::new(),
            db_directory: PathBuf::new(),
            network: "whatever".to_string(),
            network_magic: None,
            party_id: "PARTY_01".to_string(),
            run_interval: 100,
            stake_store_directory: PathBuf::new(),
        };

        SignerRunner {
            config: maybe_config.unwrap_or(config),
            services: maybe_services.unwrap_or(services),
        }
    }
    #[tokio::test]
    async fn test_get_current_beacon() {
        let runner = init_runner(None, None);
        let beacon = get_current_beacon();

        assert_eq!(
            beacon,
            runner
                .get_current_beacon()
                .await
                .expect("Get current beacon should not fail.")
        );
    }

    #[tokio::test]
    async fn test_update_stake_distribution() {
        let services = init_services();
        let stake_store = services.stake_store.clone();
        let _certificate_store = services.certificate_handler.clone();
        let current_epoch = services
            .chain_observer
            .get_current_epoch()
            .await
            .expect("chain observer should not fail")
            .expect("the observer should return an epoch");
        let runner = init_runner(Some(services), None);
        assert!(stake_store
            .get_stakes(current_epoch)
            .await
            .expect("getting stakes from store should not fail")
            .is_none());

        runner
            .update_stake_distribution()
            .await
            .expect("update_stake_distribution should not fail.");

        let stake_distribution = stake_store
            .get_stakes(current_epoch)
            .await
            .expect("getting stakes from store should not fail")
            .expect("there should be stakes for this epoch");

        assert_eq!(2, stake_distribution.len());
    }

    #[tokio::test]
    async fn test_register_signer_to_aggregator() {
        let mut services = init_services();
        let certificate_handler = Arc::new(DumbCertificateHandler::default());
        services.certificate_handler = certificate_handler.clone();
        let runner = init_runner(Some(services), None);
        runner
            .register_signer_to_aggregator()
            .await
            .expect("registering a signer to the aggregator should not fail");

        assert!(certificate_handler
            .get_last_registered_signer()
            .await
            .is_some());
    }
}
