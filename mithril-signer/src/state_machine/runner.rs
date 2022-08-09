use async_trait::async_trait;
#[cfg(test)]
use mockall::automock;
use slog_scope::debug;
use std::error::Error as StdError;
use thiserror::Error;

use mithril_common::entities::ProtocolParameters;
use mithril_common::{
    crypto_helper::key_encode_hex,
    entities::{
        Beacon, CertificatePending, Epoch, ProtocolMessage, ProtocolMessagePartKey, Signer,
        SignerWithStake, SingleSignatures,
    },
    store::StakeStorer,
};

use crate::{Config, MithrilProtocolInitializerBuilder};

use super::signer_services::SignerServices;

#[async_trait]
pub trait Runner {
    async fn get_pending_certificate(
        &self,
    ) -> Result<Option<CertificatePending>, Box<dyn StdError + Sync + Send>>;

    async fn get_current_beacon(&self) -> Result<Beacon, Box<dyn StdError + Sync + Send>>;

    async fn register_signer_to_aggregator(
        &self,
        epoch: Epoch,
        protocol_parameters: &ProtocolParameters,
    ) -> Result<(), Box<dyn StdError + Sync + Send>>;

    async fn update_stake_distribution(
        &self,
        epoch: Epoch,
    ) -> Result<(), Box<dyn StdError + Sync + Send>>;

    fn can_i_sign(&self, pending_certificate: &CertificatePending) -> bool;

    async fn associate_signers_with_stake(
        &self,
        epoch: Epoch,
        signers: &[Signer],
    ) -> Result<Vec<SignerWithStake>, Box<dyn StdError + Sync + Send>>;

    async fn compute_message(
        &self,
        certificate_pending: &CertificatePending,
    ) -> Result<ProtocolMessage, Box<dyn StdError + Sync + Send>>;

    async fn compute_single_signature(
        &self,
        message: &ProtocolMessage,
        signers: &[SignerWithStake],
        epoch: Epoch,
    ) -> Result<Option<SingleSignatures>, Box<dyn StdError + Sync + Send>>;

    async fn send_single_signature(
        &self,
        maybe_signature: Option<SingleSignatures>,
    ) -> Result<(), Box<dyn StdError + Sync + Send>>;
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
        debug!("runner: get_pending_certificate");
        self.services
            .certificate_handler
            .retrieve_pending_certificate()
            .await
            .map_err(|e| e.into())
    }

    async fn get_current_beacon(&self) -> Result<Beacon, Box<dyn StdError + Sync + Send>> {
        debug!("runner: get_current_epoch");
        self.services
            .beacon_provider
            .get_current_beacon()
            .await
            .map_err(|e| e.into())
    }

    async fn register_signer_to_aggregator(
        &self,
        epoch: Epoch,
        protocol_parameters: &ProtocolParameters,
    ) -> Result<(), Box<dyn StdError + Sync + Send>> {
        debug!("runner: register_signer_to_aggregator");
        let stake_distribution = self
            .services
            .chain_observer
            .get_current_stake_distribution()
            .await?
            .ok_or(RuntimeError::NoValueError)?;
        let stake = stake_distribution
            .get(&self.config.party_id)
            .ok_or(RuntimeError::NoStake)?;
        let protocol_initializer =
            MithrilProtocolInitializerBuilder::new(self.config.party_id.to_owned())
                .build(stake, protocol_parameters)?;
        let verification_key = key_encode_hex(protocol_initializer.verification_key())?;
        let signer = Signer::new(self.config.party_id.to_owned(), verification_key);
        self.services
            .certificate_handler
            .register_signer(&signer)
            .await?;
        self.services
            .protocol_initializer_store
            .save_protocol_initializer(epoch.offset_to_recording_epoch()?, protocol_initializer)
            .await?;

        Ok(())
    }

    async fn update_stake_distribution(
        &self,
        epoch: Epoch,
    ) -> Result<(), Box<dyn StdError + Sync + Send>> {
        debug!("runner: update_stake_distribution");
        let stake_distribution = self
            .services
            .chain_observer
            .get_current_stake_distribution()
            .await?
            .ok_or(RuntimeError::NoValueError)?;
        self.services
            .stake_store
            .save_stakes(epoch.offset_to_recording_epoch()?, stake_distribution)
            .await?;

        Ok(())
    }

    fn can_i_sign(&self, pending_certificate: &CertificatePending) -> bool {
        // todo: check that the verification key is the same as the one registered in the store
        let signer = pending_certificate.get_signer(self.config.party_id.to_owned());

        signer.is_some()
    }

    async fn associate_signers_with_stake(
        &self,
        epoch: Epoch,
        signers: &[Signer],
    ) -> Result<Vec<SignerWithStake>, Box<dyn StdError + Sync + Send>> {
        // Note: raise an error if we can't find the stake for a signer
        todo!()
    }

    async fn compute_message(
        &self,
        certificate_pending: &CertificatePending,
    ) -> Result<ProtocolMessage, Box<dyn StdError + Sync + Send>> {
        let mut message = ProtocolMessage::new();
        // 1 set the digest in the message
        let digest = self
            .services
            .digester
            .compute_digest(certificate_pending.beacon.immutable_file_number)
            .await?;
        message.set_message_part(ProtocolMessagePartKey::SnapshotDigest, digest);

        // 2 set the next signers keys and stakes in the message
        let protocol_initializer = self
            .services
            .protocol_initializer_store
            .get_protocol_initializer(certificate_pending.beacon.epoch)
            .await?
            .ok_or(RuntimeError::NoValueError)?;
        let stakes = self
            .services
            .stake_store
            .get_stakes(certificate_pending.beacon.epoch)
            .await?
            .ok_or(RuntimeError::NoStake)?;
        let mut signers: Vec<SignerWithStake> = vec![];

        // TODO: this should not be here
        for signer in &certificate_pending.next_signers[..] {
            if let Some(stake) = stakes.get(&signer.party_id) {
                signers.push(SignerWithStake::new(
                    signer.party_id.to_owned(),
                    signer.verification_key.to_owned(),
                    *stake,
                ));
            }
        }
        let avk = self
            .services
            .single_signer
            .compute_aggregate_verification_key(&signers, &protocol_initializer)?
            .ok_or(RuntimeError::NoValueError)?;
        message.set_message_part(ProtocolMessagePartKey::NextAggregateVerificationKey, avk);

        Ok(message)
    }

    async fn compute_single_signature(
        &self,
        message: &ProtocolMessage,
        signers: &[SignerWithStake],
        epoch: Epoch,
    ) -> Result<Option<SingleSignatures>, Box<dyn StdError + Sync + Send>> {
        let protocol_initializer = self
            .services
            .protocol_initializer_store
            .get_protocol_initializer(epoch)
            .await?
            .ok_or(RuntimeError::NoValueError)?;
        let signature = self.services.single_signer.compute_single_signatures(
            message,
            signers,
            &protocol_initializer,
        )?;

        Ok(signature)
    }

    async fn send_single_signature(
        &self,
        maybe_signature: Option<SingleSignatures>,
    ) -> Result<(), Box<dyn StdError + Sync + Send>> {
        if let Some(single_signatures) = maybe_signature {
            self.services
                .certificate_handler
                .register_signatures(&single_signatures)
                .await
                .map_err(|e| e.into())
        } else {
            Ok(())
        }
    }
}

#[cfg(test)]
mod tests {
    use std::{path::PathBuf, sync::Arc};

    use mithril_common::chain_observer::ChainObserver;
    use mithril_common::crypto_helper::ProtocolInitializer;
    use mithril_common::digesters::{DumbImmutableDigester, DumbImmutableFileObserver};
    use mithril_common::entities::Epoch;
    use mithril_common::store::adapter::{DumbStoreAdapter, MemoryAdapter};
    use mithril_common::store::{StakeStore, StakeStorer};
    use mithril_common::CardanoNetwork;
    use mithril_common::{chain_observer::FakeObserver, BeaconProviderImpl};

    use crate::{
        CertificateHandler, DumbCertificateHandler, MithrilSingleSigner, ProtocolInitializerStore,
    };

    use super::*;

    fn get_current_beacon() -> Beacon {
        Beacon {
            network: "whatever".to_string(),
            epoch: Epoch(9),
            immutable_file_number: 999,
        }
    }

    fn init_services() -> SignerServices {
        let adapter: MemoryAdapter<Epoch, ProtocolInitializer> = MemoryAdapter::new(None).unwrap();
        let chain_observer = Arc::new(FakeObserver::default());
        SignerServices {
            stake_store: Arc::new(StakeStore::new(Box::new(DumbStoreAdapter::new()))),
            certificate_handler: Arc::new(DumbCertificateHandler::default()),
            chain_observer: chain_observer.clone(),
            digester: Arc::new(DumbImmutableDigester::new("whatever", true)),
            single_signer: Arc::new(MithrilSingleSigner::new("1".to_string())),
            beacon_provider: Arc::new(BeaconProviderImpl::new(
                chain_observer,
                Arc::new(DumbImmutableFileObserver::default()),
                CardanoNetwork::TestNet(42),
            )),
            protocol_initializer_store: Arc::new(ProtocolInitializerStore::new(Box::new(adapter))),
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
            party_id: "1".to_string(),
            run_interval: 100,
            stake_store_directory: PathBuf::new(),
            protocol_initializer_store_directory: PathBuf::new(),
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
            .update_stake_distribution(current_epoch)
            .await
            .expect("update_stake_distribution should not fail.");

        let stake_distribution = stake_store
            .get_stakes(
                current_epoch
                    .offset_to_recording_epoch()
                    .expect("offset_to_recording_epoch should not fail"),
            )
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
        let protocol_initializer_store = services.protocol_initializer_store.clone();
        let chain_observer = Arc::new(FakeObserver::default());
        services.chain_observer = chain_observer.clone();
        let runner = init_runner(Some(services), None);
        let epoch = chain_observer
            .current_beacon
            .read()
            .await
            .clone()
            .expect("chain_observer should have a current_beacon")
            .epoch;

        let pending_certificate = certificate_handler
            .retrieve_pending_certificate()
            .await
            .expect("getting pending certificate should not fail")
            .expect("there should be a pending certificate, None returned");
        runner
            .register_signer_to_aggregator(epoch, &pending_certificate.protocol_parameters)
            .await
            .expect("registering a signer to the aggregator should not fail");

        assert!(certificate_handler
            .get_last_registered_signer()
            .await
            .is_some());
        let maybe_protocol_initializer = protocol_initializer_store
            .get_protocol_initializer(
                epoch
                    .offset_to_recording_epoch()
                    .expect("offset_to_recording_epoch should not fail"),
            )
            .await
            .expect("get_protocol_initializer should not fail");
        assert!(
            maybe_protocol_initializer.is_some(),
            "A protocol initializer should have been registered at the 'Recording' epoch"
        );
    }

    #[tokio::test]
    async fn test_compute_message() {
        let services = init_services();
        let current_epoch = services
            .chain_observer
            .get_current_epoch()
            .await
            .expect("chain observer should not fail")
            .expect("the observer should return an epoch");
        let certificate_handler = services.certificate_handler.clone();
        let runner = init_runner(Some(services), None);
        runner
            .update_stake_distribution(current_epoch)
            .await
            .expect("update_stake_distribution should not fail.");
        let certificate_pending = certificate_handler
            .retrieve_pending_certificate()
            .await
            .expect("certificate handler should not fail")
            .expect("there should be a certificate pending at this stage");
        runner
            .register_signer_to_aggregator(current_epoch, &certificate_pending.protocol_parameters)
            .await
            .expect("register signer should not fail");
        let _message = runner
            .compute_message(&certificate_pending)
            .await
            .expect("compute_message should not fail");
    }
}
