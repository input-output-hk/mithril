use async_trait::async_trait;
#[cfg(test)]
use mockall::automock;
use slog_scope::debug;
use std::error::Error as StdError;
use thiserror::Error;

use mithril_common::crypto_helper::{key_decode_hex, ProtocolSignerVerificationKey};
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

    async fn can_i_sign(
        &self,
        pending_certificate: &CertificatePending,
    ) -> Result<bool, Box<dyn StdError + Sync + Send>>;

    async fn associate_signers_with_stake(
        &self,
        epoch: Epoch,
        signers: &[Signer],
    ) -> Result<Vec<SignerWithStake>, Box<dyn StdError + Sync + Send>>;

    async fn compute_message(
        &self,
        beacon: &Beacon,
        next_signers: &[SignerWithStake],
    ) -> Result<ProtocolMessage, Box<dyn StdError + Sync + Send>>;

    async fn compute_single_signature(
        &self,
        epoch: Epoch,
        message: &ProtocolMessage,
        signers: &[SignerWithStake],
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

    async fn can_i_sign(
        &self,
        pending_certificate: &CertificatePending,
    ) -> Result<bool, Box<dyn StdError + Sync + Send>> {
        if let Some(signer) = pending_certificate.get_signer(self.config.party_id.to_owned()) {
            let protocol_initializer = self
                .services
                .protocol_initializer_store
                .get_protocol_initializer(
                    pending_certificate
                        .beacon
                        .epoch
                        .offset_to_signer_retrieval_epoch()?,
                )
                .await?;
            let recorded_verification_key =
                key_decode_hex::<ProtocolSignerVerificationKey>(&signer.verification_key)?;

            Ok(protocol_initializer.is_some()
                && recorded_verification_key == protocol_initializer.unwrap().verification_key())
        } else {
            Ok(false)
        }
    }

    async fn associate_signers_with_stake(
        &self,
        epoch: Epoch,
        signers: &[Signer],
    ) -> Result<Vec<SignerWithStake>, Box<dyn StdError + Sync + Send>> {
        // todo: dedicated error
        let stakes = self
            .services
            .stake_store
            .get_stakes(epoch)
            .await?
            .ok_or(RuntimeError::NoValueError)?;
        let mut signers_with_stake = vec![];

        for signer in signers {
            // todo: dedicated error
            let stake = stakes.get(&*signer.party_id).ok_or(RuntimeError::NoStake)?;

            signers_with_stake.push(SignerWithStake::new(
                signer.party_id.to_owned(),
                signer.verification_key.to_owned(),
                *stake,
            ));
        }
        Ok(signers_with_stake)
    }

    async fn compute_message(
        &self,
        beacon: &Beacon,
        next_signers: &[SignerWithStake],
    ) -> Result<ProtocolMessage, Box<dyn StdError + Sync + Send>> {
        let mut message = ProtocolMessage::new();
        // 1 set the digest in the message
        let digest = self
            .services
            .digester
            .compute_digest(beacon.immutable_file_number)
            .await?;
        message.set_message_part(ProtocolMessagePartKey::SnapshotDigest, digest);

        // 2 set the next signers keys and stakes in the message
        let protocol_initializer = self
            .services
            .protocol_initializer_store
            .get_protocol_initializer(beacon.epoch.offset_to_signer_retrieval_epoch()?)
            .await?
            .ok_or(RuntimeError::NoValueError)?;

        let avk = self
            .services
            .single_signer
            .compute_aggregate_verification_key(next_signers, &protocol_initializer)?
            .ok_or(RuntimeError::NoValueError)?;
        message.set_message_part(ProtocolMessagePartKey::NextAggregateVerificationKey, avk);

        Ok(message)
    }

    async fn compute_single_signature(
        &self,
        epoch: Epoch,
        message: &ProtocolMessage,
        signers: &[SignerWithStake],
    ) -> Result<Option<SingleSignatures>, Box<dyn StdError + Sync + Send>> {
        let protocol_initializer = self
            .services
            .protocol_initializer_store
            .get_protocol_initializer(epoch.offset_to_signer_retrieval_epoch()?)
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
    use mockall::mock;
    use std::{path::PathBuf, sync::Arc};

    use mithril_common::crypto_helper::tests_setup::setup_signers;
    use mithril_common::crypto_helper::ProtocolInitializer;
    use mithril_common::digesters::{DumbImmutableDigester, DumbImmutableFileObserver};
    use mithril_common::entities::{Epoch, StakeDistribution};
    use mithril_common::store::adapter::{DumbStoreAdapter, MemoryAdapter};
    use mithril_common::store::{StakeStore, StakeStorer};
    use mithril_common::{
        chain_observer::FakeObserver, BeaconProvider, BeaconProviderError, BeaconProviderImpl,
    };
    use mithril_common::{fake_data, CardanoNetwork};

    use crate::{
        CertificateHandler, DumbCertificateHandler, MithrilSingleSigner, MockCertificateHandler,
        ProtocolInitializerStore, SingleSigner,
    };

    use super::*;

    const DIGESTER_RESULT: &str = "a digest";

    mock! {
        pub FakeBeaconProvider { }

        #[async_trait]
        impl BeaconProvider for FakeBeaconProvider {
            async fn get_current_beacon(&self) -> Result<Beacon, BeaconProviderError>;
        }
    }

    fn init_services() -> SignerServices {
        let adapter: MemoryAdapter<Epoch, ProtocolInitializer> = MemoryAdapter::new(None).unwrap();
        let chain_observer = Arc::new(FakeObserver::default());
        SignerServices {
            stake_store: Arc::new(StakeStore::new(Box::new(DumbStoreAdapter::new()))),
            certificate_handler: Arc::new(DumbCertificateHandler::default()),
            chain_observer: chain_observer.clone(),
            digester: Arc::new(DumbImmutableDigester::new(DIGESTER_RESULT, true)),
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
        let mut services = init_services();
        let expected = fake_data::beacon();
        let mut beacon_provider = MockFakeBeaconProvider::new();
        beacon_provider
            .expect_get_current_beacon()
            .once()
            .returning(|| Ok(fake_data::beacon()));
        services.beacon_provider = Arc::new(beacon_provider);
        let runner = init_runner(Some(services), None);

        assert_eq!(
            expected,
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
    async fn test_can_i_sign() {
        let services = init_services();
        let protocol_initializer_store = services.protocol_initializer_store.clone();
        let runner = init_runner(Some(services), None);
        let mut pending_certificate = fake_data::certificate_pending();
        let epoch = pending_certificate.beacon.epoch;
        let mut signer = &mut pending_certificate.signers[0];

        let protocol_initializer = MithrilProtocolInitializerBuilder::new(signer.party_id.clone())
            .build(&100, &fake_data::protocol_parameters())
            .expect("build protocol initializer should not fail");
        signer.verification_key = key_encode_hex(protocol_initializer.verification_key()).unwrap();
        protocol_initializer_store
            .save_protocol_initializer(
                epoch
                    .offset_to_signer_retrieval_epoch()
                    .expect("offset_to_signer_retrieval_epoch should not fail"),
                protocol_initializer,
            )
            .await
            .expect("save_protocol_initializer should not fail");

        let can_i_sign_result = runner.can_i_sign(&pending_certificate).await.unwrap();
        assert!(can_i_sign_result);
    }

    #[tokio::test]
    async fn test_associate_signers_with_stake() {
        let services = init_services();
        let stake_store = services.stake_store.clone();
        let runner = init_runner(Some(services), None);
        let epoch = Epoch(12);
        let expected = fake_data::signers_with_stakes(5);
        let signers = expected
            .clone()
            .into_iter()
            .map(|s| s.into())
            .collect::<Vec<_>>();
        let stake_distribution = expected
            .clone()
            .into_iter()
            .map(|s| s.into())
            .collect::<StakeDistribution>();

        stake_store
            .save_stakes(epoch, stake_distribution)
            .await
            .expect("save_stakes should not fail");

        let result = runner
            .associate_signers_with_stake(epoch, &signers)
            .await
            .expect("associate_signers_with_stake should not fail");
        assert_eq!(expected, result);
    }

    #[tokio::test]
    async fn test_compute_message() {
        let mut services = init_services();
        let current_beacon = services
            .beacon_provider
            .get_current_beacon()
            .await
            .expect("get_current_beacon should not fail");
        let signers = setup_signers(5);
        let (party_id, _, _, _, protocol_initializer) = signers.first().unwrap();
        let single_signer = Arc::new(MithrilSingleSigner::new(party_id.to_string()));
        services.single_signer = single_signer.clone();
        services
            .protocol_initializer_store
            .save_protocol_initializer(
                current_beacon
                    .epoch
                    .offset_to_signer_retrieval_epoch()
                    .expect("offset_to_signer_retrieval_epoch should not fail"),
                protocol_initializer.clone(),
            )
            .await
            .expect("save_protocol_initializer should not fail");

        let next_signers = signers
            .iter()
            .map(|(p, s, vk, _, _)| {
                SignerWithStake::new(p.to_string(), key_encode_hex(vk).unwrap(), *s)
            })
            .collect::<Vec<_>>();
        let mut expected = ProtocolMessage::new();
        expected.set_message_part(
            ProtocolMessagePartKey::SnapshotDigest,
            DIGESTER_RESULT.to_string(),
        );
        let avk = services
            .single_signer
            .compute_aggregate_verification_key(&next_signers, protocol_initializer)
            .expect("compute_aggregate_verification_key should not fail")
            .expect("an avk should have been computed");
        expected.set_message_part(ProtocolMessagePartKey::NextAggregateVerificationKey, avk);

        let runner = init_runner(Some(services), None);
        let message = runner
            .compute_message(&current_beacon, &next_signers)
            .await
            .expect("compute_message should not fail");

        assert_eq!(expected, message);
    }

    #[tokio::test]
    async fn test_compute_single_signature() {
        let mut services = init_services();
        let current_beacon = services
            .beacon_provider
            .get_current_beacon()
            .await
            .expect("get_current_beacon should not fail");
        let signers = setup_signers(5);
        let (party_id, _, _, _, protocol_initializer) = signers.first().unwrap();
        let single_signer = Arc::new(MithrilSingleSigner::new(party_id.to_string()));
        services.single_signer = single_signer.clone();
        services
            .protocol_initializer_store
            .save_protocol_initializer(
                current_beacon
                    .epoch
                    .offset_to_signer_retrieval_epoch()
                    .expect("offset_to_signer_retrieval_epoch should not fail"),
                protocol_initializer.clone(),
            )
            .await
            .expect("save_protocol_initializer should not fail");
        let signers = signers
            .iter()
            .map(|(p, s, vk, _, _)| {
                SignerWithStake::new(p.to_string(), key_encode_hex(vk).unwrap(), *s)
            })
            .collect::<Vec<_>>();

        let mut message = ProtocolMessage::new();
        message.set_message_part(
            ProtocolMessagePartKey::SnapshotDigest,
            "a message".to_string(),
        );
        message.set_message_part(
            ProtocolMessagePartKey::NextAggregateVerificationKey,
            "an avk".to_string(),
        );

        let expected = single_signer
            .compute_single_signatures(&message, &signers, protocol_initializer)
            .expect("compute_single_signatures should not fail");

        let runner = init_runner(Some(services), None);
        let single_signature = runner
            .compute_single_signature(current_beacon.epoch, &message, &signers)
            .await
            .expect("compute_message should not fail");
        assert_eq!(expected, single_signature);
    }

    #[tokio::test]
    async fn test_send_single_signature() {
        let mut services = init_services();
        let mut certificate_handler = MockCertificateHandler::new();
        certificate_handler
            .expect_register_signatures()
            .once()
            .returning(|_| Ok(()));
        services.certificate_handler = Arc::new(certificate_handler);
        let runner = init_runner(Some(services), None);

        runner
            .send_single_signature(Some(fake_data::single_signatures(vec![2, 5, 12])))
            .await
            .expect("send_single_signature should not fail");
    }
}
