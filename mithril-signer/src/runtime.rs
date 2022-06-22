use slog_scope::{error, info, warn};
use std::collections::HashMap;
use std::sync::Arc;
use std::time::Duration;
use thiserror::Error;
use tokio::sync::RwLock;
use tokio::time::sleep;

use mithril_common::chain_observer::{ChainObserver, ChainObserverError};
use mithril_common::crypto_helper::{key_encode_hex, Bytes};
use mithril_common::digesters::{Digester, DigesterError};
use mithril_common::entities::{self, Beacon, CertificatePending, Epoch, PartyId, SignerWithStake};
use mithril_common::store::stake_store::{StakeStore, StakeStoreError, StakeStorer};

use super::certificate_handler::CertificateHandler;
use super::single_signer::SingleSigner;
use crate::certificate_handler::CertificateHandlerError;
use crate::single_signer::SingleSignerError;

///  StakeStoreWrapper wraps a StakeStore
pub type StakeStoreWrapper = Arc<RwLock<StakeStore>>;

///  ChainObserverWrapper wraps a ChainObserver
pub type ChainObserverWrapper = Arc<RwLock<dyn ChainObserver>>;

pub struct Runtime {
    /// Certificate handler takes care of communicating with an aggregator
    certificate_handler: Box<dyn CertificateHandler>,

    /// Single signer is in charge of Mithril cryptographic operations
    single_signer: Box<dyn SingleSigner>,

    /// Digester takes care of computing the snapshots digests
    digester: Box<dyn Digester>,

    /// Local epoch represents the epoch computed locally to the signer
    local_epoch: Option<Epoch>,

    /// Current beacon represents the beacon received from the aggregator for which to produce single signatures
    current_beacon: Option<Beacon>,

    /// Stake store stores the stake distribution
    stake_store: StakeStoreWrapper,

    /// Chain observer observes the Cardano chain and helps retrieving epoch and stake distribution
    chain_observer: ChainObserverWrapper,
}

#[derive(Error, Debug)]
pub enum RuntimeError {
    #[error("single signatures computation failed: `{0}`")]
    SingleSignaturesComputeFailed(#[from] SingleSignerError),

    #[error("could not retrieve pending certificate: `{0}`")]
    RetrievePendingCertificateFailed(#[from] CertificateHandlerError),

    #[error("could not retrieve protocol initializer")]
    RetrieveProtocolInitializerFailed(),

    #[error("register signer failed: `{0}`")]
    RegisterSignerFailed(String),

    #[error("codec error:`{0}`")]
    Codec(String),

    #[error("digest computation failed: `{0}`")]
    Digester(#[from] DigesterError),

    #[error("stake store error: '{0}'")]
    StakeStore(#[from] StakeStoreError),

    #[error("no stakes available")]
    UnavailableStakes(),

    #[error("chain observer failed: `{0}`")]
    ChainObserver(#[from] ChainObserverError),
}

impl Runtime {
    pub fn new(
        certificate_handler: Box<dyn CertificateHandler>,
        single_signer: Box<dyn SingleSigner>,
        digester: Box<dyn Digester>,
        stake_store: StakeStoreWrapper,
        chain_observer: ChainObserverWrapper,
    ) -> Self {
        Self {
            certificate_handler,
            single_signer,
            digester,
            local_epoch: None,
            current_beacon: None,
            stake_store,
            chain_observer,
        }
    }

    pub async fn infinite_loop(&mut self, loop_interval: u64) {
        loop {
            if let Err(e) = self.run().await {
                error!("{:?}", e)
            }

            info!("Sleeping for {}", loop_interval);
            sleep(Duration::from_millis(loop_interval)).await;
        }
    }

    pub async fn run(&mut self) -> Result<(), RuntimeError> {
        if let Some(pending_certificate) = self
            .certificate_handler
            .retrieve_pending_certificate()
            .await?
        {
            if let Some(epoch) = self.is_new_epoch().await? {
                let updated_stake_distribution = self.update_stake_distribution().await?;
                let registered_signer = self.register_signer().await?;
                if updated_stake_distribution && registered_signer {
                    self.local_epoch = Some(epoch);
                }
            }

            let beacon = &pending_certificate.clone().beacon;
            if self.is_new_beacon(beacon) {
                let message = self.digester.compute_digest()?;
                info!("Signing digest"; "digester_result" => #?message);
                self.register_signatures(message.digest.into_bytes(), pending_certificate)
                    .await?;
                self.current_beacon = Some(beacon.to_owned());
            }
        }
        Ok(())
    }

    fn is_new_beacon(&self, new_beacon: &Beacon) -> bool {
        match &self.current_beacon {
            None => {
                info!("Unknown beacon, signatures will be registered...");
                true
            }
            Some(beacon) => {
                if beacon != new_beacon {
                    info!("The beacon changed, signatures will be registered...");
                    true
                } else {
                    info!("Signatures already registered for this beacon");
                    false
                }
            }
        }
    }

    async fn is_new_epoch(&self) -> Result<Option<Epoch>, RuntimeError> {
        match self.chain_observer.read().await.get_current_epoch().await? {
            Some(epoch) => match self.local_epoch {
                Some(local_epoch) => {
                    if local_epoch != epoch {
                        info!("The epoch has changed"; "epoch" => epoch);
                        Ok(Some(epoch))
                    } else {
                        info!("The epoch is the same"; "epoch" => epoch);
                        Ok(None)
                    }
                }
                None => {
                    info!("The epoch has changed"; "epoch" => epoch);
                    Ok(Some(epoch))
                }
            },
            None => {
                info!("Unknown epoch");
                Ok(None)
            }
        }
    }

    async fn register_signer(&mut self) -> Result<bool, RuntimeError> {
        info!("Register signer");
        if let Some(protocol_initializer) = self.single_signer.get_protocol_initializer() {
            let verification_key = protocol_initializer.verification_key();
            let verification_key = key_encode_hex(verification_key).map_err(RuntimeError::Codec)?;
            let signer = entities::Signer::new(self.single_signer.get_party_id(), verification_key);
            self.certificate_handler
                .register_signer(&signer)
                .await
                .map_err(|e| RuntimeError::RegisterSignerFailed(e.to_string()))?;
            Ok(true)
        } else {
            Ok(false)
        }
    }

    async fn register_signatures(
        &mut self,
        message: Bytes,
        pending_certificate: CertificatePending,
    ) -> Result<(), RuntimeError> {
        info!("Register signatures");
        let verification_keys = pending_certificate
            .signers
            .iter()
            .map(|signer| (signer.party_id.to_owned(), signer.verification_key.as_str()))
            .collect::<HashMap<PartyId, &str>>();
        #[allow(clippy::identity_op)]
        let epoch = pending_certificate.beacon.epoch - 0; // TODO: Should be -1 or -2
        warn!(
            "Epoch computation is not final and needs to be fixed: {}",
            epoch
        );
        let stake_store = self.stake_store.read().await;
        let stake_distribution = stake_store
            .get_stakes(epoch)
            .await?
            .ok_or_else(RuntimeError::UnavailableStakes)?;

        let stake_distribution_extended = stake_distribution
            .into_iter()
            .map(|(_, signer)| {
                let verification_key = match verification_keys.get(&signer.party_id) {
                    Some(verification_key_found) => *verification_key_found,
                    None => "",
                };
                SignerWithStake::new(signer.party_id, verification_key.to_string(), signer.stake)
            })
            .collect::<Vec<SignerWithStake>>();

        let signatures = self.single_signer.compute_single_signatures(
            message,
            stake_distribution_extended,
            &pending_certificate.protocol_parameters,
        )?;

        if !signatures.is_empty() {
            let _ = self
                .certificate_handler
                .register_signatures(&signatures)
                .await;
        }

        Ok(())
    }

    async fn update_stake_distribution(&self) -> Result<bool, RuntimeError> {
        info!("Update stake distribution");
        match self
            .chain_observer
            .read()
            .await
            .get_current_stake_distribution()
            .await?
        {
            Some(stake_distribution) => {
                match self.chain_observer.read().await.get_current_epoch().await? {
                    Some(epoch) => {
                        let mut stake_store = self.stake_store.as_ref().write().await;
                        for (party_id, stake) in &stake_distribution {
                            stake_store
                                .save_stake(
                                    epoch,
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
        }
    }
}

#[cfg(test)]
mod tests {
    use super::super::certificate_handler::{CertificateHandlerError, MockCertificateHandler};
    use super::super::single_signer::{MockSingleSigner, SingleSignerError};
    use super::*;

    use mithril_common::crypto_helper::tests_setup::*;
    use mithril_common::crypto_helper::ProtocolStakeDistribution;
    use mithril_common::digesters::{Digester, DigesterError, DigesterResult};
    use mithril_common::entities::{Epoch, StakeDistribution};
    use mithril_common::fake_data;
    use mithril_common::store::adapter::MemoryAdapter;

    use async_trait::async_trait;
    use mockall::mock;

    mock! {
        pub DigesterImpl { }
        impl Digester for DigesterImpl {
            fn compute_digest(&self) -> Result<DigesterResult, DigesterError>;
        }
    }

    mock! {
        pub ChainObserverImpl { }

        #[async_trait]
        impl ChainObserver for ChainObserverImpl {
            async fn get_current_epoch(&self) -> Result<Option<Epoch>, ChainObserverError>;

            async fn get_current_stake_distribution(
                &self,
            ) -> Result<Option<StakeDistribution>, ChainObserverError>;
        }
    }

    async fn setup_stake_store(total_signers: u64) -> StakeStore {
        let mut stake_store = StakeStore::new(Box::new(
            MemoryAdapter::<Epoch, HashMap<PartyId, entities::SignerWithStake>>::new(None).unwrap(),
        ));
        let stakes: ProtocolStakeDistribution = fake_data::signers_with_stakes(total_signers)
            .into_iter()
            .map(|signer| signer.into())
            .collect::<_>();
        let current_epoch = fake_data::beacon().epoch;
        for epoch in current_epoch - 2..current_epoch + 2 {
            for stake in &stakes {
                stake_store
                    .save_stake(epoch, stake.to_owned().into())
                    .await
                    .expect("fake stake distribution update failed");
            }
        }
        stake_store
    }

    #[tokio::test]
    async fn signer_updates_stake_distribution_when_it_should() {
        let total_signers = 5;
        let epoch = 123;
        let stake_distribution_expected = StakeDistribution::from([
            ("0".to_string(), 10),
            ("1".to_string(), 20),
            ("2".to_string(), 30),
        ]);
        let stake_distribution_expected_length = stake_distribution_expected.len();
        let current_signer = &setup_signers(1)[0];
        let party_id = current_signer.clone().0;
        let protocol_initializer = current_signer.4.clone();
        let pending_certificate = fake_data::certificate_pending();
        let stake_store = setup_stake_store(total_signers).await;
        let mut mock_certificate_handler = MockCertificateHandler::new();
        let mut mock_single_signer = MockSingleSigner::new();
        let mut mock_digester = MockDigesterImpl::new();
        let mut mock_chain_observer = MockChainObserverImpl::new();
        mock_certificate_handler
            .expect_retrieve_pending_certificate()
            .return_once(|| Ok(Some(pending_certificate)));
        mock_certificate_handler
            .expect_register_signer()
            .return_once(|_| Ok(()));
        mock_certificate_handler
            .expect_register_signatures()
            .return_once(|_| Ok(()));
        mock_single_signer
            .expect_compute_single_signatures()
            .return_once(|_, _, _| Ok(fake_data::single_signatures(2)))
            .once();
        mock_single_signer
            .expect_get_party_id()
            .return_once(move || party_id);
        mock_single_signer
            .expect_get_protocol_initializer()
            .return_once(move || Some(protocol_initializer));
        mock_digester
            .expect_compute_digest()
            .return_once(|| Ok(fake_data::digester_result("digest")));
        mock_chain_observer
            .expect_get_current_epoch()
            .returning(move || Ok(Some(epoch)))
            .times(2);
        mock_chain_observer
            .expect_get_current_stake_distribution()
            .return_once(move || Ok(Some(stake_distribution_expected.clone())));

        let mut signer = Runtime::new(
            Box::new(mock_certificate_handler),
            Box::new(mock_single_signer),
            Box::new(mock_digester),
            Arc::new(RwLock::new(stake_store)),
            Arc::new(RwLock::new(mock_chain_observer)),
        );
        assert!(signer.run().await.is_ok());
        assert_eq!(
            stake_distribution_expected_length,
            signer
                .stake_store
                .read()
                .await
                .get_stakes(epoch)
                .await
                .unwrap()
                .unwrap()
                .len()
        );
    }

    #[tokio::test]
    async fn signer_doesnt_sign_when_there_is_no_pending_certificate() {
        let current_signer = &setup_signers(1)[0];
        let party_id = current_signer.clone().0;
        let protocol_initializer = current_signer.4.clone();
        let stake_store = setup_stake_store(5).await;
        let mut mock_certificate_handler = MockCertificateHandler::new();
        let mut mock_single_signer = MockSingleSigner::new();
        let mock_digester = MockDigesterImpl::new();
        let mut mock_chain_observer = MockChainObserverImpl::new();
        mock_certificate_handler
            .expect_retrieve_pending_certificate()
            .return_once(|| Ok(None));
        mock_certificate_handler
            .expect_register_signer()
            .return_once(|_| Ok(()));
        mock_single_signer
            .expect_compute_single_signatures()
            .never();
        mock_single_signer
            .expect_get_party_id()
            .return_once(move || party_id);
        mock_single_signer
            .expect_get_protocol_initializer()
            .return_once(move || Some(protocol_initializer));
        mock_chain_observer
            .expect_get_current_epoch()
            .return_once(|| Ok(Some(1)));
        mock_chain_observer
            .expect_get_current_stake_distribution()
            .return_once(|| Ok(Some(StakeDistribution::default())));

        let mut signer = Runtime::new(
            Box::new(mock_certificate_handler),
            Box::new(mock_single_signer),
            Box::new(mock_digester),
            Arc::new(RwLock::new(stake_store)),
            Arc::new(RwLock::new(mock_chain_observer)),
        );
        assert!(signer.run().await.is_ok());
    }

    #[tokio::test]
    async fn signer_fails_when_pending_certificate_fails() {
        let stake_store = setup_stake_store(5).await;
        let mut mock_certificate_handler = MockCertificateHandler::new();
        let mut mock_single_signer = MockSingleSigner::new();
        let mock_digester = MockDigesterImpl::new();
        let mut mock_chain_observer = MockChainObserverImpl::new();
        mock_certificate_handler
            .expect_retrieve_pending_certificate()
            .return_once(|| {
                Err(CertificateHandlerError::RemoteServerTechnical(
                    "An Error".to_string(),
                ))
            });
        mock_single_signer
            .expect_get_protocol_initializer()
            .return_once(move || None);
        mock_chain_observer
            .expect_get_current_epoch()
            .return_once(|| Ok(Some(1)));
        mock_chain_observer
            .expect_get_current_stake_distribution()
            .return_once(|| Ok(Some(StakeDistribution::default())));

        let mut signer = Runtime::new(
            Box::new(mock_certificate_handler),
            Box::new(mock_single_signer),
            Box::new(mock_digester),
            Arc::new(RwLock::new(stake_store)),
            Arc::new(RwLock::new(mock_chain_observer)),
        );
        assert_eq!(
            RuntimeError::RetrievePendingCertificateFailed(
                CertificateHandlerError::RemoteServerTechnical("An Error".to_string())
            )
            .to_string(),
            signer.run().await.unwrap_err().to_string()
        );
    }

    #[tokio::test]
    async fn signer_sign_when_triggered_by_pending_certificate() {
        let current_signer = &setup_signers(1)[0];
        let party_id = current_signer.clone().0;
        let protocol_initializer = current_signer.4.clone();
        let stake_store = setup_stake_store(5).await;
        let mut mock_certificate_handler = MockCertificateHandler::new();
        let mut mock_single_signer = MockSingleSigner::new();
        let mut mock_digester = MockDigesterImpl::new();
        let mut mock_chain_observer = MockChainObserverImpl::new();
        let pending_certificate = fake_data::certificate_pending();
        mock_certificate_handler
            .expect_retrieve_pending_certificate()
            .returning(|| Ok(None))
            .once();
        mock_certificate_handler
            .expect_retrieve_pending_certificate()
            .return_once(|| Ok(Some(pending_certificate)));
        mock_certificate_handler
            .expect_register_signer()
            .returning(|_| Ok(()))
            .once();
        mock_certificate_handler
            .expect_register_signatures()
            .return_once(|_| Ok(()));
        mock_single_signer
            .expect_compute_single_signatures()
            .return_once(|_, _, _| Ok(fake_data::single_signatures(2)));
        mock_single_signer
            .expect_get_party_id()
            .returning(move || party_id.to_owned());
        mock_single_signer
            .expect_get_protocol_initializer()
            .returning(move || Some(protocol_initializer.clone()));
        mock_digester
            .expect_compute_digest()
            .return_once(|| Ok(fake_data::digester_result("digest")));
        mock_chain_observer
            .expect_get_current_epoch()
            .returning(move || Ok(Some(1)))
            .once();
        mock_chain_observer
            .expect_get_current_epoch()
            .returning(move || Ok(None))
            .once();
        mock_chain_observer
            .expect_get_current_stake_distribution()
            .returning(|| Ok(Some(StakeDistribution::default())))
            .once();
        mock_chain_observer
            .expect_get_current_stake_distribution()
            .return_once(|| Ok(None));

        let mut signer = Runtime::new(
            Box::new(mock_certificate_handler),
            Box::new(mock_single_signer),
            Box::new(mock_digester),
            Arc::new(RwLock::new(stake_store)),
            Arc::new(RwLock::new(mock_chain_observer)),
        );
        assert!(signer.run().await.is_ok());
        assert!(signer.run().await.is_ok());
    }

    #[tokio::test]
    async fn signer_sign_only_once_if_pending_certificate_has_not_changed() {
        let current_signer = &setup_signers(1)[0];
        let party_id = current_signer.clone().0;
        let protocol_initializer = current_signer.4.clone();
        let stake_store = setup_stake_store(5).await;
        let mut mock_certificate_handler = MockCertificateHandler::new();
        let mut mock_single_signer = MockSingleSigner::new();
        let mut mock_digester = MockDigesterImpl::new();
        let mut mock_chain_observer = MockChainObserverImpl::new();
        let pending_certificate = fake_data::certificate_pending();
        mock_certificate_handler
            .expect_retrieve_pending_certificate()
            .returning(move || Ok(Some(pending_certificate.clone())))
            .times(2);
        mock_certificate_handler
            .expect_register_signatures()
            .return_once(|_| Ok(()));
        mock_certificate_handler
            .expect_register_signer()
            .returning(|_| Ok(()))
            .times(1);
        mock_single_signer
            .expect_compute_single_signatures()
            .return_once(|_, _, _| Ok(fake_data::single_signatures(2)));
        mock_single_signer
            .expect_get_party_id()
            .returning(move || party_id.to_owned())
            .once();
        mock_single_signer
            .expect_get_protocol_initializer()
            .return_once(move || Some(protocol_initializer.clone()));
        mock_digester
            .expect_compute_digest()
            .return_once(|| Ok(fake_data::digester_result("digest")));
        mock_chain_observer
            .expect_get_current_epoch()
            .returning(|| Ok(Some(1)))
            .times(3);
        mock_chain_observer
            .expect_get_current_stake_distribution()
            .returning(|| Ok(Some(StakeDistribution::default())))
            .once();
        mock_chain_observer
            .expect_get_current_stake_distribution()
            .return_once(|| Ok(None));

        let mut signer = Runtime::new(
            Box::new(mock_certificate_handler),
            Box::new(mock_single_signer),
            Box::new(mock_digester),
            Arc::new(RwLock::new(stake_store)),
            Arc::new(RwLock::new(mock_chain_observer)),
        );
        assert!(signer.run().await.is_ok());
        assert!(signer.run().await.is_ok());
    }

    #[tokio::test]
    async fn signer_does_not_send_signatures_if_none_are_computed() {
        let current_signer = &setup_signers(1)[0];
        let party_id = current_signer.clone().0;
        let protocol_initializer = current_signer.4.clone();
        let stake_store = setup_stake_store(5).await;
        let mut mock_certificate_handler = MockCertificateHandler::new();
        let mut mock_single_signer = MockSingleSigner::new();
        let mut mock_digester = MockDigesterImpl::new();
        let mut mock_chain_observer = MockChainObserverImpl::new();
        let pending_certificate = fake_data::certificate_pending();
        mock_certificate_handler
            .expect_retrieve_pending_certificate()
            .return_once(|| Ok(Some(pending_certificate)));
        mock_certificate_handler
            .expect_register_signatures()
            .never();
        mock_certificate_handler
            .expect_register_signer()
            .return_once(|_| Ok(()));
        mock_single_signer
            .expect_compute_single_signatures()
            .return_once(|_, _, _| Ok(fake_data::single_signatures(0)));
        mock_single_signer
            .expect_get_party_id()
            .return_once(move || party_id);
        mock_single_signer
            .expect_get_protocol_initializer()
            .return_once(move || Some(protocol_initializer));
        mock_digester
            .expect_compute_digest()
            .return_once(|| Ok(fake_data::digester_result("digest")));
        mock_chain_observer
            .expect_get_current_epoch()
            .returning(move || Ok(Some(1)))
            .times(2);
        mock_chain_observer
            .expect_get_current_stake_distribution()
            .return_once(|| Ok(Some(StakeDistribution::default())));

        let mut signer = Runtime::new(
            Box::new(mock_certificate_handler),
            Box::new(mock_single_signer),
            Box::new(mock_digester),
            Arc::new(RwLock::new(stake_store)),
            Arc::new(RwLock::new(mock_chain_observer)),
        );
        assert!(signer.run().await.is_ok());
    }

    #[tokio::test]
    async fn signer_fails_if_signature_computation_fails() {
        let epoch = fake_data::beacon().epoch;
        let stake_store = setup_stake_store(5).await;
        let stake_distribution_expected: StakeDistribution = stake_store
            .get_stakes(epoch)
            .await
            .unwrap()
            .unwrap()
            .iter()
            .map(|(_, signer)| (signer.party_id.to_owned(), signer.stake))
            .collect::<_>();
        let mut mock_certificate_handler = MockCertificateHandler::new();
        let mut mock_single_signer = MockSingleSigner::new();
        let mut mock_digester = MockDigesterImpl::new();
        let mut mock_chain_observer = MockChainObserverImpl::new();
        let pending_certificate = fake_data::certificate_pending();
        mock_certificate_handler
            .expect_retrieve_pending_certificate()
            .return_once(|| Ok(Some(pending_certificate)));
        mock_single_signer
            .expect_compute_single_signatures()
            .return_once(|_, _, _| Err(SingleSignerError::UnregisteredVerificationKey()));
        mock_single_signer
            .expect_get_protocol_initializer()
            .return_once(move || None);
        mock_digester
            .expect_compute_digest()
            .return_once(|| Ok(fake_data::digester_result("digest")));
        mock_chain_observer
            .expect_get_current_epoch()
            .returning(move || Ok(Some(1)))
            .times(2);
        mock_chain_observer
            .expect_get_current_stake_distribution()
            .return_once(|| Ok(Some(stake_distribution_expected)));

        let mut signer = Runtime::new(
            Box::new(mock_certificate_handler),
            Box::new(mock_single_signer),
            Box::new(mock_digester),
            Arc::new(RwLock::new(stake_store)),
            Arc::new(RwLock::new(mock_chain_observer)),
        );
        assert_eq!(
            RuntimeError::SingleSignaturesComputeFailed(
                SingleSignerError::UnregisteredVerificationKey()
            )
            .to_string(),
            signer.run().await.unwrap_err().to_string()
        );
    }

    #[tokio::test]
    async fn signer_fails_when_register_signer_fails() {
        let current_signer = &setup_signers(1)[0];
        let party_id = current_signer.clone().0;
        let protocol_initializer = current_signer.4.clone();
        let pending_certificate = fake_data::certificate_pending();
        let stake_store = setup_stake_store(5).await;
        let mut mock_certificate_handler = MockCertificateHandler::new();
        let mut mock_single_signer = MockSingleSigner::new();
        let mock_digester = MockDigesterImpl::new();
        let mut mock_chain_observer = MockChainObserverImpl::new();
        mock_certificate_handler
            .expect_retrieve_pending_certificate()
            .return_once(|| Ok(Some(pending_certificate)));
        mock_certificate_handler
            .expect_register_signer()
            .return_once(|_| {
                Err(CertificateHandlerError::RemoteServerLogical(
                    "an error occurred".to_string(),
                ))
            });
        mock_single_signer
            .expect_compute_single_signatures()
            .never();
        mock_single_signer
            .expect_get_party_id()
            .return_once(move || party_id);
        mock_single_signer
            .expect_get_protocol_initializer()
            .return_once(move || Some(protocol_initializer));
        mock_chain_observer
            .expect_get_current_epoch()
            .returning(move || Ok(Some(1)))
            .times(2);
        mock_chain_observer
            .expect_get_current_stake_distribution()
            .return_once(|| Ok(Some(StakeDistribution::default())));

        let mut signer = Runtime::new(
            Box::new(mock_certificate_handler),
            Box::new(mock_single_signer),
            Box::new(mock_digester),
            Arc::new(RwLock::new(stake_store)),
            Arc::new(RwLock::new(mock_chain_observer)),
        );
        assert_eq!(
            RuntimeError::RegisterSignerFailed(
                CertificateHandlerError::RemoteServerLogical("an error occurred".to_string())
                    .to_string()
            )
            .to_string(),
            signer.run().await.unwrap_err().to_string()
        );
    }

    #[tokio::test]
    async fn signer_fails_if_digest_computation_fails() {
        let stake_store = setup_stake_store(5).await;
        let mut mock_certificate_handler = MockCertificateHandler::new();
        let mut mock_single_signer = MockSingleSigner::new();
        let mut mock_digester = MockDigesterImpl::new();
        let mut mock_chain_observer = MockChainObserverImpl::new();
        let pending_certificate = fake_data::certificate_pending();
        mock_certificate_handler
            .expect_retrieve_pending_certificate()
            .return_once(|| Ok(Some(pending_certificate)));
        mock_single_signer
            .expect_compute_single_signatures()
            .never();
        mock_single_signer
            .expect_get_protocol_initializer()
            .return_once(move || None);
        mock_digester
            .expect_compute_digest()
            .return_once(|| Err(DigesterError::NotEnoughImmutable()));
        mock_chain_observer
            .expect_get_current_epoch()
            .returning(move || Ok(Some(1)))
            .times(2);
        mock_chain_observer
            .expect_get_current_stake_distribution()
            .return_once(|| Ok(Some(StakeDistribution::default())));

        let mut signer = Runtime::new(
            Box::new(mock_certificate_handler),
            Box::new(mock_single_signer),
            Box::new(mock_digester),
            Arc::new(RwLock::new(stake_store)),
            Arc::new(RwLock::new(mock_chain_observer)),
        );
        assert_eq!(
            RuntimeError::Digester(DigesterError::NotEnoughImmutable()).to_string(),
            signer.run().await.unwrap_err().to_string()
        );
    }
}
