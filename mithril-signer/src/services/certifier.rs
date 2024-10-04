use async_trait::async_trait;
use chrono::Utc;
use std::sync::Arc;

use mithril_common::entities::{SignedEntityConfig, SignedEntityType, TimePoint};
use mithril_common::signed_entity_type_lock::SignedEntityTypeLock;
use mithril_common::{StdResult, TickerService};

use crate::entities::BeaconToSign;

/// Certifier Service
///
/// This service is responsible for providing the beacons that need to be signed by the signer.
#[cfg_attr(test, mockall::automock)]
#[async_trait]
pub trait CertifierService: Sync + Send {
    /// Get the beacon to sign.
    ///
    /// If all available signed entity have already been signed, `None` is returned.
    async fn get_beacon_to_sign(&self) -> StdResult<Option<BeaconToSign>>;

    /// Mark a beacon as signed so it won't be returned by `get_beacon_to_sign` anymore.
    async fn mark_beacon_as_signed(&self, signed_beacon: &BeaconToSign) -> StdResult<()>;
}

/// Trait to provide the current signed entity configuration that can change over time.
#[cfg_attr(test, mockall::automock)]
#[async_trait]
pub trait SignedEntityConfigProvider: Sync + Send {
    /// Get the current signed entity configuration.
    async fn get(&self) -> StdResult<SignedEntityConfig>;
}

/// Trait to store beacons that have been signed in order to avoid signing them twice.
#[cfg_attr(test, mockall::automock)]
#[async_trait]
pub trait SignedBeaconStore: Sync + Send {
    /// Filter out already signed entities from a list of signed entities.
    async fn filter_out_already_signed_entities(
        &self,
        entities: Vec<SignedEntityType>,
    ) -> StdResult<Vec<SignedEntityType>>;

    /// Mark a beacon as signed.
    async fn mark_beacon_as_signed(&self, entity: &BeaconToSign) -> StdResult<()>;
}

/// Implementation of the [Certifier Service][CertifierService] for the Mithril Signer.
pub struct SignerCertifierService {
    ticker_service: Arc<dyn TickerService>,
    signed_beacon_store: Arc<dyn SignedBeaconStore>,
    signed_entity_config_provider: Arc<dyn SignedEntityConfigProvider>,
    signed_entity_type_lock: Arc<SignedEntityTypeLock>,
}

impl SignerCertifierService {
    /// Create a new `SignerCertifierService` instance.
    pub fn new(
        ticker_service: Arc<dyn TickerService>,
        signed_beacon_store: Arc<dyn SignedBeaconStore>,
        signed_entity_config_provider: Arc<dyn SignedEntityConfigProvider>,
        signed_entity_type_lock: Arc<SignedEntityTypeLock>,
    ) -> Self {
        Self {
            ticker_service,
            signed_beacon_store,
            signed_entity_config_provider,
            signed_entity_type_lock,
        }
    }

    async fn list_available_signed_entity_types(
        &self,
        time_point: &TimePoint,
    ) -> StdResult<Vec<SignedEntityType>> {
        let signed_entity_types = self
            .signed_entity_config_provider
            .get()
            .await?
            .list_allowed_signed_entity_types(time_point)?;
        let unlocked_signed_entities = self
            .signed_entity_type_lock
            .filter_unlocked_entries(signed_entity_types)
            .await;
        let not_already_signed_entities = self
            .signed_beacon_store
            .filter_out_already_signed_entities(unlocked_signed_entities)
            .await?;

        Ok(not_already_signed_entities)
    }
}

#[async_trait]
impl CertifierService for SignerCertifierService {
    async fn get_beacon_to_sign(&self) -> StdResult<Option<BeaconToSign>> {
        let time_point = self.ticker_service.get_current_time_point().await?;

        let available_signed_entity_types =
            self.list_available_signed_entity_types(&time_point).await?;

        if available_signed_entity_types.is_empty() {
            Ok(None)
        } else {
            let signed_entity_type = available_signed_entity_types[0].clone();
            let beacon_to_sign =
                BeaconToSign::new(time_point.epoch, signed_entity_type, Utc::now());

            Ok(Some(beacon_to_sign))
        }
    }

    async fn mark_beacon_as_signed(&self, signed_beacon: &BeaconToSign) -> StdResult<()> {
        self.signed_beacon_store
            .mark_beacon_as_signed(signed_beacon)
            .await
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::entities::{
        CardanoTransactionsSigningConfig, ChainPoint, Epoch, SignedEntityTypeDiscriminants,
    };

    use super::{tests::tests_tooling::*, *};

    #[tokio::test]
    async fn no_beacon_can_be_signed_if_all_entities_are_locked() {
        let locker = Arc::new(SignedEntityTypeLock::new());
        for signed_entity_type in SignedEntityTypeDiscriminants::all() {
            locker.lock(signed_entity_type).await;
        }
        let certifier_service = SignerCertifierService {
            signed_entity_type_lock: locker.clone(),
            signed_entity_config_provider: Arc::new(DumbSignedEntityConfigProvider::new(
                CardanoTransactionsSigningConfig::dummy(),
                SignedEntityTypeDiscriminants::all(),
            )),
            ..SignerCertifierService::dumb_dependencies(TimePoint::new(1, 14, ChainPoint::dummy()))
        };

        let beacon_to_sign = certifier_service.get_beacon_to_sign().await.unwrap();
        assert_eq!(beacon_to_sign, None);
    }

    #[tokio::test]
    async fn only_one_unlocked_and_not_yet_signed_yield_a_beacon_to_sign() {
        let locker = Arc::new(SignedEntityTypeLock::new());
        for signed_entity_type in SignedEntityTypeDiscriminants::all().into_iter().skip(1) {
            locker.lock(signed_entity_type).await;
        }
        let certifier_service = SignerCertifierService {
            signed_entity_type_lock: locker.clone(),
            signed_entity_config_provider: Arc::new(DumbSignedEntityConfigProvider::new(
                CardanoTransactionsSigningConfig::dummy(),
                SignedEntityTypeDiscriminants::all(),
            )),
            ..SignerCertifierService::dumb_dependencies(TimePoint::new(3, 14, ChainPoint::dummy()))
        };

        let beacon_to_sign = certifier_service.get_beacon_to_sign().await.unwrap();
        let signed_discriminant: Option<SignedEntityTypeDiscriminants> =
            beacon_to_sign.map(|b| b.signed_entity_type.into());

        assert_eq!(
            SignedEntityTypeDiscriminants::all().first().cloned(),
            signed_discriminant
        );
    }

    #[tokio::test]
    async fn mark_beacon_as_signed_update_the_store() {
        let signed_beacons_store = Arc::new(DumbSignedBeaconStore::default());
        let certifier_service = SignerCertifierService {
            signed_beacon_store: signed_beacons_store.clone(),
            signed_entity_config_provider: Arc::new(DumbSignedEntityConfigProvider::new(
                CardanoTransactionsSigningConfig::dummy(),
                SignedEntityTypeDiscriminants::all(),
            )),
            ..SignerCertifierService::dumb_dependencies(TimePoint::dummy())
        };

        certifier_service
            .mark_beacon_as_signed(&BeaconToSign {
                epoch: Epoch(1),
                signed_entity_type: SignedEntityType::MithrilStakeDistribution(Epoch(4)),
                initiated_at: Utc::now(),
            })
            .await
            .unwrap();

        let signed_beacons = signed_beacons_store.signed_beacons().await;
        assert_eq!(
            vec![SignedEntityType::MithrilStakeDistribution(Epoch(4))],
            signed_beacons
        );
    }

    #[tokio::test]
    async fn if_already_signed_a_beacon_is_not_returned_anymore() {
        let certifier_service = SignerCertifierService {
            signed_beacon_store: Arc::new(DumbSignedBeaconStore::default()),
            signed_entity_config_provider: Arc::new(DumbSignedEntityConfigProvider::new(
                CardanoTransactionsSigningConfig::dummy(),
                SignedEntityTypeDiscriminants::all(),
            )),
            ..SignerCertifierService::dumb_dependencies(TimePoint::new(1, 14, ChainPoint::dummy()))
        };

        let first_beacon_to_sign = certifier_service
            .get_beacon_to_sign()
            .await
            .unwrap()
            .unwrap();
        certifier_service
            .mark_beacon_as_signed(&first_beacon_to_sign.clone())
            .await
            .unwrap();
        let second_beacon_to_sign = certifier_service
            .get_beacon_to_sign()
            .await
            .unwrap()
            .unwrap();

        assert_ne!(
            first_beacon_to_sign.signed_entity_type,
            second_beacon_to_sign.signed_entity_type
        );
    }

    #[tokio::test]
    async fn draining_out_all_beacons_to_sign_use_signed_entity_discriminant_order() {
        let certifier_service = SignerCertifierService {
            signed_entity_config_provider: Arc::new(DumbSignedEntityConfigProvider::new(
                CardanoTransactionsSigningConfig::dummy(),
                SignedEntityTypeDiscriminants::all(),
            )),
            ..SignerCertifierService::dumb_dependencies(TimePoint::new(1, 14, ChainPoint::dummy()))
        };

        let mut previous_beacon_to_sign = certifier_service
            .get_beacon_to_sign()
            .await
            .unwrap()
            .expect("There should be a beacon to sign since nothing is locked or signed");

        loop {
            certifier_service
                .mark_beacon_as_signed(&previous_beacon_to_sign)
                .await
                .unwrap();
            let next_beacon_to_sign = certifier_service.get_beacon_to_sign().await.unwrap();

            if let Some(beacon) = next_beacon_to_sign {
                assert!(
                    SignedEntityTypeDiscriminants::from(
                        &previous_beacon_to_sign.signed_entity_type
                    ) < SignedEntityTypeDiscriminants::from(&beacon.signed_entity_type),
                    "Beacon should follow SignedEntityTypeDiscriminants order"
                );

                previous_beacon_to_sign = beacon;
            } else {
                break;
            }
        }
    }

    #[tokio::test]
    async fn draining_out_all_beacons_to_sign_doesnt_repeat_value() {
        let certifier_service = SignerCertifierService {
            signed_entity_config_provider: Arc::new(DumbSignedEntityConfigProvider::new(
                CardanoTransactionsSigningConfig::dummy(),
                SignedEntityTypeDiscriminants::all(),
            )),
            ..SignerCertifierService::dumb_dependencies(TimePoint::new(1, 14, ChainPoint::dummy()))
        };

        let mut all_signed_beacons = vec![];
        while let Some(beacon_to_sign) = certifier_service.get_beacon_to_sign().await.unwrap() {
            certifier_service
                .mark_beacon_as_signed(&beacon_to_sign)
                .await
                .unwrap();
            all_signed_beacons.push(beacon_to_sign);
        }

        let mut dedup_signed_beacons = all_signed_beacons.clone();
        dedup_signed_beacons.dedup();
        assert!(
            !all_signed_beacons.is_empty(),
            "There should be at least one beacon to sign"
        );
        assert_eq!(
            all_signed_beacons, dedup_signed_beacons,
            "Beacon should not repeat"
        );
    }

    pub mod tests_tooling {
        use std::collections::BTreeSet;
        use tokio::sync::RwLock;

        use mithril_common::CardanoNetwork;

        use super::*;

        impl SignerCertifierService {
            pub fn dumb_dependencies(initial_time_point: TimePoint) -> Self {
                Self {
                    ticker_service: Arc::new(DumbTickerService::new(initial_time_point)),
                    signed_beacon_store: Arc::new(DumbSignedBeaconStore::default()),
                    signed_entity_config_provider: Arc::new(DumbSignedEntityConfigProvider::new(
                        CardanoTransactionsSigningConfig::dummy(),
                        SignedEntityTypeDiscriminants::all(),
                    )),
                    signed_entity_type_lock: Arc::new(SignedEntityTypeLock::new()),
                }
            }
        }

        pub struct DumbTickerService {
            time_point: TimePoint,
        }

        impl DumbTickerService {
            pub fn new(time_point: TimePoint) -> Self {
                Self { time_point }
            }
        }

        #[async_trait]
        impl TickerService for DumbTickerService {
            async fn get_current_time_point(&self) -> StdResult<TimePoint> {
                Ok(self.time_point.clone())
            }
        }

        pub struct DumbSignedEntityConfigProvider {
            config: SignedEntityConfig,
        }

        impl DumbSignedEntityConfigProvider {
            pub fn new(
                config: CardanoTransactionsSigningConfig,
                allowed_discriminants: BTreeSet<SignedEntityTypeDiscriminants>,
            ) -> Self {
                Self {
                    config: SignedEntityConfig {
                        cardano_transactions_signing_config: config,
                        network: CardanoNetwork::TestNet(42),
                        allowed_discriminants,
                    },
                }
            }
        }

        #[async_trait]
        impl SignedEntityConfigProvider for DumbSignedEntityConfigProvider {
            async fn get(&self) -> StdResult<SignedEntityConfig> {
                Ok(self.config.clone())
            }
        }

        pub struct DumbSignedBeaconStore {
            signed_beacons: RwLock<Vec<SignedEntityType>>,
        }

        impl DumbSignedBeaconStore {
            pub fn new(signed_beacons: Vec<SignedEntityType>) -> Self {
                Self {
                    signed_beacons: RwLock::new(signed_beacons),
                }
            }

            pub async fn signed_beacons(&self) -> Vec<SignedEntityType> {
                self.signed_beacons.read().await.clone()
            }
        }

        impl Default for DumbSignedBeaconStore {
            fn default() -> Self {
                Self::new(vec![])
            }
        }

        #[async_trait]
        impl SignedBeaconStore for DumbSignedBeaconStore {
            async fn filter_out_already_signed_entities(
                &self,
                entities: Vec<SignedEntityType>,
            ) -> StdResult<Vec<SignedEntityType>> {
                let already_signed_entities = self.signed_beacons.read().await.clone();
                Ok(entities
                    .into_iter()
                    .filter(|entity| !already_signed_entities.contains(entity))
                    .collect())
            }

            async fn mark_beacon_as_signed(&self, beacon: &BeaconToSign) -> StdResult<()> {
                let mut already_signed_entities = self.signed_beacons.write().await;
                already_signed_entities.push(beacon.signed_entity_type.clone());
                Ok(())
            }
        }
    }
}
