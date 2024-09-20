use async_trait::async_trait;
use chrono::{DateTime, Utc};
use std::sync::Arc;

use mithril_common::entities::{Epoch, SignedEntityType};
use mithril_common::signed_entity_type_lock::SignedEntityTypeLock;
use mithril_common::StdResult;

/// Beacon to sign
#[derive(Debug, Clone, PartialEq)]
pub struct BeaconToSign {
    /// The epoch when the beacon was issued
    pub epoch: Epoch,

    /// The signed entity type to sign
    pub signed_entity_type: SignedEntityType,

    /// Datetime when the beacon was initiated
    pub initiated_at: DateTime<Utc>,
}

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
}

/// Implementation of the [Certifier Service][CertifierService] for the Mithril Signer.
pub struct SignerCertifierService {
    signed_entity_type_lock: Arc<SignedEntityTypeLock>,
}

impl SignerCertifierService {
    /// Create a new `SignerCertifierService` instance.
    pub fn new(signed_entity_type_lock: Arc<SignedEntityTypeLock>) -> Self {
        Self {
            signed_entity_type_lock,
        }
    }
}

#[async_trait]
impl CertifierService for SignerCertifierService {
    async fn get_beacon_to_sign(&self) -> StdResult<Option<BeaconToSign>> {
        Ok(None)
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::entities::SignedEntityTypeDiscriminants;

    use super::*;

    #[tokio::test]
    async fn no_beacon_can_be_signed_if_all_entities_are_locked() {
        let locker = Arc::new(SignedEntityTypeLock::new());
        for signed_entity_type in SignedEntityTypeDiscriminants::all() {
            locker.lock(signed_entity_type).await;
        }
        let certifier_service = SignerCertifierService::new(locker);

        let beacon_to_sign = certifier_service.get_beacon_to_sign().await.unwrap();
        assert_eq!(beacon_to_sign, None);
    }
}
