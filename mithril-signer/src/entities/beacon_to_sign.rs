use chrono::{DateTime, Utc};

use mithril_common::entities::{Epoch, SignedEntityType};

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

impl BeaconToSign {
    /// Create a new `BeaconToSign`
    pub fn new(
        epoch: Epoch,
        signed_entity_type: SignedEntityType,
        initiated_at: DateTime<Utc>,
    ) -> Self {
        Self {
            epoch,
            signed_entity_type,
            initiated_at,
        }
    }
}
