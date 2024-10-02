use chrono::{DateTime, Utc};
use sqlite::Row;

use mithril_common::entities::{Epoch, SignedEntityType};
use mithril_persistence::database::Hydrator;
use mithril_persistence::sqlite::{HydrationError, Projection, SqLiteEntity};

use crate::entities::BeaconToSign;

/// Database record of a beacon signed by the signer
#[derive(Debug, Clone, PartialEq)]
pub struct SignedBeaconRecord {
    /// The epoch when the beacon was issued
    pub epoch: Epoch,

    /// The signed entity type to sign
    pub signed_entity_type: SignedEntityType,

    /// Datetime when the beacon was initiated
    pub initiated_at: DateTime<Utc>,

    /// Datetime when the beacon was signed
    pub signed_at: DateTime<Utc>,
}

impl From<BeaconToSign> for SignedBeaconRecord {
    fn from(beacon: BeaconToSign) -> Self {
        Self {
            epoch: beacon.epoch,
            signed_entity_type: beacon.signed_entity_type,
            initiated_at: beacon.initiated_at,
            signed_at: Utc::now(),
        }
    }
}

#[cfg(test)]
impl SignedBeaconRecord {
    /// Create a fake `SignedBeaconRecord` for testing purposes
    ///
    /// The dates fields are set to constant values to make it easier to compare.
    pub(crate) fn fake(epoch: Epoch, signed_entity_type: SignedEntityType) -> Self {
        let initiated_at = DateTime::<Utc>::default();
        Self {
            epoch,
            signed_entity_type,
            initiated_at,
            signed_at: initiated_at + chrono::TimeDelta::minutes(3),
        }
    }

    pub(crate) fn fakes(records: &[(Epoch, Vec<SignedEntityType>)]) -> Vec<Self> {
        records
            .iter()
            .flat_map(|(epoch, signed_entity_types)| {
                signed_entity_types
                    .iter()
                    .map(|se| Self::fake(*epoch, se.clone()))
            })
            .collect()
    }
}

#[cfg(test)]
impl PartialEq<BeaconToSign> for SignedBeaconRecord {
    fn eq(&self, other: &BeaconToSign) -> bool {
        self.epoch.eq(&other.epoch)
            && self.signed_entity_type.eq(&other.signed_entity_type)
            && self.initiated_at.eq(&other.initiated_at)
    }
}

#[cfg(test)]
impl PartialEq<SignedBeaconRecord> for BeaconToSign {
    fn eq(&self, other: &SignedBeaconRecord) -> bool {
        other.eq(self)
    }
}

impl SqLiteEntity for SignedBeaconRecord {
    fn hydrate(row: Row) -> Result<Self, HydrationError>
    where
        Self: Sized,
    {
        let epoch = row.read::<i64, _>(0);
        let beacon_str = Hydrator::read_signed_entity_beacon_column(&row, 1);
        let signed_entity_type_id = usize::try_from(row.read::<i64, _>(2)).map_err(|e| {
            panic!(
                "Integer field open_message.signed_entity_type_id cannot be turned into usize: {e}"
            )
        })?;
        let initiated_at = &row.read::<&str, _>(3);
        let signed_at = &row.read::<&str, _>(4);

        let open_message = Self {
            epoch: Epoch(epoch.try_into().map_err(|e| {
                HydrationError::InvalidData(format!(
                    "Could not cast i64 ({epoch}) to u64. Error: '{e}'"
                ))
            })?),
            signed_entity_type: Hydrator::hydrate_signed_entity_type(signed_entity_type_id, &beacon_str)?,
            initiated_at: DateTime::parse_from_rfc3339(initiated_at).map_err(|e| {
                HydrationError::InvalidData(format!(
                    "Could not turn signed_beacon.initiated_at field value '{initiated_at}' to rfc3339 Datetime. Error: {e}"
                ))
            })?.with_timezone(&Utc),
            signed_at:DateTime::parse_from_rfc3339(signed_at).map_err(|e| {
                HydrationError::InvalidData(format!(
                    "Could not turn signed_beacon.initiated_at field value '{initiated_at}' to rfc3339 Datetime. Error: {e}"
                ))
            })?.with_timezone(&Utc),
        };

        Ok(open_message)
    }

    fn get_projection() -> Projection {
        Projection::from(&[
            ("epoch", "{:signed_beacon:}.epoch", "int"),
            ("beacon", "{:signed_beacon:}.beacon", "text"),
            (
                "signed_entity_type_id",
                "{:signed_beacon:}.signed_entity_type_id",
                "int",
            ),
            ("created_at", "{:signed_beacon:}.initiated_at", "text"),
            ("expires_at", "{:signed_beacon:}.signed_at", "text"),
        ])
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn eq_beacon_to_sign() {
        let initiated_at = DateTime::<Utc>::default();
        let beacon_to_sign = BeaconToSign {
            epoch: Epoch(3),
            signed_entity_type: SignedEntityType::MithrilStakeDistribution(Epoch(4)),
            initiated_at,
        };
        let signed_beacon = SignedBeaconRecord {
            epoch: Epoch(3),
            signed_entity_type: SignedEntityType::MithrilStakeDistribution(Epoch(4)),
            initiated_at,
            signed_at: initiated_at + chrono::TimeDelta::minutes(2),
        };

        // Check `impl PartialEq<BeaconToSign> for SignedBeaconRecord`
        assert_eq!(signed_beacon, beacon_to_sign);
        assert_ne!(
            signed_beacon,
            BeaconToSign {
                epoch: beacon_to_sign.epoch + 13,
                ..beacon_to_sign.clone()
            }
        );

        // Check `impl PartialEq<SignedBeaconRecord> for BeaconToSign`
        assert_eq!(beacon_to_sign, signed_beacon);
        assert_ne!(
            beacon_to_sign,
            SignedBeaconRecord {
                epoch: signed_beacon.epoch + 11,
                ..signed_beacon.clone()
            }
        );
    }
}
