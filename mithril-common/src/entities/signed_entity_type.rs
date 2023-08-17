use serde::{Deserialize, Serialize};
use strum_macros::{Display, EnumDiscriminants};

use crate::{sqlite::HydrationError, StdResult};

use super::{Beacon, Epoch};

/// Database representation of the SignedEntityType::MithrilStakeDistribution value
const ENTITY_TYPE_MITHRIL_STAKE_DISTRIBUTION: usize = 0;

/// Database representation of the SignedEntityType::CardanoStakeDistribution value
const ENTITY_TYPE_CARDANO_STAKE_DISTRIBUTION: usize = 1;

/// Database representation of the SignedEntityType::CardanoImmutableFilesFull value
const ENTITY_TYPE_CARDANO_IMMUTABLE_FILES_FULL: usize = 2;

/// The signed entity type that represents a type of data signed by the Mithril
/// protocol Note: Each variant of this enum must be associated to an entry in
/// the `signed_entity_type` table of the signer/aggregator nodes. The variant
/// are identified by their discriminant (i.e. index in the enum), thus the
/// modification of this type should only ever consist of appending new
/// variants.
#[derive(Display, Debug, Clone, PartialEq, Eq, Serialize, Deserialize, EnumDiscriminants)]
#[strum(serialize_all = "PascalCase")]
pub enum SignedEntityType {
    /// Mithril stake distribution
    MithrilStakeDistribution(Epoch),

    /// Cardano Stake Distribution
    CardanoStakeDistribution(Epoch),

    /// Full Cardano Immutable Files
    CardanoImmutableFilesFull(Beacon),
}

impl SignedEntityType {
    /// Retrieve a dummy enty (for test only)
    pub fn dummy() -> Self {
        Self::MithrilStakeDistribution(Epoch(5))
    }

    /// Return the epoch from the intern beacon.
    pub fn get_epoch(&self) -> Epoch {
        match self {
            Self::CardanoImmutableFilesFull(b) => b.epoch,
            Self::CardanoStakeDistribution(e) | Self::MithrilStakeDistribution(e) => *e,
        }
    }
    /// Create an instance from data coming from the database
    pub fn hydrate(signed_entity_type_id: usize, beacon_str: &str) -> Result<Self, HydrationError> {
        let myself = match signed_entity_type_id {
            ENTITY_TYPE_MITHRIL_STAKE_DISTRIBUTION => {
                let epoch: Epoch = serde_json::from_str(beacon_str).map_err(|e| {
                    HydrationError::InvalidData(format!(
                        "Invalid Epoch JSON representation '{beacon_str}. Error: {e}'."
                    ))
                })?;
                Self::MithrilStakeDistribution(epoch)
            }
            ENTITY_TYPE_CARDANO_STAKE_DISTRIBUTION => {
                let epoch: Epoch = serde_json::from_str(beacon_str).map_err(|e| {
                    HydrationError::InvalidData(format!(
                        "Invalid Epoch JSON representation '{beacon_str}. Error: {e}'."
                    ))
                })?;
                Self::CardanoStakeDistribution(epoch)
            }
            ENTITY_TYPE_CARDANO_IMMUTABLE_FILES_FULL => {
                let beacon: Beacon = serde_json::from_str(beacon_str).map_err(|e| {
                    HydrationError::InvalidData(format!(
                        "Invalid Beacon JSON in open_message.beacon: '{beacon_str}'. Error: {e}"
                    ))
                })?;
                Self::CardanoImmutableFilesFull(beacon)
            }
            index => panic!("Invalid entity_type_id {index}."),
        };

        Ok(myself)
    }

    /// Get the database value from enum's instance
    pub fn index(&self) -> usize {
        match self {
            Self::MithrilStakeDistribution(_) => ENTITY_TYPE_MITHRIL_STAKE_DISTRIBUTION,
            Self::CardanoStakeDistribution(_) => ENTITY_TYPE_CARDANO_STAKE_DISTRIBUTION,
            Self::CardanoImmutableFilesFull(_) => ENTITY_TYPE_CARDANO_IMMUTABLE_FILES_FULL,
        }
    }

    /// Return a JSON serialized value of the internal beacon
    pub fn get_json_beacon(&self) -> StdResult<String> {
        let value = match self {
            Self::CardanoImmutableFilesFull(value) => serde_json::to_string(value)?,
            Self::CardanoStakeDistribution(value) | Self::MithrilStakeDistribution(value) => {
                serde_json::to_string(value)?
            }
        };

        Ok(value)
    }
}

impl SignedEntityTypeDiscriminants {
    /// Get the database value from enum's instance
    pub fn index(&self) -> usize {
        match self {
            Self::MithrilStakeDistribution => ENTITY_TYPE_MITHRIL_STAKE_DISTRIBUTION,
            Self::CardanoStakeDistribution => ENTITY_TYPE_CARDANO_STAKE_DISTRIBUTION,
            Self::CardanoImmutableFilesFull => ENTITY_TYPE_CARDANO_IMMUTABLE_FILES_FULL,
        }
    }
}

#[cfg(test)]
mod tests {}
