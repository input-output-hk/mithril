use crate::StdResult;
use anyhow::anyhow;
use serde::{Deserialize, Serialize};
use std::time::Duration;
use strum::{AsRefStr, Display, EnumDiscriminants, EnumString};

use super::{Beacon, Epoch};

/// Database representation of the SignedEntityType::MithrilStakeDistribution value
const ENTITY_TYPE_MITHRIL_STAKE_DISTRIBUTION: usize = 0;

/// Database representation of the SignedEntityType::CardanoStakeDistribution value
const ENTITY_TYPE_CARDANO_STAKE_DISTRIBUTION: usize = 1;

/// Database representation of the SignedEntityType::CardanoImmutableFilesFull value
const ENTITY_TYPE_CARDANO_IMMUTABLE_FILES_FULL: usize = 2;

/// Database representation of the SignedEntityType::CardanoTransactions value
const ENTITY_TYPE_CARDANO_TRANSACTIONS: usize = 3;

/// The signed entity type that represents a type of data signed by the Mithril
/// protocol Note: Each variant of this enum must be associated to an entry in
/// the `signed_entity_type` table of the signer/aggregator nodes. The variant
/// are identified by their discriminant (i.e. index in the enum), thus the
/// modification of this type should only ever consist of appending new
/// variants.
#[derive(Display, Debug, Clone, PartialEq, Eq, Serialize, Deserialize, EnumDiscriminants)]
#[strum(serialize_all = "PascalCase")]
#[strum_discriminants(derive(EnumString, AsRefStr, Serialize, Deserialize, PartialOrd, Ord))]
pub enum SignedEntityType {
    /// Mithril stake distribution
    MithrilStakeDistribution(Epoch),

    /// Cardano Stake Distribution
    CardanoStakeDistribution(Epoch),

    /// Full Cardano Immutable Files
    CardanoImmutableFilesFull(Beacon),

    /// Cardano Transactions
    CardanoTransactions(Beacon),
}

impl SignedEntityType {
    /// Retrieve a dummy enty (for test only)
    pub fn dummy() -> Self {
        Self::MithrilStakeDistribution(Epoch(5))
    }

    /// Return the epoch from the intern beacon.
    pub fn get_epoch(&self) -> Epoch {
        match self {
            Self::CardanoImmutableFilesFull(b) | Self::CardanoTransactions(b) => b.epoch,
            Self::CardanoStakeDistribution(e) | Self::MithrilStakeDistribution(e) => *e,
        }
    }

    /// Get the database value from enum's instance
    pub fn index(&self) -> usize {
        match self {
            Self::MithrilStakeDistribution(_) => ENTITY_TYPE_MITHRIL_STAKE_DISTRIBUTION,
            Self::CardanoStakeDistribution(_) => ENTITY_TYPE_CARDANO_STAKE_DISTRIBUTION,
            Self::CardanoImmutableFilesFull(_) => ENTITY_TYPE_CARDANO_IMMUTABLE_FILES_FULL,
            Self::CardanoTransactions(_) => ENTITY_TYPE_CARDANO_TRANSACTIONS,
        }
    }

    /// Return a JSON serialized value of the internal beacon
    pub fn get_json_beacon(&self) -> StdResult<String> {
        let value = match self {
            Self::CardanoImmutableFilesFull(value) | Self::CardanoTransactions(value) => {
                serde_json::to_string(value)?
            }
            Self::CardanoStakeDistribution(value) | Self::MithrilStakeDistribution(value) => {
                serde_json::to_string(value)?
            }
        };

        Ok(value)
    }

    /// Return the associated open message timeout
    pub fn get_open_message_timeout(&self) -> Option<Duration> {
        match self {
            Self::MithrilStakeDistribution(_) | Self::CardanoImmutableFilesFull(_) => None,
            Self::CardanoStakeDistribution(_) | Self::CardanoTransactions(_) => {
                Some(Duration::from_secs(600))
            }
        }
    }

    /// Create a SignedEntityType from beacon and SignedEntityTypeDiscriminants
    pub fn from_beacon(discriminant: &SignedEntityTypeDiscriminants, beacon: &Beacon) -> Self {
        match discriminant {
            SignedEntityTypeDiscriminants::MithrilStakeDistribution => {
                Self::MithrilStakeDistribution(beacon.epoch)
            }
            SignedEntityTypeDiscriminants::CardanoStakeDistribution => {
                Self::CardanoStakeDistribution(beacon.epoch)
            }
            SignedEntityTypeDiscriminants::CardanoImmutableFilesFull => {
                Self::CardanoImmutableFilesFull(beacon.to_owned())
            }
            SignedEntityTypeDiscriminants::CardanoTransactions => {
                Self::CardanoTransactions(beacon.to_owned())
            }
        }
    }
}

impl SignedEntityTypeDiscriminants {
    /// Get the database value from enum's instance
    pub fn index(&self) -> usize {
        match self {
            Self::MithrilStakeDistribution => ENTITY_TYPE_MITHRIL_STAKE_DISTRIBUTION,
            Self::CardanoStakeDistribution => ENTITY_TYPE_CARDANO_STAKE_DISTRIBUTION,
            Self::CardanoImmutableFilesFull => ENTITY_TYPE_CARDANO_IMMUTABLE_FILES_FULL,
            Self::CardanoTransactions => ENTITY_TYPE_CARDANO_TRANSACTIONS,
        }
    }

    /// Get the discriminant associated with the given id
    pub fn from_id(signed_entity_type_id: usize) -> StdResult<SignedEntityTypeDiscriminants> {
        match signed_entity_type_id {
            ENTITY_TYPE_MITHRIL_STAKE_DISTRIBUTION => Ok(Self::MithrilStakeDistribution),
            ENTITY_TYPE_CARDANO_STAKE_DISTRIBUTION => Ok(Self::CardanoStakeDistribution),
            ENTITY_TYPE_CARDANO_IMMUTABLE_FILES_FULL => Ok(Self::CardanoImmutableFilesFull),
            ENTITY_TYPE_CARDANO_TRANSACTIONS => Ok(Self::CardanoTransactions),
            index => Err(anyhow!("Invalid entity_type_id {index}.")),
        }
    }
}

#[cfg(test)]
mod tests {}
