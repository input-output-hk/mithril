//! Signed Entity helpers for persistence

use mithril_common::entities::{
    CardanoDbBeacon, Epoch, SignedEntityType, SignedEntityTypeDiscriminants,
};

use crate::sqlite::HydrationError;

/// Helper struct to hydrate [SignedEntityType].
pub struct SignedEntityTypeHydrator {}

impl SignedEntityTypeHydrator {
    /// Create a [SignedEntityType] from data coming from the database
    pub fn hydrate(
        signed_entity_type_id: usize,
        beacon_str: &str,
    ) -> Result<SignedEntityType, HydrationError> {
        let signed_entity = match SignedEntityTypeDiscriminants::from_id(signed_entity_type_id)
            .map_err(|e| {
                HydrationError::InvalidData(format!("Unknown signed entity. Error: {e}."))
            })? {
            SignedEntityTypeDiscriminants::MithrilStakeDistribution => {
                let epoch: Epoch = serde_json::from_str(beacon_str).map_err(|e| {
                    HydrationError::InvalidData(format!(
                        "Invalid Epoch JSON representation '{beacon_str}. Error: {e}'."
                    ))
                })?;
                SignedEntityType::MithrilStakeDistribution(epoch)
            }
            SignedEntityTypeDiscriminants::CardanoStakeDistribution => {
                let epoch: Epoch = serde_json::from_str(beacon_str).map_err(|e| {
                    HydrationError::InvalidData(format!(
                        "Invalid Epoch JSON representation '{beacon_str}. Error: {e}'."
                    ))
                })?;
                SignedEntityType::CardanoStakeDistribution(epoch)
            }
            SignedEntityTypeDiscriminants::CardanoImmutableFilesFull => {
                let beacon: CardanoDbBeacon = serde_json::from_str(beacon_str).map_err(|e| {
                    HydrationError::InvalidData(format!(
                        "Invalid Beacon JSON in open_message.beacon: '{beacon_str}'. Error: {e}"
                    ))
                })?;
                SignedEntityType::CardanoImmutableFilesFull(beacon)
            }
            SignedEntityTypeDiscriminants::CardanoTransactions => {
                let beacon: CardanoDbBeacon = serde_json::from_str(beacon_str).map_err(|e| {
                    HydrationError::InvalidData(format!(
                        "Invalid Beacon JSON in open_message.beacon: '{beacon_str}'. Error: {e}"
                    ))
                })?;
                SignedEntityType::CardanoTransactions(beacon)
            }
        };

        Ok(signed_entity)
    }
}
