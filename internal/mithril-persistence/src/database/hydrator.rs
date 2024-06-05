//! Shared hydrator helpers for persistence

use serde::Deserialize;

use mithril_common::entities::{
    BlockNumber, CardanoDbBeacon, Epoch, SignedEntityType, SignedEntityTypeDiscriminants,
};

use crate::sqlite::HydrationError;

/// Helper struct to hydrate common data.
pub struct Hydrator;

impl Hydrator {
    /// Read a signed entity beacon column from the database
    pub fn read_signed_entity_beacon_column<U: sqlite::RowIndex + Clone>(
        row: &sqlite::Row,
        column_index: U,
    ) -> String {
        // We need to check first that the cell can be read as a string first
        // (e.g. when beacon json is '{"network": "dev", "epoch": 1, "immutable_file_number": 2}').
        // If it fails, we fallback on reading the cell as an integer (e.g. when beacon json is '5').
        // TODO: Maybe there is a better way of doing this.
        match row.try_read::<&str, _>(column_index.clone()) {
            Ok(value) => value.to_string(),
            Err(_) => (row.read::<i64, _>(column_index)).to_string(),
        }
    }

    /// Try to convert an i64 field from the database to a u64
    pub fn try_to_u64(field: &str, value: i64) -> Result<u64, HydrationError> {
        u64::try_from(value)
            .map_err(|e|
                HydrationError::InvalidData(
                    format!("Integer field {field} (value={value}) is incompatible with u64 representation. Error = {e}")
                )
            )
    }

    /// Create a [SignedEntityType] from data coming from the database
    pub fn hydrate_signed_entity_type(
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
                #[derive(Deserialize)]
                struct CardanoTransactionsBeacon {
                    epoch: Epoch,
                    block_number: BlockNumber,
                }

                let beacon: CardanoTransactionsBeacon =
                    serde_json::from_str(beacon_str).map_err(|e| {
                        HydrationError::InvalidData(format!(
                        "Invalid Beacon JSON in open_message.beacon: '{beacon_str}'. Error: {e}"
                    ))
                    })?;
                SignedEntityType::CardanoTransactions(beacon.epoch, beacon.block_number)
            }
        };

        Ok(signed_entity)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn hydrate_cardano_transaction_signed_entity_type() {
        let expected = SignedEntityType::CardanoTransactions(Epoch(35), 77);
        let signed_entity = Hydrator::hydrate_signed_entity_type(
            SignedEntityTypeDiscriminants::CardanoTransactions.index(),
            &expected.get_json_beacon().unwrap(),
        )
        .unwrap();

        assert_eq!(expected, signed_entity);
    }
}
