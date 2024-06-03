use crate::StdResult;
use anyhow::anyhow;
use digest::Update;
use serde::{Deserialize, Serialize};
use sha2::Sha256;
use std::time::Duration;
use strum::{AsRefStr, Display, EnumDiscriminants, EnumString};

use super::{BlockNumber, BlockRange, CardanoDbBeacon, Epoch, TimePoint};

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
// Important note: The order of the variants is important as it is used for the derived Ord trait.
#[derive(Display, Debug, Clone, PartialEq, Eq, Serialize, Deserialize, EnumDiscriminants)]
#[strum(serialize_all = "PascalCase")]
#[strum_discriminants(derive(EnumString, AsRefStr, Serialize, Deserialize, PartialOrd, Ord))]
pub enum SignedEntityType {
    /// Mithril stake distribution
    MithrilStakeDistribution(Epoch),

    /// Cardano Stake Distribution
    CardanoStakeDistribution(Epoch),

    /// Full Cardano Immutable Files
    CardanoImmutableFilesFull(CardanoDbBeacon),

    /// Cardano Transactions
    CardanoTransactions(Epoch, BlockNumber),
}

/// Configuration for the signing of Cardano transactions
///
/// Allow to compute the block number to be signed based on the chain tip block number.
///
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct CardanoTransactionsSigningConfig {
    /// Number of blocks to wait before taking in account a transaction.
    pub security_parameter: BlockNumber,

    /// The frequency at which the transactions are signed.
    ///
    /// *Note: The step is adjusted to be a multiple of the block range length in order.*
    pub step: BlockNumber,
}

impl CardanoTransactionsSigningConfig {
    cfg_test_tools! {
        /// Create a dummy config
        pub fn dummy() -> Self {
            Self {
                security_parameter: 0,
                step: 15,
            }
        }
    }

    /// Compute the block number to be signed based on che chain tip block number.
    ///
    /// Given k' = `security_parameter` and n = `step`,
    /// the latest block number to be signed is computed as *(this use a integer division)*:
    ///
    /// **block_number = ((tip.block_number - k') / n) × n**
    ///
    /// *Note: The step is adjusted to be a multiple of the block range length in order
    /// to guarantee that the block number signed in a certificate is effectively signed.*
    pub fn compute_block_number_to_be_signed(&self, block_number: BlockNumber) -> BlockNumber {
        // TODO: See if we can remove this adjustment by including a "partial" block range in
        // the signed data.
        let adjusted_step = BlockRange::from_block_number(self.step).start;
        // We can't have a step lower than the block range length.
        let adjusted_step = std::cmp::max(adjusted_step, BlockRange::LENGTH);

        (block_number - self.security_parameter) / adjusted_step * adjusted_step
    }
}

impl SignedEntityType {
    /// Retrieve a dummy enty (for test only)
    pub fn dummy() -> Self {
        Self::MithrilStakeDistribution(Epoch(5))
    }

    /// Create a new signed entity type for a genesis certificate (a [Self::MithrilStakeDistribution])
    pub fn genesis(epoch: Epoch) -> Self {
        Self::MithrilStakeDistribution(epoch)
    }

    /// Return the epoch from the intern beacon.
    pub fn get_epoch(&self) -> Epoch {
        match self {
            Self::CardanoImmutableFilesFull(b) => b.epoch,
            Self::CardanoStakeDistribution(e)
            | Self::MithrilStakeDistribution(e)
            | Self::CardanoTransactions(e, _) => *e,
        }
    }

    /// Get the database value from enum's instance
    pub fn index(&self) -> usize {
        match self {
            Self::MithrilStakeDistribution(_) => ENTITY_TYPE_MITHRIL_STAKE_DISTRIBUTION,
            Self::CardanoStakeDistribution(_) => ENTITY_TYPE_CARDANO_STAKE_DISTRIBUTION,
            Self::CardanoImmutableFilesFull(_) => ENTITY_TYPE_CARDANO_IMMUTABLE_FILES_FULL,
            Self::CardanoTransactions(_, _) => ENTITY_TYPE_CARDANO_TRANSACTIONS,
        }
    }

    /// Return a JSON serialized value of the internal beacon
    pub fn get_json_beacon(&self) -> StdResult<String> {
        let value = match self {
            Self::CardanoImmutableFilesFull(value) => serde_json::to_string(value)?,
            Self::CardanoStakeDistribution(value) | Self::MithrilStakeDistribution(value) => {
                serde_json::to_string(value)?
            }
            Self::CardanoTransactions(epoch, block_number) => {
                let json = serde_json::json!({
                    "epoch": epoch,
                    "block_number": block_number,
                });
                serde_json::to_string(&json)?
            }
        };

        Ok(value)
    }

    /// Return the associated open message timeout
    pub fn get_open_message_timeout(&self) -> Option<Duration> {
        match self {
            Self::MithrilStakeDistribution(_) | Self::CardanoImmutableFilesFull(_) => None,
            Self::CardanoStakeDistribution(_) => Some(Duration::from_secs(600)),
            Self::CardanoTransactions(_, _) => Some(Duration::from_secs(1800)),
        }
    }

    /// Create a SignedEntityType from beacon and SignedEntityTypeDiscriminants
    pub fn from_time_point(
        discriminant: &SignedEntityTypeDiscriminants,
        network: &str,
        time_point: &TimePoint,
        cardano_transactions_signing_config: &CardanoTransactionsSigningConfig,
    ) -> Self {
        match discriminant {
            SignedEntityTypeDiscriminants::MithrilStakeDistribution => {
                Self::MithrilStakeDistribution(time_point.epoch)
            }
            SignedEntityTypeDiscriminants::CardanoStakeDistribution => {
                Self::CardanoStakeDistribution(time_point.epoch)
            }
            SignedEntityTypeDiscriminants::CardanoImmutableFilesFull => {
                Self::CardanoImmutableFilesFull(CardanoDbBeacon::new(
                    network,
                    *time_point.epoch,
                    time_point.immutable_file_number,
                ))
            }
            SignedEntityTypeDiscriminants::CardanoTransactions => Self::CardanoTransactions(
                time_point.epoch,
                cardano_transactions_signing_config
                    .compute_block_number_to_be_signed(time_point.chain_point.block_number),
            ),
        }
    }

    pub(crate) fn feed_hash(&self, hasher: &mut Sha256) {
        match self {
            SignedEntityType::MithrilStakeDistribution(epoch)
            | SignedEntityType::CardanoStakeDistribution(epoch) => {
                hasher.update(&epoch.to_be_bytes())
            }
            SignedEntityType::CardanoImmutableFilesFull(db_beacon) => {
                hasher.update(db_beacon.network.as_bytes());
                hasher.update(&db_beacon.epoch.to_be_bytes());
                hasher.update(&db_beacon.immutable_file_number.to_be_bytes());
            }
            SignedEntityType::CardanoTransactions(epoch, block_number) => {
                hasher.update(&epoch.to_be_bytes());
                hasher.update(&block_number.to_be_bytes())
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
mod tests {
    use digest::Digest;

    use crate::test_utils::assert_same_json;

    use super::*;

    #[test]
    fn verify_signed_entity_type_properties_are_included_in_computed_hash() {
        fn hash(signed_entity_type: SignedEntityType) -> String {
            let mut hasher = Sha256::new();
            signed_entity_type.feed_hash(&mut hasher);
            hex::encode(hasher.finalize())
        }

        let reference_hash = hash(SignedEntityType::MithrilStakeDistribution(Epoch(5)));
        assert_ne!(
            reference_hash,
            hash(SignedEntityType::MithrilStakeDistribution(Epoch(15)))
        );

        let reference_hash = hash(SignedEntityType::CardanoStakeDistribution(Epoch(5)));
        assert_ne!(
            reference_hash,
            hash(SignedEntityType::CardanoStakeDistribution(Epoch(15)))
        );

        let reference_hash = hash(SignedEntityType::CardanoImmutableFilesFull(
            CardanoDbBeacon::new("network", 5, 100),
        ));
        assert_ne!(
            reference_hash,
            hash(SignedEntityType::CardanoImmutableFilesFull(
                CardanoDbBeacon::new("other_network", 5, 100)
            ))
        );
        assert_ne!(
            reference_hash,
            hash(SignedEntityType::CardanoImmutableFilesFull(
                CardanoDbBeacon::new("network", 20, 100)
            ))
        );
        assert_ne!(
            reference_hash,
            hash(SignedEntityType::CardanoImmutableFilesFull(
                CardanoDbBeacon::new("network", 5, 507)
            ))
        );

        let reference_hash = hash(SignedEntityType::CardanoTransactions(Epoch(35), 77));
        assert_ne!(
            reference_hash,
            hash(SignedEntityType::CardanoTransactions(Epoch(3), 77))
        );
        assert_ne!(
            reference_hash,
            hash(SignedEntityType::CardanoTransactions(Epoch(35), 98765))
        );
    }

    #[test]
    fn serialize_beacon_to_json() {
        let cardano_stake_distribution_json = SignedEntityType::CardanoStakeDistribution(Epoch(25))
            .get_json_beacon()
            .unwrap();
        assert_same_json!("25", &cardano_stake_distribution_json);

        let cardano_transactions_json = SignedEntityType::CardanoTransactions(Epoch(35), 77)
            .get_json_beacon()
            .unwrap();
        assert_same_json!(
            r#"{"epoch":35,"block_number":77}"#,
            &cardano_transactions_json
        );

        let cardano_immutable_files_full_json =
            SignedEntityType::CardanoImmutableFilesFull(CardanoDbBeacon::new("network", 5, 100))
                .get_json_beacon()
                .unwrap();
        assert_same_json!(
            r#"{"network":"network","epoch":5,"immutable_file_number":100}"#,
            &cardano_immutable_files_full_json
        );

        let msd_json = SignedEntityType::MithrilStakeDistribution(Epoch(15))
            .get_json_beacon()
            .unwrap();
        assert_same_json!("15", &msd_json);
    }

    // Expected ord:
    // MithrilStakeDistribution < CardanoStakeDistribution < CardanoImmutableFilesFull < CardanoTransactions
    #[test]
    fn ordering_discriminant() {
        let mut list = vec![
            SignedEntityTypeDiscriminants::CardanoStakeDistribution,
            SignedEntityTypeDiscriminants::CardanoTransactions,
            SignedEntityTypeDiscriminants::CardanoImmutableFilesFull,
            SignedEntityTypeDiscriminants::MithrilStakeDistribution,
        ];
        list.sort();

        assert_eq!(
            list,
            vec![
                SignedEntityTypeDiscriminants::MithrilStakeDistribution,
                SignedEntityTypeDiscriminants::CardanoStakeDistribution,
                SignedEntityTypeDiscriminants::CardanoImmutableFilesFull,
                SignedEntityTypeDiscriminants::CardanoTransactions,
            ]
        );
    }

    #[test]
    fn ordering_discriminant_with_duplicate() {
        let mut list = vec![
            SignedEntityTypeDiscriminants::CardanoStakeDistribution,
            SignedEntityTypeDiscriminants::MithrilStakeDistribution,
            SignedEntityTypeDiscriminants::CardanoTransactions,
            SignedEntityTypeDiscriminants::CardanoStakeDistribution,
            SignedEntityTypeDiscriminants::CardanoImmutableFilesFull,
            SignedEntityTypeDiscriminants::MithrilStakeDistribution,
            SignedEntityTypeDiscriminants::MithrilStakeDistribution,
        ];
        list.sort();

        assert_eq!(
            list,
            vec![
                SignedEntityTypeDiscriminants::MithrilStakeDistribution,
                SignedEntityTypeDiscriminants::MithrilStakeDistribution,
                SignedEntityTypeDiscriminants::MithrilStakeDistribution,
                SignedEntityTypeDiscriminants::CardanoStakeDistribution,
                SignedEntityTypeDiscriminants::CardanoStakeDistribution,
                SignedEntityTypeDiscriminants::CardanoImmutableFilesFull,
                SignedEntityTypeDiscriminants::CardanoTransactions,
            ]
        );
    }

    #[test]
    fn computing_block_number_to_be_signed() {
        // **block_number = ((tip.block_number - k') / n) × n**
        assert_eq!(
            CardanoTransactionsSigningConfig {
                security_parameter: 0,
                step: 15,
            }
            .compute_block_number_to_be_signed(105),
            105
        );

        assert_eq!(
            CardanoTransactionsSigningConfig {
                security_parameter: 5,
                step: 15,
            }
            .compute_block_number_to_be_signed(100),
            90
        );

        assert_eq!(
            CardanoTransactionsSigningConfig {
                security_parameter: 85,
                step: 15,
            }
            .compute_block_number_to_be_signed(100),
            15
        );

        assert_eq!(
            CardanoTransactionsSigningConfig {
                security_parameter: 0,
                step: 30,
            }
            .compute_block_number_to_be_signed(29),
            0
        );
    }

    #[test]
    fn computing_block_number_to_be_signed_round_step_to_a_block_range_start() {
        assert_eq!(
            CardanoTransactionsSigningConfig {
                security_parameter: 0,
                step: BlockRange::LENGTH * 2 - 1,
            }
            .compute_block_number_to_be_signed(BlockRange::LENGTH * 5 + 1),
            BlockRange::LENGTH * 5
        );

        assert_eq!(
            CardanoTransactionsSigningConfig {
                security_parameter: 0,
                step: BlockRange::LENGTH * 2 + 1,
            }
            .compute_block_number_to_be_signed(BlockRange::LENGTH * 5 + 1),
            BlockRange::LENGTH * 4
        );

        // Adjusted step is always at least BLOCK_RANGE_LENGTH.
        assert_eq!(
            CardanoTransactionsSigningConfig {
                security_parameter: 0,
                step: BlockRange::LENGTH - 1,
            }
            .compute_block_number_to_be_signed(BlockRange::LENGTH * 10 - 1),
            BlockRange::LENGTH * 9
        );

        assert_eq!(
            CardanoTransactionsSigningConfig {
                security_parameter: 0,
                step: BlockRange::LENGTH - 1,
            }
            .compute_block_number_to_be_signed(BlockRange::LENGTH - 1),
            0
        );
    }
}
