use serde::{Deserialize, Serialize};
use std::cmp::Ordering;
use std::collections::BTreeSet;
use std::fmt::{Display, Formatter};

use crate::entities::{
    BlockNumber, BlockRange, CardanoDbBeacon, ChainPoint, Epoch, ImmutableFileNumber,
    SignedEntityType, SignedEntityTypeDiscriminants,
};
use crate::CardanoNetwork;

/// TimePoint aggregates all types of point in the Cardano chain and is used by the state machines
/// for their computations.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TimePoint {
    /// Cardano chain epoch number
    pub epoch: Epoch,

    /// Number of the last immutable files used for the digest computation
    pub immutable_file_number: ImmutableFileNumber,

    /// Chain point
    pub chain_point: ChainPoint,

    /// Parameters needed to convert this time point to signed entity types.
    pub signed_entity_conversion_config: SignedEntityConversionConfig,
}

impl TimePoint {
    /// [TimePoint] factory
    pub fn new(
        epoch: u64,
        immutable_file_number: ImmutableFileNumber,
        chain_point: ChainPoint,
        signed_entity_conversion_config: SignedEntityConversionConfig,
    ) -> TimePoint {
        TimePoint {
            epoch: Epoch(epoch),
            immutable_file_number,
            chain_point,
            signed_entity_conversion_config,
        }
    }

    cfg_test_tools! {
        /// Create a dummy TimePoint
        pub fn dummy() -> Self {
            Self::new(10, 100, ChainPoint::dummy(), SignedEntityConversionConfig::dummy())
        }
    }
}

impl PartialOrd for TimePoint {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for TimePoint {
    fn cmp(&self, other: &Self) -> Ordering {
        self.epoch
            .cmp(&other.epoch)
            .then(self.immutable_file_number.cmp(&other.immutable_file_number))
            .then(self.chain_point.cmp(&other.chain_point))
    }
}

impl Display for TimePoint {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "TimePoint (epoch: {}, immutable_file_number: {}, chain_point: {})",
            self.epoch, self.immutable_file_number, self.chain_point
        )
    }
}

/// Parameters needed to convert time points to signed entity types.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SignedEntityConversionConfig {
    /// List of discriminants that the node is allowed to sign
    pub allowed_discriminants: BTreeSet<SignedEntityTypeDiscriminants>,
    /// Cardano network
    pub network: CardanoNetwork,
    /// Cardano transactions signing configuration
    pub cardano_transactions_signing_config: CardanoTransactionsSigningConfig,
}

impl SignedEntityConversionConfig {
    cfg_test_tools! {
        /// Create a dummy SignedEntityConversionConfig
        pub fn dummy() -> Self {
            Self {
                allowed_discriminants: SignedEntityTypeDiscriminants::all(),
                network: CardanoNetwork::DevNet(12),
                cardano_transactions_signing_config: CardanoTransactionsSigningConfig::dummy(),
            }
        }
    }
}

/// Configuration for the signing of Cardano transactions
///
/// Allow to compute the block number to be signed based on the chain tip block number.
///
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct CardanoTransactionsSigningConfig {
    /// Number of blocks to discard from the tip of the chain when importing transactions.
    pub security_parameter: BlockNumber,

    /// The number of blocks between signature of the transactions.
    ///
    /// *Note: The step is adjusted to be a multiple of the block range length in order
    /// to guarantee that the block number signed in a certificate is effectively signed.*
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

    /// Compute the block number to be signed based on the chain tip block number.
    ///
    /// The latest block number to be signed is the highest multiple of the step less or equal than the
    /// block number minus the security parameter.
    ///
    /// The formula is as follows:
    ///
    /// `block_number = ⌊(tip.block_number - security_parameter) / step⌋ × step`
    ///
    /// where `⌊x⌋` is the floor function which rounds to the greatest integer less than or equal to `x`.
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

#[cfg(test)]
mod tests {
    use std::cmp::Ordering;

    use super::*;

    #[test]
    fn time_point_ord_cmp_epochs_take_precedence_over_other_fields() {
        let time_point1 = TimePoint {
            epoch: Epoch(5),
            immutable_file_number: 0,
            chain_point: ChainPoint {
                slot_number: 10,
                block_number: 20,
                block_hash: "hash1".to_string(),
            },
            ..TimePoint::dummy()
        };
        let time_point2 = TimePoint {
            epoch: Epoch(0),
            immutable_file_number: 1,
            chain_point: ChainPoint {
                slot_number: 15,
                block_number: 25,
                block_hash: "hash2".to_string(),
            },
            ..TimePoint::dummy()
        };

        assert_eq!(Ordering::Greater, time_point1.cmp(&time_point2));
    }

    #[test]
    fn time_point_ord_cmp_if_epoch_equals_then_immutable_take_precedence_over_chain_point() {
        let time_point1 = TimePoint {
            epoch: Epoch(0),
            immutable_file_number: 5,
            chain_point: ChainPoint {
                slot_number: 10,
                block_number: 20,
                block_hash: "hash1".to_string(),
            },
            ..TimePoint::dummy()
        };
        let time_point2 = TimePoint {
            epoch: Epoch(0),
            immutable_file_number: 0,
            chain_point: ChainPoint {
                slot_number: 15,
                block_number: 25,
                block_hash: "hash2".to_string(),
            },
            ..TimePoint::dummy()
        };

        assert_eq!(Ordering::Greater, time_point1.cmp(&time_point2));
    }

    #[test]
    fn time_point_ord_cmp_if_epoch_and_immutables_equals_then_compare_over_chain_points() {
        let time_point1 = TimePoint {
            epoch: Epoch(0),
            immutable_file_number: 0,
            chain_point: ChainPoint {
                slot_number: 10,
                block_number: 20,
                block_hash: "hash1".to_string(),
            },
            ..TimePoint::dummy()
        };
        let time_point2 = TimePoint {
            epoch: Epoch(0),
            immutable_file_number: 0,
            chain_point: ChainPoint {
                slot_number: 15,
                block_number: 25,
                block_hash: "hash2".to_string(),
            },
            ..TimePoint::dummy()
        };

        assert_eq!(Ordering::Less, time_point1.cmp(&time_point2));
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
