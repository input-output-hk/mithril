use std::collections::BTreeSet;

use serde::{Deserialize, Serialize};

use crate::StdResult;
use crate::entities::{
    BlockNumber, BlockRange, CardanoDbBeacon, SignedEntityType, SignedEntityTypeDiscriminants,
    TimePoint,
};

/// Convert [TimePoint] to [SignedEntityType] and list allowed signed entity types and
/// discriminants.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SignedEntityConfig {
    /// List of discriminants that the node is allowed to sign
    pub allowed_discriminants: BTreeSet<SignedEntityTypeDiscriminants>,
    /// Cardano transactions signing configuration
    pub cardano_transactions_signing_config: Option<CardanoTransactionsSigningConfig>,
    /// Cardano blocks and transactions signing configuration
    pub cardano_blocks_transactions_signing_config: Option<CardanoBlocksTransactionsSigningConfig>,
}

impl SignedEntityConfig {
    /// Default allowed discriminants
    ///
    /// Appended to the allowed discriminants in the configuration.
    pub const DEFAULT_ALLOWED_DISCRIMINANTS: [SignedEntityTypeDiscriminants; 1] =
        [SignedEntityTypeDiscriminants::MithrilStakeDistribution];

    /// Append to the given list of allowed signed entity types discriminants the [Self::DEFAULT_ALLOWED_DISCRIMINANTS]
    /// if not already present.
    pub fn append_allowed_signed_entity_types_discriminants(
        discriminants: BTreeSet<SignedEntityTypeDiscriminants>,
    ) -> BTreeSet<SignedEntityTypeDiscriminants> {
        let mut discriminants = discriminants;
        discriminants.append(&mut BTreeSet::from(Self::DEFAULT_ALLOWED_DISCRIMINANTS));
        discriminants
    }

    /// Create the deduplicated list of allowed signed entity types discriminants.
    ///
    /// The list is the aggregation of [Self::DEFAULT_ALLOWED_DISCRIMINANTS] and
    /// `allowed_discriminants`.
    pub fn list_allowed_signed_entity_types_discriminants(
        &self,
    ) -> BTreeSet<SignedEntityTypeDiscriminants> {
        let discriminants = self.allowed_discriminants.clone();
        Self::append_allowed_signed_entity_types_discriminants(discriminants)
    }

    /// Convert this time point to a signed entity type based on the given discriminant.
    pub fn time_point_to_signed_entity<D: Into<SignedEntityTypeDiscriminants>>(
        &self,
        discriminant: D,
        time_point: &TimePoint,
    ) -> StdResult<SignedEntityType> {
        let signed_entity_type = match discriminant.into() {
            SignedEntityTypeDiscriminants::MithrilStakeDistribution => {
                SignedEntityType::MithrilStakeDistribution(time_point.epoch)
            }
            SignedEntityTypeDiscriminants::CardanoStakeDistribution => {
                SignedEntityType::CardanoStakeDistribution(time_point.epoch.previous()?)
            }
            SignedEntityTypeDiscriminants::CardanoImmutableFilesFull => {
                SignedEntityType::CardanoImmutableFilesFull(CardanoDbBeacon::new(
                    *time_point.epoch,
                    time_point.immutable_file_number,
                ))
            }
            SignedEntityTypeDiscriminants::CardanoTransactions => {
                match &self.cardano_transactions_signing_config {
                    Some(config) => SignedEntityType::CardanoTransactions(
                        time_point.epoch,
                        config
                            .compute_block_number_to_be_signed(time_point.chain_point.block_number),
                    ),
                    None => {
                        anyhow::bail!(
                            "Can't derive a `CardanoTransactions` signed entity type from a time point without a `CardanoTransactionsSigningConfig`"
                        )
                    }
                }
            }
            SignedEntityTypeDiscriminants::CardanoBlocksTransactions => {
                match &self.cardano_blocks_transactions_signing_config {
                    Some(config) => SignedEntityType::CardanoBlocksTransactions(
                        time_point.epoch,
                        config
                            .compute_block_number_to_be_signed(time_point.chain_point.block_number),
                    ),
                    None => {
                        anyhow::bail!(
                            "Can't derive a `CardanoBlocksTransactions` signed entity type from a time point without a `CardanoBlocksTransactionsSigningConfig`"
                        )
                    }
                }
            }
            SignedEntityTypeDiscriminants::CardanoDatabase => SignedEntityType::CardanoDatabase(
                CardanoDbBeacon::new(*time_point.epoch, time_point.immutable_file_number),
            ),
        };

        Ok(signed_entity_type)
    }

    /// Create the deduplicated list of allowed signed entity types discriminants.
    ///
    /// The list is the aggregation of [Self::DEFAULT_ALLOWED_DISCRIMINANTS] and
    /// `allowed_discriminants`.
    pub fn list_allowed_signed_entity_types(
        &self,
        time_point: &TimePoint,
    ) -> StdResult<Vec<SignedEntityType>> {
        self.list_allowed_signed_entity_types_discriminants()
            .into_iter()
            .map(|discriminant| self.time_point_to_signed_entity(discriminant, time_point))
            .collect()
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
    /// Compute the block number to be signed based on the chain tip block number.
    ///
    pub fn compute_block_number_to_be_signed(&self, block_number: BlockNumber) -> BlockNumber {
        compute_block_number_to_be_signed(block_number, self.security_parameter, self.step)
    }
}

/// Configuration for the signing of Cardano blocks and transactions
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct CardanoBlocksTransactionsSigningConfig {
    /// Number of blocks to discard from the tip of the chain when importing blocks and transactions.
    pub security_parameter: BlockNumber,

    /// The number of blocks between signature of the blocks and transactions.
    ///
    /// *Note: The step is adjusted to be a multiple of the block range length in order
    /// to guarantee that the block number signed in a certificate is effectively signed.*
    pub step: BlockNumber,
}

impl CardanoBlocksTransactionsSigningConfig {
    /// Compute the block number to be signed based on the chain tip block number.
    ///
    pub fn compute_block_number_to_be_signed(&self, block_number: BlockNumber) -> BlockNumber {
        compute_block_number_to_be_signed(block_number, self.security_parameter, self.step)
    }
}

/// Compute the block number to be signed based on the chain tip block number.
///
/// The latest block number to be signed is the highest multiple of the step less or equal than the
/// block number minus the security parameter.
///
/// The formula is as follows:
///
/// `block_number = ⌊(tip.block_number - security_parameter) / step⌋ × step - 1`
///
/// where `⌊x⌋` is the floor function which rounds to the greatest integer less than or equal to `x`.
///
/// *Notes:*
/// * *The step is adjusted to be a multiple of the block range length in order
///   to guarantee that the block number signed in a certificate is effectively signed.*
/// * *1 is subtracted to the result because block range end is exclusive (ie: a BlockRange over
///   `30..45` finish at 44 included, 45 is included in the next block range).*
fn compute_block_number_to_be_signed(
    block_number: BlockNumber,
    security_parameter: BlockNumber,
    step: BlockNumber,
) -> BlockNumber {
    // TODO: See if we can remove this adjustment by including a "partial" block range in
    // the signed data.
    let adjusted_step = BlockRange::from_block_number(step).start;
    // We can't have a step lower than the block range length.
    let adjusted_step = std::cmp::max(adjusted_step, BlockRange::LENGTH);

    let block_number_to_be_signed =
        (block_number - security_parameter) / adjusted_step * adjusted_step;
    block_number_to_be_signed - 1
}

#[cfg(test)]
mod tests {
    use crate::entities::{
        CardanoDbBeacon, ChainPoint, Epoch, SignedEntityType, SlotNumber, TimePoint,
    };
    use crate::test::{double::Dummy, double::fake_data};

    use super::*;

    #[test]
    fn given_discriminant_convert_to_signed_entity() {
        let time_point = TimePoint {
            epoch: Epoch(1),
            immutable_file_number: 5,
            chain_point: ChainPoint {
                slot_number: SlotNumber(73),
                block_number: BlockNumber(20),
                block_hash: "block_hash-20".to_string(),
            },
        };
        let config = SignedEntityConfig {
            allowed_discriminants: SignedEntityTypeDiscriminants::all(),
            cardano_transactions_signing_config: Some(CardanoTransactionsSigningConfig {
                security_parameter: BlockNumber(0),
                step: BlockNumber(15),
            }),
            cardano_blocks_transactions_signing_config: Some(
                CardanoBlocksTransactionsSigningConfig {
                    security_parameter: BlockNumber(1),
                    step: BlockNumber(30),
                },
            ),
        };

        assert_eq!(
            SignedEntityType::MithrilStakeDistribution(Epoch(1)),
            config
                .time_point_to_signed_entity(
                    SignedEntityTypeDiscriminants::MithrilStakeDistribution,
                    &time_point
                )
                .unwrap()
        );

        // An offset of -1 is applied to the epoch of the time point to get the epoch of the stake distribution to be signed
        assert_eq!(
            SignedEntityType::CardanoStakeDistribution(Epoch(0)),
            config
                .time_point_to_signed_entity(
                    SignedEntityTypeDiscriminants::CardanoStakeDistribution,
                    &time_point
                )
                .unwrap()
        );

        assert_eq!(
            SignedEntityType::CardanoImmutableFilesFull(CardanoDbBeacon::new(1, 5)),
            config
                .time_point_to_signed_entity(
                    SignedEntityTypeDiscriminants::CardanoImmutableFilesFull,
                    &time_point
                )
                .unwrap()
        );

        // The block number to be signed is 14 because the step is 15, the block number is 20, and
        // the security parameter is 0.
        // This is further tested in the "computing_block_number_to_be_signed" tests below.
        assert_eq!(
            SignedEntityType::CardanoTransactions(Epoch(1), BlockNumber(14)),
            config
                .time_point_to_signed_entity(
                    SignedEntityTypeDiscriminants::CardanoTransactions,
                    &time_point
                )
                .unwrap()
        );

        assert_eq!(
            SignedEntityType::CardanoDatabase(CardanoDbBeacon::new(1, 5)),
            config
                .time_point_to_signed_entity(
                    SignedEntityTypeDiscriminants::CardanoDatabase,
                    &time_point
                )
                .unwrap()
        );
    }

    #[test]
    fn can_not_convert_time_point_to_cardano_transaction_without_the_associated_config() {
        let time_point = TimePoint {
            epoch: Epoch(1),
            immutable_file_number: 5,
            chain_point: ChainPoint {
                slot_number: SlotNumber(73),
                block_number: BlockNumber(20),
                block_hash: "block_hash-20".to_string(),
            },
        };
        let config = SignedEntityConfig {
            allowed_discriminants: SignedEntityTypeDiscriminants::all(),
            cardano_transactions_signing_config: None,
            cardano_blocks_transactions_signing_config: None,
        };

        let error = config
            .time_point_to_signed_entity(
                SignedEntityTypeDiscriminants::CardanoTransactions,
                &time_point,
            )
            .unwrap_err();

        let expected_error = "Can't derive a `CardanoTransactions` signed entity type from a time point without a `CardanoTransactionsSigningConfig`";
        assert!(
            error.to_string().contains(expected_error),
            "Error message: {error:?}\nshould contains: {expected_error}\n"
        );
    }

    #[test]
    fn computing_block_number_to_be_signed() {
        // **block_number = ((tip.block_number - k') / n) × n**
        let block_number = BlockNumber(105);
        let security_parameter = BlockNumber(0);
        let step = BlockNumber(15);
        assert_eq!(
            compute_block_number_to_be_signed(block_number, security_parameter, step),
            104
        );

        let block_number = BlockNumber(100);
        let security_parameter = BlockNumber(5);
        let step = BlockNumber(15);
        assert_eq!(
            compute_block_number_to_be_signed(block_number, security_parameter, step),
            89
        );

        let block_number = BlockNumber(100);
        let security_parameter = BlockNumber(85);
        let step = BlockNumber(15);
        assert_eq!(
            compute_block_number_to_be_signed(block_number, security_parameter, step),
            14
        );

        let block_number = BlockNumber(29);
        let security_parameter = BlockNumber(0);
        let step = BlockNumber(30);
        assert_eq!(
            compute_block_number_to_be_signed(block_number, security_parameter, step),
            0
        );
    }

    #[test]
    fn computing_block_number_to_be_signed_should_not_overlow_on_security_parameter() {
        let block_number = BlockNumber(50);
        let security_parameter = BlockNumber(100);
        let step = BlockNumber(30);
        assert_eq!(
            compute_block_number_to_be_signed(block_number, security_parameter, step),
            0
        );
    }

    #[test]
    fn computing_block_number_to_be_signed_round_step_to_a_block_range_start() {
        let block_number = BlockRange::LENGTH * 5 + 1;
        let security_parameter = BlockNumber(0);
        let step = BlockRange::LENGTH * 2 - 1;
        assert_eq!(
            compute_block_number_to_be_signed(block_number, security_parameter, step),
            BlockRange::LENGTH * 5 - 1
        );

        let block_number = BlockRange::LENGTH * 5 + 1;
        let security_parameter = BlockNumber(0);
        let step = BlockRange::LENGTH * 2 + 1;
        assert_eq!(
            compute_block_number_to_be_signed(block_number, security_parameter, step),
            BlockRange::LENGTH * 4 - 1
        );

        // Adjusted step is always at least BLOCK_RANGE_LENGTH.
        let block_number = BlockRange::LENGTH * 10 - 1;
        let security_parameter = BlockNumber(0);
        let step = BlockRange::LENGTH - 1;
        assert_eq!(
            compute_block_number_to_be_signed(block_number, security_parameter, step),
            BlockRange::LENGTH * 9 - 1
        );

        // Adjusted step is always at least BLOCK_RANGE_LENGTH.
        let block_number = BlockRange::LENGTH - 1;
        let security_parameter = BlockNumber(0);
        let step = BlockRange::LENGTH - 1;
        assert_eq!(
            compute_block_number_to_be_signed(block_number, security_parameter, step),
            0
        );
    }

    #[test]
    fn test_list_allowed_signed_entity_types_discriminant_without_specific_configuration() {
        let config = SignedEntityConfig {
            allowed_discriminants: BTreeSet::new(),
            ..SignedEntityConfig::dummy()
        };

        let discriminants = config.list_allowed_signed_entity_types_discriminants();

        assert_eq!(
            BTreeSet::from(SignedEntityConfig::DEFAULT_ALLOWED_DISCRIMINANTS),
            discriminants
        );
    }

    #[test]
    fn test_list_allowed_signed_entity_types_discriminant_should_not_duplicate_a_signed_entity_discriminant_type_already_in_default_ones()
     {
        let config = SignedEntityConfig {
            allowed_discriminants: BTreeSet::from([
                SignedEntityConfig::DEFAULT_ALLOWED_DISCRIMINANTS[0],
            ]),
            ..SignedEntityConfig::dummy()
        };

        let discriminants = config.list_allowed_signed_entity_types_discriminants();

        assert_eq!(
            BTreeSet::from(SignedEntityConfig::DEFAULT_ALLOWED_DISCRIMINANTS),
            discriminants
        );
    }

    #[test]
    fn test_list_allowed_signed_entity_types_discriminants_should_add_configured_discriminants() {
        let config = SignedEntityConfig {
            allowed_discriminants: BTreeSet::from([
                SignedEntityTypeDiscriminants::CardanoStakeDistribution,
                SignedEntityTypeDiscriminants::CardanoTransactions,
                SignedEntityTypeDiscriminants::CardanoDatabase,
            ]),
            ..SignedEntityConfig::dummy()
        };

        let discriminants = config.list_allowed_signed_entity_types_discriminants();

        assert_eq!(
            BTreeSet::from_iter(
                [
                    SignedEntityConfig::DEFAULT_ALLOWED_DISCRIMINANTS.as_slice(),
                    [
                        SignedEntityTypeDiscriminants::CardanoStakeDistribution,
                        SignedEntityTypeDiscriminants::CardanoTransactions,
                        SignedEntityTypeDiscriminants::CardanoDatabase
                    ]
                    .as_slice()
                ]
                .concat()
            ),
            discriminants
        );
    }

    #[test]
    fn test_list_allowed_signed_entity_types_discriminants_with_multiple_identical_signed_entity_types_in_configuration_should_not_be_added_several_times()
     {
        let config = SignedEntityConfig {
            allowed_discriminants: BTreeSet::from([
                SignedEntityTypeDiscriminants::CardanoTransactions,
                SignedEntityTypeDiscriminants::CardanoTransactions,
                SignedEntityTypeDiscriminants::CardanoTransactions,
            ]),
            ..SignedEntityConfig::dummy()
        };

        let discriminants = config.list_allowed_signed_entity_types_discriminants();

        assert_eq!(
            BTreeSet::from_iter(
                [
                    SignedEntityConfig::DEFAULT_ALLOWED_DISCRIMINANTS.as_slice(),
                    [SignedEntityTypeDiscriminants::CardanoTransactions].as_slice()
                ]
                .concat()
            ),
            discriminants
        );
    }

    #[test]
    fn test_list_allowed_signed_entity_types_with_specific_configuration() {
        let beacon = fake_data::beacon();
        let chain_point = ChainPoint {
            block_number: BlockNumber(45),
            ..ChainPoint::dummy()
        };
        let time_point = TimePoint::new(
            *beacon.epoch,
            beacon.immutable_file_number,
            chain_point.clone(),
        );
        let config = SignedEntityConfig {
            allowed_discriminants: BTreeSet::from([
                SignedEntityTypeDiscriminants::CardanoStakeDistribution,
                SignedEntityTypeDiscriminants::CardanoTransactions,
                SignedEntityTypeDiscriminants::CardanoBlocksTransactions,
            ]),
            cardano_transactions_signing_config: Some(CardanoTransactionsSigningConfig {
                security_parameter: BlockNumber(0),
                step: BlockNumber(15),
            }),
            cardano_blocks_transactions_signing_config: Some(
                CardanoBlocksTransactionsSigningConfig {
                    security_parameter: BlockNumber(0),
                    step: BlockNumber(15),
                },
            ),
        };

        let signed_entity_types = config.list_allowed_signed_entity_types(&time_point).unwrap();

        assert_eq!(
            vec![
                SignedEntityType::MithrilStakeDistribution(beacon.epoch),
                SignedEntityType::CardanoStakeDistribution(beacon.epoch - 1),
                SignedEntityType::CardanoTransactions(beacon.epoch, chain_point.block_number - 1),
                SignedEntityType::CardanoBlocksTransactions(
                    beacon.epoch,
                    chain_point.block_number - 1
                ),
            ],
            signed_entity_types
        );
    }
}
