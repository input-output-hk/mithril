use serde::{Deserialize, Serialize};
use strum::{AsRefStr, Display, EnumDiscriminants, EnumIter};

use crate::entities::{
    BlockNumber, BlockNumberOffset, CardanoDbBeacon, Epoch, SignedEntityType,
    SignedEntityTypeDiscriminants,
};

/// The signed entity type that represents a type of data signed by the Mithril protocol.
///
// Important note: The order of the variants is important as it is used for the derived Ord trait.
#[derive(Display, Debug, Clone, PartialEq, Eq, Serialize, Deserialize, EnumDiscriminants)]
#[strum(serialize_all = "PascalCase")]
#[strum_discriminants(
    name(SignedEntityTypeDiscriminantsMessage),
    doc = "The discriminants of the SignedEntityTypeMessage enum.",
    derive(Display, AsRefStr, Serialize, Deserialize, PartialOrd, Ord, EnumIter,)
)]
pub enum SignedEntityTypeMessage {
    /// Mithril stake distribution
    MithrilStakeDistribution(Epoch),

    /// Cardano Stake Distribution
    CardanoStakeDistribution(Epoch),

    /// Full Cardano Immutable Files
    CardanoImmutableFilesFull(CardanoDbBeacon),

    /// Cardano Database
    CardanoDatabase(CardanoDbBeacon),

    /// Cardano Transactions
    CardanoTransactions(Epoch, BlockNumber),

    /// Cardano Blocks and Transactions
    CardanoBlocksTransactions(Epoch, BlockNumber, BlockNumberOffset),

    /// Unknown signed entity type, used to catch unknown values
    #[serde(other)]
    #[strum_discriminants(serde(other))]
    Unknown,
}

// Manual implementation instead of using strum::EnumString because it does not allow a "catch all"
// variant if that variant has no associated value (which is the case for derived discriminants).
impl From<&str> for SignedEntityTypeDiscriminantsMessage {
    fn from(value: &str) -> Self {
        match value {
            "MithrilStakeDistribution" => {
                SignedEntityTypeDiscriminantsMessage::MithrilStakeDistribution
            }
            "CardanoStakeDistribution" => {
                SignedEntityTypeDiscriminantsMessage::CardanoStakeDistribution
            }
            "CardanoImmutableFilesFull" => {
                SignedEntityTypeDiscriminantsMessage::CardanoImmutableFilesFull
            }
            "CardanoDatabase" => SignedEntityTypeDiscriminantsMessage::CardanoDatabase,
            "CardanoTransactions" => SignedEntityTypeDiscriminantsMessage::CardanoTransactions,
            "CardanoBlocksTransactions" => {
                SignedEntityTypeDiscriminantsMessage::CardanoBlocksTransactions
            }
            _ => SignedEntityTypeDiscriminantsMessage::Unknown,
        }
    }
}

impl From<SignedEntityType> for SignedEntityTypeMessage {
    fn from(value: SignedEntityType) -> Self {
        match value {
            SignedEntityType::MithrilStakeDistribution(epoch) => {
                SignedEntityTypeMessage::MithrilStakeDistribution(epoch)
            }
            SignedEntityType::CardanoStakeDistribution(epoch) => {
                SignedEntityTypeMessage::CardanoStakeDistribution(epoch)
            }
            SignedEntityType::CardanoImmutableFilesFull(beacon) => {
                SignedEntityTypeMessage::CardanoImmutableFilesFull(beacon)
            }
            SignedEntityType::CardanoDatabase(beacon) => {
                SignedEntityTypeMessage::CardanoDatabase(beacon)
            }
            SignedEntityType::CardanoTransactions(epoch, block) => {
                SignedEntityTypeMessage::CardanoTransactions(epoch, block)
            }
            SignedEntityType::CardanoBlocksTransactions(epoch, block, offset) => {
                SignedEntityTypeMessage::CardanoBlocksTransactions(epoch, block, offset)
            }
        }
    }
}

impl From<SignedEntityType> for SignedEntityTypeDiscriminantsMessage {
    fn from(value: SignedEntityType) -> Self {
        match value {
            SignedEntityType::MithrilStakeDistribution(..) => {
                SignedEntityTypeDiscriminantsMessage::MithrilStakeDistribution
            }
            SignedEntityType::CardanoStakeDistribution(..) => {
                SignedEntityTypeDiscriminantsMessage::CardanoStakeDistribution
            }
            SignedEntityType::CardanoImmutableFilesFull(..) => {
                SignedEntityTypeDiscriminantsMessage::CardanoImmutableFilesFull
            }
            SignedEntityType::CardanoDatabase(..) => {
                SignedEntityTypeDiscriminantsMessage::CardanoDatabase
            }
            SignedEntityType::CardanoTransactions(..) => {
                SignedEntityTypeDiscriminantsMessage::CardanoTransactions
            }
            SignedEntityType::CardanoBlocksTransactions(..) => {
                SignedEntityTypeDiscriminantsMessage::CardanoBlocksTransactions
            }
        }
    }
}

impl From<SignedEntityTypeDiscriminants> for SignedEntityTypeDiscriminantsMessage {
    fn from(value: SignedEntityTypeDiscriminants) -> Self {
        match value {
            SignedEntityTypeDiscriminants::MithrilStakeDistribution => {
                SignedEntityTypeDiscriminantsMessage::MithrilStakeDistribution
            }
            SignedEntityTypeDiscriminants::CardanoStakeDistribution => {
                SignedEntityTypeDiscriminantsMessage::CardanoStakeDistribution
            }
            SignedEntityTypeDiscriminants::CardanoImmutableFilesFull => {
                SignedEntityTypeDiscriminantsMessage::CardanoImmutableFilesFull
            }
            SignedEntityTypeDiscriminants::CardanoDatabase => {
                SignedEntityTypeDiscriminantsMessage::CardanoDatabase
            }
            SignedEntityTypeDiscriminants::CardanoTransactions => {
                SignedEntityTypeDiscriminantsMessage::CardanoTransactions
            }
            SignedEntityTypeDiscriminants::CardanoBlocksTransactions => {
                SignedEntityTypeDiscriminantsMessage::CardanoBlocksTransactions
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn infallible_conversion_cases() -> Vec<(
        SignedEntityType,
        SignedEntityTypeDiscriminants,
        SignedEntityTypeMessage,
        SignedEntityTypeDiscriminantsMessage,
    )> {
        vec![
            (
                SignedEntityType::MithrilStakeDistribution(Epoch(6)),
                SignedEntityTypeDiscriminants::MithrilStakeDistribution,
                SignedEntityTypeMessage::MithrilStakeDistribution(Epoch(6)),
                SignedEntityTypeDiscriminantsMessage::MithrilStakeDistribution,
            ),
            (
                SignedEntityType::CardanoStakeDistribution(Epoch(7)),
                SignedEntityTypeDiscriminants::CardanoStakeDistribution,
                SignedEntityTypeMessage::CardanoStakeDistribution(Epoch(7)),
                SignedEntityTypeDiscriminantsMessage::CardanoStakeDistribution,
            ),
            (
                SignedEntityType::CardanoImmutableFilesFull(CardanoDbBeacon::new(8, 109)),
                SignedEntityTypeDiscriminants::CardanoImmutableFilesFull,
                SignedEntityTypeMessage::CardanoImmutableFilesFull(CardanoDbBeacon::new(8, 109)),
                SignedEntityTypeDiscriminantsMessage::CardanoImmutableFilesFull,
            ),
            (
                SignedEntityType::CardanoDatabase(CardanoDbBeacon::new(9, 110)),
                SignedEntityTypeDiscriminants::CardanoDatabase,
                SignedEntityTypeMessage::CardanoDatabase(CardanoDbBeacon::new(9, 110)),
                SignedEntityTypeDiscriminantsMessage::CardanoDatabase,
            ),
            (
                SignedEntityType::CardanoTransactions(Epoch(10), BlockNumber(11)),
                SignedEntityTypeDiscriminants::CardanoTransactions,
                SignedEntityTypeMessage::CardanoTransactions(Epoch(10), BlockNumber(11)),
                SignedEntityTypeDiscriminantsMessage::CardanoTransactions,
            ),
            (
                SignedEntityType::CardanoBlocksTransactions(
                    Epoch(12),
                    BlockNumber(13),
                    BlockNumberOffset(14),
                ),
                SignedEntityTypeDiscriminants::CardanoBlocksTransactions,
                SignedEntityTypeMessage::CardanoBlocksTransactions(
                    Epoch(12),
                    BlockNumber(13),
                    BlockNumberOffset(14),
                ),
                SignedEntityTypeDiscriminantsMessage::CardanoBlocksTransactions,
            ),
        ]
    }

    mod infallible_conversions {
        use super::*;

        #[test]
        fn from_signed_entity_to_message() {
            for (signed_entity, _, message, _) in infallible_conversion_cases() {
                assert_eq!(SignedEntityTypeMessage::from(signed_entity), message);
            }
        }

        #[test]
        fn from_signed_entity_to_discriminant_message() {
            for (signed_entity, _, _, discriminant_message) in infallible_conversion_cases() {
                assert_eq!(
                    SignedEntityTypeDiscriminantsMessage::from(signed_entity),
                    discriminant_message,
                )
            }
        }

        #[test]
        fn from_message_entity_to_discriminant_message() {
            for (_, _, message_entity, discriminant_message) in infallible_conversion_cases() {
                assert_eq!(
                    SignedEntityTypeDiscriminantsMessage::from(message_entity),
                    discriminant_message,
                )
            }
        }
    }

    mod deserialization {
        use super::*;

        #[test]
        fn catch_unknown_signed_entity_type() {
            let discriminant: SignedEntityTypeMessage =
                serde_json::from_str(r#""not_exist""#).unwrap();
            assert_eq!(discriminant, SignedEntityTypeMessage::Unknown);
        }

        #[test]
        fn catch_unknown_signed_entity_type_discriminant() {
            let discriminant: SignedEntityTypeDiscriminantsMessage =
                serde_json::from_str(r#""not_exist""#).unwrap();
            assert_eq!(discriminant, SignedEntityTypeDiscriminantsMessage::Unknown);
        }
    }

    mod discriminant_from_str {
        use super::*;

        #[test]
        fn from_known_discriminants() {
            assert_eq!(
                SignedEntityTypeDiscriminantsMessage::from("MithrilStakeDistribution"),
                SignedEntityTypeDiscriminantsMessage::MithrilStakeDistribution
            );
            assert_eq!(
                SignedEntityTypeDiscriminantsMessage::from("CardanoStakeDistribution"),
                SignedEntityTypeDiscriminantsMessage::CardanoStakeDistribution
            );
            assert_eq!(
                SignedEntityTypeDiscriminantsMessage::from("CardanoImmutableFilesFull"),
                SignedEntityTypeDiscriminantsMessage::CardanoImmutableFilesFull
            );
            assert_eq!(
                SignedEntityTypeDiscriminantsMessage::from("CardanoDatabase"),
                SignedEntityTypeDiscriminantsMessage::CardanoDatabase
            );
            assert_eq!(
                SignedEntityTypeDiscriminantsMessage::from("CardanoTransactions"),
                SignedEntityTypeDiscriminantsMessage::CardanoTransactions
            );
            assert_eq!(
                SignedEntityTypeDiscriminantsMessage::from("CardanoBlocksTransactions"),
                SignedEntityTypeDiscriminantsMessage::CardanoBlocksTransactions
            );
            // special case included for completeness
            assert_eq!(
                SignedEntityTypeDiscriminantsMessage::from("Unknown"),
                SignedEntityTypeDiscriminantsMessage::Unknown
            );
        }

        #[test]
        fn unknown_signed_entity_type_discriminant() {
            let discriminant = SignedEntityTypeDiscriminantsMessage::from("not_exist");
            assert_eq!(discriminant, SignedEntityTypeDiscriminantsMessage::Unknown);
        }
    }
}
