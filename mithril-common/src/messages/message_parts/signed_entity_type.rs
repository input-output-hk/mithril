use serde::{Deserialize, Serialize};
use strum::{AsRefStr, Display, EnumDiscriminants, EnumIter};

use crate::entities::{BlockNumber, BlockNumberOffset, CardanoDbBeacon, Epoch};

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

#[cfg(test)]
mod tests {
    use super::*;

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
