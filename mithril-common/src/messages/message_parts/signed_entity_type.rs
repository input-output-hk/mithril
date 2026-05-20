use serde::{Deserialize, Serialize};
use std::collections::BTreeSet;
use strum::{AsRefStr, Display, EnumDiscriminants, EnumIter};
use thiserror::Error;

use crate::entities::{
    BlockNumber, BlockNumberOffset, CardanoDbBeacon, Epoch, SignedEntityType,
    SignedEntityTypeDiscriminants,
};

/// The signed entity type that represents a type of data signed by the Mithril protocol.
///
// Important note: The order of the variants is important as it is used for the derived Ord trait
// of the `SignedEntityTypeDiscriminantsMessage` enum.
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

impl SignedEntityTypeMessage {
    /// Checks if the current instance of `SignedEntityTypeMessage` is a known type.
    pub fn is_known(&self) -> bool {
        !matches!(self, SignedEntityTypeMessage::Unknown)
    }

    /// Converts the message into a [SignedEntityType].
    ///
    /// Returns `None` for unknown values.
    pub fn into_entity(self) -> Option<SignedEntityType> {
        match self {
            SignedEntityTypeMessage::MithrilStakeDistribution(epoch) => {
                Some(SignedEntityType::MithrilStakeDistribution(epoch))
            }
            SignedEntityTypeMessage::CardanoStakeDistribution(epoch) => {
                Some(SignedEntityType::CardanoStakeDistribution(epoch))
            }
            SignedEntityTypeMessage::CardanoImmutableFilesFull(beacon) => {
                Some(SignedEntityType::CardanoImmutableFilesFull(beacon))
            }
            SignedEntityTypeMessage::CardanoDatabase(beacon) => {
                Some(SignedEntityType::CardanoDatabase(beacon))
            }
            SignedEntityTypeMessage::CardanoTransactions(epoch, block) => {
                Some(SignedEntityType::CardanoTransactions(epoch, block))
            }
            SignedEntityTypeMessage::CardanoBlocksTransactions(epoch, block, offset) => Some(
                SignedEntityType::CardanoBlocksTransactions(epoch, block, offset),
            ),
            SignedEntityTypeMessage::Unknown => None,
        }
    }
}

impl SignedEntityTypeDiscriminantsMessage {
    /// Get all the discriminants without unstable values
    pub fn all() -> BTreeSet<Self> {
        // Leverage the list from `SignedEntityTypeDiscriminants` to avoid Unknown and duplicating the filter
        SignedEntityTypeDiscriminants::all()
            .into_iter()
            .map(Self::from)
            .collect()
    }

    /// Checks if the current instance of `SignedEntityTypeDiscriminantsMessage` is a known type.
    pub fn is_known(&self) -> bool {
        !matches!(self, SignedEntityTypeDiscriminantsMessage::Unknown)
    }

    /// Converts the message into a [SignedEntityTypeDiscriminants].
    ///
    /// Returns `None` for unknown values.
    pub fn into_discriminant(self) -> Option<SignedEntityTypeDiscriminants> {
        match self {
            SignedEntityTypeDiscriminantsMessage::MithrilStakeDistribution => {
                Some(SignedEntityTypeDiscriminants::MithrilStakeDistribution)
            }
            SignedEntityTypeDiscriminantsMessage::CardanoStakeDistribution => {
                Some(SignedEntityTypeDiscriminants::CardanoStakeDistribution)
            }
            SignedEntityTypeDiscriminantsMessage::CardanoImmutableFilesFull => {
                Some(SignedEntityTypeDiscriminants::CardanoImmutableFilesFull)
            }
            SignedEntityTypeDiscriminantsMessage::CardanoDatabase => {
                Some(SignedEntityTypeDiscriminants::CardanoDatabase)
            }
            SignedEntityTypeDiscriminantsMessage::CardanoTransactions => {
                Some(SignedEntityTypeDiscriminants::CardanoTransactions)
            }
            SignedEntityTypeDiscriminantsMessage::CardanoBlocksTransactions => {
                Some(SignedEntityTypeDiscriminants::CardanoBlocksTransactions)
            }
            SignedEntityTypeDiscriminantsMessage::Unknown => None,
        }
    }

    /// Convert an iterator of `SignedEntityTypeDiscriminantsMessage` into an iterator of `SignedEntityTypeDiscriminants`
    ///
    /// Instead of failing, any [SignedEntityTypeDiscriminantsMessage::Unknown] will be discarded
    pub fn into_known_discriminants<
        T: IntoIterator<Item = Self>,
        B: FromIterator<SignedEntityTypeDiscriminants>,
    >(
        iter: T,
    ) -> B {
        iter.into_iter()
            .filter_map(|message| message.into_discriminant())
            .collect()
    }
}

mod infallible_conversions {
    use super::*;

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
}

/// Raised when trying to convert a [SignedEntityTypeMessage] to a [SignedEntityType] or a
/// [SignedEntityTypeDiscriminantsMessage] to a [SignedEntityTypeDiscriminants] and the
/// parsed value is unknown.
///
/// Note: this is the only error that can be raised by these conversions.
#[derive(Debug, PartialEq, Eq, Error)]
#[error("Unknown signed entity type")]
pub struct UnknownSignedEntityTypeError;

mod fallible_conversions {
    use super::*;

    impl TryFrom<SignedEntityTypeMessage> for SignedEntityType {
        type Error = UnknownSignedEntityTypeError;

        fn try_from(value: SignedEntityTypeMessage) -> Result<Self, Self::Error> {
            value.into_entity().ok_or(UnknownSignedEntityTypeError)
        }
    }

    impl TryFrom<SignedEntityTypeMessage> for SignedEntityTypeDiscriminants {
        type Error = UnknownSignedEntityTypeError;

        fn try_from(value: SignedEntityTypeMessage) -> Result<Self, Self::Error> {
            match value {
                SignedEntityTypeMessage::MithrilStakeDistribution(..) => {
                    Ok(SignedEntityTypeDiscriminants::MithrilStakeDistribution)
                }
                SignedEntityTypeMessage::CardanoStakeDistribution(..) => {
                    Ok(SignedEntityTypeDiscriminants::CardanoStakeDistribution)
                }
                SignedEntityTypeMessage::CardanoImmutableFilesFull(..) => {
                    Ok(SignedEntityTypeDiscriminants::CardanoImmutableFilesFull)
                }
                SignedEntityTypeMessage::CardanoDatabase(..) => {
                    Ok(SignedEntityTypeDiscriminants::CardanoDatabase)
                }
                SignedEntityTypeMessage::CardanoTransactions(..) => {
                    Ok(SignedEntityTypeDiscriminants::CardanoTransactions)
                }
                SignedEntityTypeMessage::CardanoBlocksTransactions(..) => {
                    Ok(SignedEntityTypeDiscriminants::CardanoBlocksTransactions)
                }
                SignedEntityTypeMessage::Unknown => Err(UnknownSignedEntityTypeError),
            }
        }
    }

    impl TryFrom<SignedEntityTypeDiscriminantsMessage> for SignedEntityTypeDiscriminants {
        type Error = UnknownSignedEntityTypeError;

        fn try_from(value: SignedEntityTypeDiscriminantsMessage) -> Result<Self, Self::Error> {
            value.into_discriminant().ok_or(UnknownSignedEntityTypeError)
        }
    }
}

mod comparison {
    use super::*;

    impl PartialEq<SignedEntityTypeMessage> for SignedEntityType {
        fn eq(&self, other: &SignedEntityTypeMessage) -> bool {
            match (self, other) {
                (
                    SignedEntityType::MithrilStakeDistribution(epoch),
                    SignedEntityTypeMessage::MithrilStakeDistribution(other_epoch),
                ) => epoch.eq(&other_epoch),
                (
                    SignedEntityType::CardanoStakeDistribution(epoch),
                    SignedEntityTypeMessage::CardanoStakeDistribution(other_epoch),
                ) => epoch.eq(&other_epoch),
                (
                    SignedEntityType::CardanoImmutableFilesFull(beacon),
                    SignedEntityTypeMessage::CardanoImmutableFilesFull(other_beacon),
                ) => beacon.eq(other_beacon),
                (
                    SignedEntityType::CardanoDatabase(beacon),
                    SignedEntityTypeMessage::CardanoDatabase(other_beacon),
                ) => beacon.eq(other_beacon),
                (
                    SignedEntityType::CardanoTransactions(epoch, block),
                    SignedEntityTypeMessage::CardanoTransactions(other_epoch, other_block),
                ) => epoch.eq(&other_epoch) && block.eq(&other_block),
                (
                    SignedEntityType::CardanoBlocksTransactions(epoch, block, offset),
                    SignedEntityTypeMessage::CardanoBlocksTransactions(
                        other_epoch,
                        other_block,
                        other_offset,
                    ),
                ) => epoch.eq(&other_epoch) && block.eq(&other_block) && offset.eq(&other_offset),
                _ => false,
            }
        }
    }

    impl PartialEq<SignedEntityType> for SignedEntityTypeMessage {
        fn eq(&self, other: &SignedEntityType) -> bool {
            other.eq(self)
        }
    }

    impl PartialEq<SignedEntityTypeDiscriminantsMessage> for SignedEntityTypeDiscriminants {
        fn eq(&self, other: &SignedEntityTypeDiscriminantsMessage) -> bool {
            matches!(
                (self, other),
                (
                    SignedEntityTypeDiscriminants::MithrilStakeDistribution,
                    SignedEntityTypeDiscriminantsMessage::MithrilStakeDistribution,
                ) | (
                    SignedEntityTypeDiscriminants::CardanoStakeDistribution,
                    SignedEntityTypeDiscriminantsMessage::CardanoStakeDistribution,
                ) | (
                    SignedEntityTypeDiscriminants::CardanoImmutableFilesFull,
                    SignedEntityTypeDiscriminantsMessage::CardanoImmutableFilesFull,
                ) | (
                    SignedEntityTypeDiscriminants::CardanoDatabase,
                    SignedEntityTypeDiscriminantsMessage::CardanoDatabase,
                ) | (
                    SignedEntityTypeDiscriminants::CardanoTransactions,
                    SignedEntityTypeDiscriminantsMessage::CardanoTransactions,
                ) | (
                    SignedEntityTypeDiscriminants::CardanoBlocksTransactions,
                    SignedEntityTypeDiscriminantsMessage::CardanoBlocksTransactions,
                )
            )
        }
    }

    impl PartialEq<SignedEntityTypeDiscriminants> for SignedEntityTypeDiscriminantsMessage {
        fn eq(&self, other: &SignedEntityTypeDiscriminants) -> bool {
            other.eq(self)
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::assert_equivalent;
    use crate::test::entities_extensions::SignedEntityTypeDiscriminantsTestExtension;

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

    #[test]
    fn infallible_conversion_cases_are_exhaustive() {
        let discriminants: Vec<_> = infallible_conversion_cases()
            .into_iter()
            .map(|(_, discriminant, _, _)| discriminant)
            .collect();

        assert_equivalent!(
            SignedEntityTypeDiscriminants::all_with_unstable_vec(),
            discriminants
        );
    }

    #[test]
    fn all_returns_known_discriminant_messages() {
        assert_eq!(
            SignedEntityTypeDiscriminants::all()
                .into_iter()
                .map(From::from)
                .collect::<BTreeSet<_>>(),
            SignedEntityTypeDiscriminantsMessage::all()
        );
        assert!(
            !SignedEntityTypeDiscriminantsMessage::all()
                .contains(&SignedEntityTypeDiscriminantsMessage::Unknown)
        );
    }

    #[test]
    fn is_known_returns_true_for_known_messages() {
        let (known_message, known_discriminants_message): (Vec<_>, Vec<_>) =
            infallible_conversion_cases()
                .into_iter()
                .map(|(_, _, msg, discriminant_msg)| (msg, discriminant_msg))
                .unzip();

        assert!(known_message.iter().all(|message| message.is_known()));
        assert!(
            known_discriminants_message
                .iter()
                .all(|discriminant| discriminant.is_known())
        );
    }

    #[test]
    fn is_known_returns_false_for_unknown_variants() {
        assert!(!SignedEntityTypeMessage::Unknown.is_known());
        assert!(!SignedEntityTypeDiscriminantsMessage::Unknown.is_known());
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
        fn from_signed_entity_discriminant_to_discriminant_message() {
            for (_, discriminant, _, discriminant_message) in infallible_conversion_cases() {
                assert_eq!(
                    SignedEntityTypeDiscriminantsMessage::from(discriminant),
                    discriminant_message,
                )
            }
        }

        #[test]
        fn from_signed_entity_type_message_to_discriminant_message() {
            for (_, _, message_entity, discriminant_message) in infallible_conversion_cases() {
                assert_eq!(
                    SignedEntityTypeDiscriminantsMessage::from(message_entity),
                    discriminant_message,
                )
            }
        }

        #[test]
        fn into_entity_returns_some_for_known_messages() {
            for (entity, _, message, _) in infallible_conversion_cases() {
                assert_eq!(Some(entity), message.into_entity())
            }

            assert_eq!(None, SignedEntityTypeMessage::Unknown.into_entity());
        }

        #[test]
        fn into_discriminant_returns_some_for_known_messages() {
            for (_, discriminant, _, discriminant_message) in infallible_conversion_cases() {
                assert_eq!(Some(discriminant), discriminant_message.into_discriminant())
            }

            assert_eq!(
                None,
                SignedEntityTypeDiscriminantsMessage::Unknown.into_discriminant()
            );
        }
    }

    mod fallible_conversions {
        use super::*;

        #[test]
        fn try_from_message_to_entity_succeeds_for_known_values() {
            for (signed_entity, _, message, _) in infallible_conversion_cases() {
                assert_eq!(SignedEntityType::try_from(message).unwrap(), signed_entity);
            }
        }

        #[test]
        fn try_from_message_to_discriminant_succeeds_for_known_values() {
            for (_, discriminant, message, _) in infallible_conversion_cases() {
                assert_eq!(
                    SignedEntityTypeDiscriminants::try_from(message).unwrap(),
                    discriminant,
                )
            }
        }

        #[test]
        fn try_from_discriminant_message_succeeds_for_known_values() {
            for (_, discriminant, _, discriminant_message) in infallible_conversion_cases() {
                assert_eq!(
                    SignedEntityTypeDiscriminants::try_from(discriminant_message).unwrap(),
                    discriminant,
                )
            }
        }

        #[test]
        fn try_from_message_to_entity_fails_for_unknown_values() {
            assert_eq!(
                SignedEntityType::try_from(SignedEntityTypeMessage::Unknown).unwrap_err(),
                UnknownSignedEntityTypeError
            );
        }

        #[test]
        fn try_from_entity_message_to_discriminant_fails_for_unknown_values() {
            assert_eq!(
                SignedEntityTypeDiscriminants::try_from(SignedEntityTypeMessage::Unknown)
                    .unwrap_err(),
                UnknownSignedEntityTypeError
            );
        }

        #[test]
        fn try_from_discriminant_message_to_discriminant_fails_for_unknown_values() {
            assert_eq!(
                SignedEntityTypeDiscriminants::try_from(
                    SignedEntityTypeDiscriminantsMessage::Unknown
                )
                .unwrap_err(),
                UnknownSignedEntityTypeError
            );
        }
    }

    mod deserialization {
        use super::*;

        #[test]
        fn catch_unknown_signed_entity_type() {
            let message: SignedEntityTypeMessage = serde_json::from_str(r#""not_exist""#).unwrap();
            assert_eq!(message, SignedEntityTypeMessage::Unknown);
        }

        #[test]
        fn catch_unknown_signed_entity_type_discriminant() {
            let message: SignedEntityTypeDiscriminantsMessage =
                serde_json::from_str(r#""not_exist""#).unwrap();
            assert_eq!(message, SignedEntityTypeDiscriminantsMessage::Unknown);
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

    mod convert_iterable_into_known_discriminants {
        use super::*;

        #[test]
        fn converts_all_known_discriminants_message() {
            let discriminants_message: Vec<_> = infallible_conversion_cases()
                .into_iter()
                .map(|(_, _, _, message)| message)
                .collect();

            let converted: Vec<SignedEntityTypeDiscriminants> =
                SignedEntityTypeDiscriminantsMessage::into_known_discriminants(
                    discriminants_message,
                );
            assert_eq!(
                converted,
                SignedEntityTypeDiscriminants::all_with_unstable_vec()
            );
        }

        #[test]
        fn discards_unknown_discriminants() {
            let discriminants: Vec<SignedEntityTypeDiscriminants> =
                SignedEntityTypeDiscriminantsMessage::into_known_discriminants(vec![
                    SignedEntityTypeDiscriminantsMessage::Unknown,
                    SignedEntityTypeDiscriminantsMessage::MithrilStakeDistribution,
                    SignedEntityTypeDiscriminantsMessage::Unknown,
                    SignedEntityTypeDiscriminantsMessage::CardanoTransactions,
                ]);

            assert_eq!(
                vec![
                    SignedEntityTypeDiscriminants::MithrilStakeDistribution,
                    SignedEntityTypeDiscriminants::CardanoTransactions,
                ],
                discriminants
            );
        }

        #[test]
        fn returns_empty_collection_when_input_is_empty() {
            let discriminants: Vec<SignedEntityTypeDiscriminants> =
                SignedEntityTypeDiscriminantsMessage::into_known_discriminants(vec![]);

            assert_eq!(Vec::<SignedEntityTypeDiscriminants>::new(), discriminants);
        }

        #[test]
        fn returns_empty_collection_when_input_contains_only_unknown_discriminants() {
            let discriminants: Vec<SignedEntityTypeDiscriminants> =
                SignedEntityTypeDiscriminantsMessage::into_known_discriminants(vec![
                    SignedEntityTypeDiscriminantsMessage::Unknown,
                    SignedEntityTypeDiscriminantsMessage::Unknown,
                ]);

            assert_eq!(Vec::<SignedEntityTypeDiscriminants>::new(), discriminants);
        }
    }

    mod comparison {
        use super::*;

        fn alter_entity(entity: &SignedEntityType) -> SignedEntityType {
            match entity.clone() {
                SignedEntityType::MithrilStakeDistribution(epoch) => {
                    SignedEntityType::MithrilStakeDistribution(epoch + 1)
                }
                SignedEntityType::CardanoStakeDistribution(epoch) => {
                    SignedEntityType::CardanoStakeDistribution(epoch + 1)
                }
                SignedEntityType::CardanoImmutableFilesFull(beacon) => {
                    SignedEntityType::CardanoImmutableFilesFull(CardanoDbBeacon::new(
                        *beacon.epoch + 1,
                        beacon.immutable_file_number + 5,
                    ))
                }
                SignedEntityType::CardanoDatabase(beacon) => SignedEntityType::CardanoDatabase(
                    CardanoDbBeacon::new(*beacon.epoch + 1, beacon.immutable_file_number + 5),
                ),
                SignedEntityType::CardanoTransactions(epoch, block) => {
                    SignedEntityType::CardanoTransactions(epoch + 1, block + 1)
                }
                SignedEntityType::CardanoBlocksTransactions(epoch, block, offset) => {
                    SignedEntityType::CardanoBlocksTransactions(epoch + 1, block + 1, offset + 1)
                }
            }
        }

        #[test]
        fn discriminant_message_equals_discriminant_and_reverse() {
            for (_, discriminant, _, discriminant_message) in infallible_conversion_cases() {
                assert_eq!(discriminant_message, discriminant);
                assert_eq!(discriminant, discriminant_message);
            }
        }

        #[test]
        fn message_equals_entity_and_reverse() {
            for (entity, _, message, _) in infallible_conversion_cases() {
                assert_eq!(message, entity);
                assert_eq!(entity, message);
                assert_ne!(message, alter_entity(&entity));
                assert_ne!(alter_entity(&entity), message);
            }
        }

        #[test]
        fn unknown_messages_do_not_equal_entities() {
            for (entity, discriminant, _, _) in infallible_conversion_cases() {
                assert_ne!(entity, SignedEntityTypeMessage::Unknown);
                assert_ne!(SignedEntityTypeMessage::Unknown, entity);
                assert_ne!(discriminant, SignedEntityTypeDiscriminantsMessage::Unknown);
                assert_ne!(SignedEntityTypeDiscriminantsMessage::Unknown, discriminant);
            }
        }
    }
}
