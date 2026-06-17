use std::collections::BTreeSet;

use serde::de::IgnoredAny;
use serde::{Deserialize, Deserializer, Serialize};
use strum::{Display, EnumIter};

use crate::entities::{SignedEntityType, SignedEntityTypeDiscriminants};

/// Signed entity type received through the API.
///
/// This message preserves backward compatibility by distinguishing current,
/// discontinued, and unknown signed entity types.
#[derive(Debug, Clone, PartialEq, Serialize)]
#[serde(untagged)]
pub enum SignedEntityTypeMessage {
    /// A currently supported signed entity type.
    Known(SignedEntityType),

    /// A signed entity type that is no longer supported.
    Discontinued(DiscontinuedSignedEntityTypeMessage),

    /// An unrecognized signed entity type.
    Unknown,
}

/// Signed entity types that were supported by previous API versions.
///
/// These values are kept to deserialize historical messages without treating
/// them as unknown.
#[derive(
    Debug, Copy, Clone, PartialEq, Eq, Ord, PartialOrd, Serialize, Deserialize, Display, EnumIter,
)]
pub enum DiscontinuedSignedEntityTypeMessage {
    /// Full Cardano immutable files snapshot.
    CardanoImmutableFilesFull,
}

/// Signed entity type discriminant received through the API.
///
/// This message preserves backward compatibility by distinguishing current,
/// discontinued, and unknown signed entity type discriminants.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Serialize)]
#[serde(untagged)]
pub enum SignedEntityTypeDiscriminantsMessage {
    /// A currently supported signed entity type discriminant.
    Known(SignedEntityTypeDiscriminants),

    /// A signed entity type discriminant that is no longer supported.
    Discontinued(DiscontinuedSignedEntityTypeMessage),

    /// An unrecognized signed entity type discriminant.
    Unknown,
}

impl SignedEntityTypeMessage {
    /// Converts the message into a [SignedEntityType].
    ///
    /// Returns `None` for unknown or discontinued values.
    pub fn into_entity(self) -> Option<SignedEntityType> {
        match self {
            SignedEntityTypeMessage::Known(signed_entity) => Some(signed_entity),
            SignedEntityTypeMessage::Discontinued(_) | SignedEntityTypeMessage::Unknown => None,
        }
    }
}

impl SignedEntityTypeDiscriminantsMessage {
    /// Get all the discriminants without unstable values
    pub fn all_known() -> BTreeSet<Self> {
        // Leverage the list from `SignedEntityTypeDiscriminants` to avoid Unknown and Discontinued and duplicating the filter
        SignedEntityTypeDiscriminants::all()
            .into_iter()
            .map(Self::from)
            .collect()
    }

    /// Converts the message into a [SignedEntityTypeDiscriminants].
    ///
    /// Returns `None` for unknown values.
    pub fn into_discriminant(self) -> Option<SignedEntityTypeDiscriminants> {
        match self {
            SignedEntityTypeDiscriminantsMessage::Known(discriminant) => Some(discriminant),
            SignedEntityTypeDiscriminantsMessage::Discontinued(_)
            | SignedEntityTypeDiscriminantsMessage::Unknown => None,
        }
    }

    /// Convert an iterator of `SignedEntityTypeDiscriminantsMessage` into an iterator of `SignedEntityTypeDiscriminants`
    ///
    /// Instead of failing, any unknown or discontinued values will be discarded
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

impl<'de> Deserialize<'de> for SignedEntityTypeMessage {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        // Duplicated enum to support discarding associated beacon using `IgnoredAny`
        #[derive(Deserialize)]
        enum DiscontinuedSignedEntityRepresentation {
            CardanoImmutableFilesFull(IgnoredAny),
        }

        // Duplicated enum because `IgnoredAny` is not `Serialize`
        #[derive(Deserialize)]
        #[serde(untagged)]
        enum InternalRepresentation {
            Known(SignedEntityType),
            Discontinued(DiscontinuedSignedEntityRepresentation),
            Unknown(IgnoredAny),
        }

        Ok(match InternalRepresentation::deserialize(deserializer)? {
            InternalRepresentation::Known(entity) => SignedEntityTypeMessage::Known(entity),
            InternalRepresentation::Discontinued(entity) => match entity {
                DiscontinuedSignedEntityRepresentation::CardanoImmutableFilesFull(_) => {
                    SignedEntityTypeMessage::Discontinued(
                        DiscontinuedSignedEntityTypeMessage::CardanoImmutableFilesFull,
                    )
                }
            },
            InternalRepresentation::Unknown(_) => SignedEntityTypeMessage::Unknown,
        })
    }
}

impl<'de> Deserialize<'de> for SignedEntityTypeDiscriminantsMessage {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        // Duplicated enums because `IgnoredAny` is not `Serialize`
        #[derive(Deserialize)]
        #[serde(untagged)]
        enum InternalRepresentation {
            Known(SignedEntityTypeDiscriminants),
            Discontinued(DiscontinuedSignedEntityTypeMessage),
            Unknown(IgnoredAny),
        }

        Ok(match InternalRepresentation::deserialize(deserializer)? {
            InternalRepresentation::Known(discriminant) => {
                SignedEntityTypeDiscriminantsMessage::Known(discriminant)
            }
            InternalRepresentation::Discontinued(entity) => {
                SignedEntityTypeDiscriminantsMessage::Discontinued(entity)
            }
            InternalRepresentation::Unknown(_) => SignedEntityTypeDiscriminantsMessage::Unknown,
        })
    }
}

mod infallible_conversions {
    use super::*;

    // Manual implementation instead of using strum::EnumString because it does not allow a "catch all"
    // variant if that variant has no associated value.
    impl From<&str> for SignedEntityTypeDiscriminantsMessage {
        fn from(value: &str) -> Self {
            match value {
                "MithrilStakeDistribution" => SignedEntityTypeDiscriminantsMessage::Known(
                    SignedEntityTypeDiscriminants::MithrilStakeDistribution,
                ),
                "CardanoStakeDistribution" => SignedEntityTypeDiscriminantsMessage::Known(
                    SignedEntityTypeDiscriminants::CardanoStakeDistribution,
                ),
                "CardanoDatabase" => SignedEntityTypeDiscriminantsMessage::Known(
                    SignedEntityTypeDiscriminants::CardanoDatabase,
                ),
                "CardanoTransactions" => SignedEntityTypeDiscriminantsMessage::Known(
                    SignedEntityTypeDiscriminants::CardanoTransactions,
                ),
                "CardanoBlocksTransactions" => SignedEntityTypeDiscriminantsMessage::Known(
                    SignedEntityTypeDiscriminants::CardanoBlocksTransactions,
                ),
                "CardanoImmutableFilesFull" => SignedEntityTypeDiscriminantsMessage::Discontinued(
                    DiscontinuedSignedEntityTypeMessage::CardanoImmutableFilesFull,
                ),
                _ => SignedEntityTypeDiscriminantsMessage::Unknown,
            }
        }
    }

    impl From<SignedEntityType> for SignedEntityTypeMessage {
        fn from(value: SignedEntityType) -> Self {
            SignedEntityTypeMessage::Known(value)
        }
    }

    impl From<SignedEntityType> for SignedEntityTypeDiscriminantsMessage {
        fn from(value: SignedEntityType) -> Self {
            SignedEntityTypeDiscriminantsMessage::Known(value.into())
        }
    }

    impl From<SignedEntityTypeDiscriminants> for SignedEntityTypeDiscriminantsMessage {
        fn from(value: SignedEntityTypeDiscriminants) -> Self {
            SignedEntityTypeDiscriminantsMessage::Known(value)
        }
    }

    impl From<SignedEntityTypeMessage> for SignedEntityTypeDiscriminantsMessage {
        fn from(value: SignedEntityTypeMessage) -> Self {
            match value {
                SignedEntityTypeMessage::Known(value) => {
                    SignedEntityTypeDiscriminantsMessage::Known(value.into())
                }
                SignedEntityTypeMessage::Discontinued(discontinued) => {
                    SignedEntityTypeDiscriminantsMessage::Discontinued(discontinued)
                }
                SignedEntityTypeMessage::Unknown => SignedEntityTypeDiscriminantsMessage::Unknown,
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use strum::IntoEnumIterator;

    use crate::entities::{BlockNumber, BlockNumberOffset, CardanoDbBeacon, Epoch};

    use super::*;

    fn malformed_json_cases() -> [&'static str; 7] {
        [
            "\"string_missing_closing_quote",
            "string_missing_opening_quote\"",
            "[",
            "]",
            r#"{"foo": "bar""#,
            r#""foo": "bar"}"#,
            "()",
        ]
    }

    fn unsupported_json_values() -> [&'static str; 9] {
        [
            "1",
            "true",
            "false",
            "null",
            "{}",
            "[]",
            "[1, 2, 3]",
            r#""does not exist""#,
            r#"{"foo": "bar"}"#,
        ]
    }

    fn externally_tagged_payload_cases<S: AsRef<str>>(variant_name: S) -> [String; 7] {
        let variant_name = variant_name.as_ref();
        [
            format!(r#"{{"{variant_name}":1}}"#),
            format!(r#"{{"{variant_name}":"foo"}}"#),
            format!(r#"{{"{variant_name}":[1]}}"#),
            format!(r#"{{"{variant_name}":["foo"]}}"#),
            format!(r#"{{"{variant_name}":[57, "foo", "bar"]}}"#),
            format!(r#"{{"{variant_name}":[57, {{"foo": "bar"}}]}}"#),
            format!(r#"{{"{variant_name}":{{"foo":"bar"}}}}"#),
        ]
    }

    fn known_entity_and_discriminant_cases()
    -> Vec<(SignedEntityType, SignedEntityTypeDiscriminants)> {
        vec![
            (
                SignedEntityType::MithrilStakeDistribution(Epoch(6)),
                SignedEntityTypeDiscriminants::MithrilStakeDistribution,
            ),
            (
                SignedEntityType::CardanoStakeDistribution(Epoch(7)),
                SignedEntityTypeDiscriminants::CardanoStakeDistribution,
            ),
            (
                SignedEntityType::CardanoDatabase(CardanoDbBeacon::new(9, 110)),
                SignedEntityTypeDiscriminants::CardanoDatabase,
            ),
            (
                SignedEntityType::CardanoTransactions(Epoch(10), BlockNumber(11)),
                SignedEntityTypeDiscriminants::CardanoTransactions,
            ),
            (
                SignedEntityType::CardanoBlocksTransactions(
                    Epoch(12),
                    BlockNumber(13),
                    BlockNumberOffset(14),
                ),
                SignedEntityTypeDiscriminants::CardanoBlocksTransactions,
            ),
        ]
    }

    #[test]
    fn all_known_returns_known_discriminant_messages() {
        assert_eq!(
            SignedEntityTypeDiscriminants::all()
                .into_iter()
                .map(From::from)
                .collect::<BTreeSet<_>>(),
            SignedEntityTypeDiscriminantsMessage::all_known()
        );
        assert!(
            !SignedEntityTypeDiscriminantsMessage::all_known()
                .contains(&SignedEntityTypeDiscriminantsMessage::Unknown)
        );
    }

    mod deserialization_entity {
        use super::*;

        #[test]
        fn known_signed_entity_deserializes_as_known_message() {
            for (signed_entity, _) in known_entity_and_discriminant_cases() {
                let json = serde_json::to_string(&signed_entity).unwrap();
                let res = serde_json::from_str::<SignedEntityTypeMessage>(&json)
                    .unwrap_or_else(|_| panic!("Failed to deserialize: {json}"));

                assert_eq!(SignedEntityTypeMessage::Known(signed_entity), res);
            }
        }

        #[test]
        fn unknown_externally_tagged_entity_deserializes_as_unknown_message() {
            for json in externally_tagged_payload_cases("Whatever") {
                let res = serde_json::from_str::<SignedEntityTypeMessage>(&json)
                    .unwrap_or_else(|_| panic!("Failed to deserialize: {json}"));

                assert_eq!(SignedEntityTypeMessage::Unknown, res);
            }
        }

        #[test]
        fn unsupported_json_values_deserialize_as_unknown_entity_message() {
            for json in unsupported_json_values() {
                let res = serde_json::from_str::<SignedEntityTypeMessage>(json)
                    .unwrap_or_else(|_| panic!("Failed to deserialize: {json}"));

                assert_eq!(SignedEntityTypeMessage::Unknown, res);
            }
        }

        #[test]
        fn discontinued_entity_deserializes_as_discontinued_regardless_of_payload() {
            for (entity, json) in DiscontinuedSignedEntityTypeMessage::iter().flat_map(|entity| {
                externally_tagged_payload_cases(entity.to_string())
                    .into_iter()
                    .map(move |json| (entity, json))
            }) {
                let res = serde_json::from_str::<SignedEntityTypeMessage>(&json)
                    .unwrap_or_else(|_| panic!("Failed to deserialize: {json}"));

                assert_eq!(SignedEntityTypeMessage::Discontinued(entity), res);
            }
        }

        #[test]
        fn discontinued_discriminant_json_deserializes_as_unknown_entity_message() {
            for entity in DiscontinuedSignedEntityTypeMessage::iter() {
                let json = format!("\"{entity}\"");
                let res = serde_json::from_str::<SignedEntityTypeMessage>(&json)
                    .unwrap_or_else(|_| panic!("Failed to deserialize: {json}"));

                assert_eq!(SignedEntityTypeMessage::Unknown, res);
            }
        }

        #[test]
        fn malformed_json_returns_an_error() {
            for json in malformed_json_cases() {
                let res = serde_json::from_str::<SignedEntityTypeMessage>(&json);

                assert!(
                    res.is_err(),
                    "Expected error but got: json: `{json}` result: {res:?}"
                );
            }
        }

        #[test]
        fn known_discriminant_json_deserializes_as_unknown_entity_message() {
            for (_, discriminant) in known_entity_and_discriminant_cases() {
                let json = serde_json::to_string(&discriminant).unwrap();
                let res = serde_json::from_str::<SignedEntityTypeMessage>(&json)
                    .unwrap_or_else(|_| panic!("Failed to deserialize: {json}"));

                assert_eq!(SignedEntityTypeMessage::Unknown, res);
            }
        }
    }

    mod deserialization_discriminant {
        use super::*;

        #[test]
        fn known_discriminant_deserializes_as_known_message() {
            for (_, discriminant) in known_entity_and_discriminant_cases() {
                let json = serde_json::to_string(&discriminant).unwrap();
                let res = serde_json::from_str::<SignedEntityTypeDiscriminantsMessage>(&json)
                    .unwrap_or_else(|_| panic!("Failed to deserialize: {json}"));

                assert_eq!(
                    SignedEntityTypeDiscriminantsMessage::Known(discriminant),
                    res
                );
            }
        }

        #[test]
        fn unsupported_json_values_deserialize_as_unknown_discriminant_message() {
            for json in unsupported_json_values() {
                let res = serde_json::from_str::<SignedEntityTypeDiscriminantsMessage>(json)
                    .unwrap_or_else(|_| panic!("Failed to deserialize: {json}"));

                assert_eq!(SignedEntityTypeDiscriminantsMessage::Unknown, res);
            }
        }

        #[test]
        fn externally_tagged_json_deserializes_as_unknown_discriminant_message() {
            for json in externally_tagged_payload_cases("Whatever") {
                let res = serde_json::from_str::<SignedEntityTypeDiscriminantsMessage>(&json)
                    .unwrap_or_else(|_| panic!("Failed to deserialize: {json}"));

                assert_eq!(SignedEntityTypeDiscriminantsMessage::Unknown, res);
            }
        }

        #[test]
        fn discontinued_discriminant_deserializes_as_discontinued_message() {
            for entity in DiscontinuedSignedEntityTypeMessage::iter() {
                let json = format!("\"{entity}\"");
                let res = serde_json::from_str::<SignedEntityTypeDiscriminantsMessage>(&json)
                    .unwrap_or_else(|_| panic!("Failed to deserialize: {json}"));

                assert_eq!(
                    SignedEntityTypeDiscriminantsMessage::Discontinued(entity),
                    res,
                );
            }
        }

        #[test]
        fn discontinued_full_entity_json_deserializes_as_unknown_discriminant_message() {
            for json in DiscontinuedSignedEntityTypeMessage::iter()
                .flat_map(|entity| externally_tagged_payload_cases(entity.to_string()))
            {
                let res = serde_json::from_str::<SignedEntityTypeDiscriminantsMessage>(&json)
                    .unwrap_or_else(|_| panic!("Failed to deserialize: {json}"));

                assert_eq!(SignedEntityTypeDiscriminantsMessage::Unknown, res);
            }
        }

        #[test]
        fn malformed_json_returns_an_error() {
            for json in malformed_json_cases() {
                let res = serde_json::from_str::<SignedEntityTypeDiscriminantsMessage>(&json);

                assert!(
                    res.is_err(),
                    "Expected error but got: json: `{json}` result: {res:?}"
                );
            }
        }

        #[test]
        fn known_full_entity_json_deserializes_as_unknown_discriminant_message() {
            for (signed_entity, _) in known_entity_and_discriminant_cases() {
                let json = serde_json::to_string(&signed_entity).unwrap();
                let res = serde_json::from_str::<SignedEntityTypeDiscriminantsMessage>(&json)
                    .unwrap_or_else(|_| panic!("Failed to deserialize: {json}"));

                assert_eq!(SignedEntityTypeDiscriminantsMessage::Unknown, res);
            }
        }
    }

    mod infallible_conversions {
        use super::*;

        #[test]
        fn from_signed_entity_to_message() {
            for (signed_entity, _) in known_entity_and_discriminant_cases() {
                assert_eq!(
                    SignedEntityTypeMessage::from(signed_entity.clone()),
                    SignedEntityTypeMessage::Known(signed_entity)
                );
            }
        }

        #[test]
        fn from_signed_entity_to_discriminant_message() {
            for (signed_entity, _) in known_entity_and_discriminant_cases() {
                assert_eq!(
                    SignedEntityTypeDiscriminantsMessage::from(signed_entity.clone()),
                    SignedEntityTypeDiscriminantsMessage::Known(signed_entity.into())
                )
            }
        }

        #[test]
        fn from_signed_entity_discriminant_to_discriminant_message() {
            for (_, discriminant) in known_entity_and_discriminant_cases() {
                assert_eq!(
                    SignedEntityTypeDiscriminantsMessage::from(discriminant),
                    SignedEntityTypeDiscriminantsMessage::Known(discriminant)
                )
            }
        }

        #[test]
        fn from_signed_entity_type_message_to_discriminant_message() {
            for (signed_entity, _) in known_entity_and_discriminant_cases() {
                assert_eq!(
                    SignedEntityTypeDiscriminantsMessage::from(SignedEntityTypeMessage::Known(
                        signed_entity.clone()
                    )),
                    SignedEntityTypeDiscriminantsMessage::Known(signed_entity.into())
                )
            }

            for entity in DiscontinuedSignedEntityTypeMessage::iter() {
                assert_eq!(
                    SignedEntityTypeDiscriminantsMessage::from(
                        SignedEntityTypeMessage::Discontinued(entity)
                    ),
                    SignedEntityTypeDiscriminantsMessage::Discontinued(entity)
                )
            }

            assert_eq!(
                SignedEntityTypeDiscriminantsMessage::from(SignedEntityTypeMessage::Unknown),
                SignedEntityTypeDiscriminantsMessage::Unknown
            )
        }

        #[test]
        fn into_entity_returns_some_for_known_messages() {
            for (entity, _) in known_entity_and_discriminant_cases() {
                assert_eq!(
                    Some(entity.clone()),
                    SignedEntityTypeMessage::Known(entity).into_entity()
                )
            }

            for entity in DiscontinuedSignedEntityTypeMessage::iter() {
                assert_eq!(
                    None,
                    SignedEntityTypeMessage::Discontinued(entity).into_entity()
                )
            }

            assert_eq!(None, SignedEntityTypeMessage::Unknown.into_entity());
        }

        #[test]
        fn into_discriminant_returns_some_for_known_messages() {
            for (_, discriminant) in known_entity_and_discriminant_cases() {
                assert_eq!(
                    Some(discriminant),
                    SignedEntityTypeDiscriminantsMessage::Known(discriminant).into_discriminant()
                )
            }

            for entity in DiscontinuedSignedEntityTypeMessage::iter() {
                assert_eq!(
                    None,
                    SignedEntityTypeDiscriminantsMessage::Discontinued(entity).into_discriminant()
                )
            }

            assert_eq!(
                None,
                SignedEntityTypeDiscriminantsMessage::Unknown.into_discriminant()
            );
        }
    }
}
