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
#[derive(Debug, Copy, Clone, PartialEq, Serialize, Deserialize, Display, EnumIter)]
pub enum DiscontinuedSignedEntityTypeMessage {
    /// Full Cardano immutable files snapshot.
    CardanoImmutableFilesFull,
}

/// Signed entity type discriminant received through the API.
///
/// This message preserves backward compatibility by distinguishing current,
/// discontinued, and unknown signed entity type discriminants.
#[derive(Debug, Clone, PartialEq, Serialize)]
#[serde(untagged)]
pub enum SignedEntityTypeDiscriminantsMessage {
    /// A currently supported signed entity type discriminant.
    Known(SignedEntityTypeDiscriminants),

    /// A signed entity type discriminant that is no longer supported.
    Discontinued(DiscontinuedSignedEntityTypeMessage),

    /// An unrecognized signed entity type discriminant.
    Unknown,
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
}
