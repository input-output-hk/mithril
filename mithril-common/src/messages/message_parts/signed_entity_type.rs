use std::collections::BTreeSet;
use std::fmt::{Display, Formatter};

use serde::de::IgnoredAny;
use serde::{Deserialize, Deserializer, Serialize};
use strum::{AsRefStr, Display, EnumIter, EnumString};
use thiserror::Error;

use crate::entities::{SignedEntityType, SignedEntityTypeDiscriminants};

/// Signed entity type received through the API.
///
/// This message preserves backward compatibility by distinguishing current,
/// discontinued, and unknown signed entity types.
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[serde(untagged)]
pub enum SignedEntityTypeMessage {
    /// A currently supported signed entity type.
    Known(SignedEntityType),

    /// A signed entity type that is no longer supported.
    Discontinued(DiscontinuedSignedEntityType),

    /// An unrecognized signed entity type.
    ///
    /// For JSON, serialized as `null`.
    Unknown,
}

/// Signed entity types that were supported by previous API versions.
///
/// These values are kept to deserialize historical messages without treating
/// them as unknown.
// IMPORTANT: When adding a variant, also update `DiscontinuedEntityPayload` in `SignedEntityTypeMessage`
// deserialization.
#[derive(
    Debug,
    Copy,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Serialize,
    Deserialize,
    Display,
    EnumIter,
    EnumString,
    AsRefStr,
)]
pub enum DiscontinuedSignedEntityType {
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
    Discontinued(DiscontinuedSignedEntityType),

    /// An unrecognized signed entity type discriminant.
    ///
    /// For JSON, serialized as `null`.
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
    /// Returns all currently supported signed entity type discriminants.
    pub fn all_known() -> BTreeSet<Self> {
        // Leverage the list from `SignedEntityTypeDiscriminants` to avoid Unknown and Discontinued and duplicating the filter
        SignedEntityTypeDiscriminants::all()
            .into_iter()
            .map(Self::from)
            .collect()
    }

    /// Converts the message into a [SignedEntityTypeDiscriminants].
    ///
    /// Returns `None` for unknown or discontinued values.
    pub fn into_discriminant(self) -> Option<SignedEntityTypeDiscriminants> {
        match self {
            SignedEntityTypeDiscriminantsMessage::Known(discriminant) => Some(discriminant),
            SignedEntityTypeDiscriminantsMessage::Discontinued(_)
            | SignedEntityTypeDiscriminantsMessage::Unknown => None,
        }
    }

    /// Convert an iterator of `SignedEntityTypeDiscriminantsMessage` into a collection of `SignedEntityTypeDiscriminants`
    ///
    /// Unknown and discontinued values are discarded.
    pub fn into_known_discriminants<
        T: IntoIterator<Item = Self>,
        B: FromIterator<SignedEntityTypeDiscriminants>,
    >(
        iter: T,
    ) -> B {
        iter.into_iter().filter_map(Self::into_discriminant).collect()
    }
}

impl Display for SignedEntityTypeMessage {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            SignedEntityTypeMessage::Known(discriminant) => discriminant.fmt(f),
            SignedEntityTypeMessage::Discontinued(entity) => {
                write!(f, "Discontinued({entity})")
            }
            SignedEntityTypeMessage::Unknown => write!(f, "Unknown"),
        }
    }
}

impl Display for SignedEntityTypeDiscriminantsMessage {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            SignedEntityTypeDiscriminantsMessage::Known(discriminant) => discriminant.fmt(f),
            SignedEntityTypeDiscriminantsMessage::Discontinued(entity) => {
                write!(f, "Discontinued({entity})")
            }
            SignedEntityTypeDiscriminantsMessage::Unknown => write!(f, "Unknown"),
        }
    }
}

impl<'de> Deserialize<'de> for SignedEntityTypeMessage {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        // Duplicated enum to support discarding associated beacon using `IgnoredAny`
        #[derive(Deserialize)]
        enum DiscontinuedEntityPayload {
            CardanoImmutableFilesFull(IgnoredAny),
        }

        impl From<DiscontinuedEntityPayload> for DiscontinuedSignedEntityType {
            fn from(value: DiscontinuedEntityPayload) -> Self {
                match value {
                    DiscontinuedEntityPayload::CardanoImmutableFilesFull(_) => {
                        DiscontinuedSignedEntityType::CardanoImmutableFilesFull
                    }
                }
            }
        }

        // Duplicated enum because `IgnoredAny` is not `Serialize`
        #[derive(Deserialize)]
        #[serde(untagged)]
        enum InternalRepresentation {
            Known(SignedEntityType),
            Discontinued(DiscontinuedEntityPayload),
            Unknown(IgnoredAny),
        }

        Ok(match InternalRepresentation::deserialize(deserializer)? {
            InternalRepresentation::Known(entity) => SignedEntityTypeMessage::Known(entity),
            InternalRepresentation::Discontinued(entity) => {
                SignedEntityTypeMessage::Discontinued(entity.into())
            }
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
            Discontinued(DiscontinuedSignedEntityType),
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

    // Manual implementation instead of deriving `strum::EnumString` because it does not allow a "catch all"
    // variant if that variant has no associated value.
    impl From<&str> for SignedEntityTypeDiscriminantsMessage {
        fn from(value: &str) -> Self {
            value
                .parse::<SignedEntityTypeDiscriminants>()
                .map(Self::Known)
                .or_else(|_| value.parse::<DiscontinuedSignedEntityType>().map(Self::Discontinued))
                .unwrap_or(Self::Unknown)
        }
    }

    impl From<SignedEntityType> for SignedEntityTypeMessage {
        fn from(value: SignedEntityType) -> Self {
            SignedEntityTypeMessage::Known(value)
        }
    }

    impl From<SignedEntityType> for SignedEntityTypeDiscriminantsMessage {
        fn from(value: SignedEntityType) -> Self {
            Self::Known(value.into())
        }
    }

    impl From<SignedEntityTypeDiscriminants> for SignedEntityTypeDiscriminantsMessage {
        fn from(value: SignedEntityTypeDiscriminants) -> Self {
            Self::Known(value)
        }
    }

    impl From<SignedEntityTypeMessage> for SignedEntityTypeDiscriminantsMessage {
        fn from(value: SignedEntityTypeMessage) -> Self {
            match value {
                SignedEntityTypeMessage::Known(value) => Self::Known(value.into()),
                SignedEntityTypeMessage::Discontinued(discontinued) => {
                    Self::Discontinued(discontinued)
                }
                SignedEntityTypeMessage::Unknown => Self::Unknown,
            }
        }
    }
}

/// Error returned when a signed entity type message cannot be converted to a
/// current signed entity type or discriminant.
#[derive(Debug, PartialEq, Eq, Error)]
pub enum IncompatibleSignedEntityTypeError {
    /// The message contains a signed entity type that is no longer supported.
    #[error("Discontinued signed entity type: {0}")]
    Discontinued(DiscontinuedSignedEntityType),

    /// The message contains an unrecognized signed entity type.
    #[error("Unknown signed entity type")]
    Unknown,
}

mod fallible_conversions {
    use super::*;

    impl TryFrom<SignedEntityTypeMessage> for SignedEntityType {
        type Error = IncompatibleSignedEntityTypeError;

        fn try_from(value: SignedEntityTypeMessage) -> Result<Self, Self::Error> {
            match value {
                SignedEntityTypeMessage::Known(entity) => Ok(entity),
                SignedEntityTypeMessage::Discontinued(entity) => {
                    Err(IncompatibleSignedEntityTypeError::Discontinued(entity))
                }
                SignedEntityTypeMessage::Unknown => Err(IncompatibleSignedEntityTypeError::Unknown),
            }
        }
    }

    impl TryFrom<SignedEntityTypeMessage> for SignedEntityTypeDiscriminants {
        type Error = IncompatibleSignedEntityTypeError;

        fn try_from(value: SignedEntityTypeMessage) -> Result<Self, Self::Error> {
            SignedEntityType::try_from(value).map(Into::into)
        }
    }

    impl TryFrom<SignedEntityTypeDiscriminantsMessage> for SignedEntityTypeDiscriminants {
        type Error = IncompatibleSignedEntityTypeError;

        fn try_from(value: SignedEntityTypeDiscriminantsMessage) -> Result<Self, Self::Error> {
            match value {
                SignedEntityTypeDiscriminantsMessage::Known(entity) => Ok(entity),
                SignedEntityTypeDiscriminantsMessage::Discontinued(entity) => {
                    Err(IncompatibleSignedEntityTypeError::Discontinued(entity))
                }
                SignedEntityTypeDiscriminantsMessage::Unknown => {
                    Err(IncompatibleSignedEntityTypeError::Unknown)
                }
            }
        }
    }
}

mod comparison {
    use super::*;

    impl PartialEq<SignedEntityTypeMessage> for SignedEntityType {
        fn eq(&self, message: &SignedEntityTypeMessage) -> bool {
            match message {
                SignedEntityTypeMessage::Known(entity) => entity.eq(self),
                SignedEntityTypeMessage::Discontinued(_) | SignedEntityTypeMessage::Unknown => {
                    false
                }
            }
        }
    }

    impl PartialEq<SignedEntityType> for SignedEntityTypeMessage {
        fn eq(&self, other: &SignedEntityType) -> bool {
            other.eq(self)
        }
    }

    impl PartialEq<SignedEntityTypeDiscriminantsMessage> for SignedEntityTypeDiscriminants {
        fn eq(&self, message: &SignedEntityTypeDiscriminantsMessage) -> bool {
            match message {
                SignedEntityTypeDiscriminantsMessage::Known(discriminant) => discriminant.eq(self),
                SignedEntityTypeDiscriminantsMessage::Discontinued(_)
                | SignedEntityTypeDiscriminantsMessage::Unknown => false,
            }
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
    use strum::IntoEnumIterator;

    use crate::{
        assert_equivalent,
        entities::{BlockNumber, BlockNumberOffset, CardanoDbBeacon, Epoch},
        test::{assert_same_json, entities_extensions::SignedEntityTypeDiscriminantsTestExtension},
    };

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

    fn serialize_then_deserialize<T: Serialize + serde::de::DeserializeOwned>(value: &T) -> T {
        let json = serde_json::to_string(value).unwrap();
        serde_json::from_str::<T>(&json)
            .unwrap_or_else(|e| panic!("Failed to deserialize `{json}`: {e}"))
    }

    #[test]
    fn known_entity_and_discriminant_cases_are_exhaustive() {
        let discriminants: Vec<_> = known_entity_and_discriminant_cases()
            .into_iter()
            .map(|(_, discriminant)| discriminant)
            .collect();

        assert_equivalent!(
            SignedEntityTypeDiscriminants::all_with_unstable_vec(),
            discriminants
        );
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

    mod display {
        use super::*;

        #[test]
        fn displaying_untags_known_values() {
            for (entity, discriminant) in known_entity_and_discriminant_cases() {
                assert_eq!(
                    entity.to_string(),
                    format!("{}", SignedEntityTypeMessage::from(entity)),
                );
                assert_eq!(
                    discriminant.to_string(),
                    format!(
                        "{}",
                        SignedEntityTypeDiscriminantsMessage::from(discriminant)
                    ),
                );
            }
        }

        #[test]
        fn displaying_discontinued_value_wraps_it_in_discontinued() {
            for entity in DiscontinuedSignedEntityType::iter() {
                let expected = format!("Discontinued({entity})");
                assert_eq!(
                    expected,
                    format!("{}", SignedEntityTypeMessage::Discontinued(entity)),
                );
                assert_eq!(
                    expected,
                    format!(
                        "{}",
                        SignedEntityTypeDiscriminantsMessage::Discontinued(entity)
                    ),
                );
            }
        }

        #[test]
        fn displaying_unknown_value_yields_unknown() {
            assert_eq!("Unknown", format!("{}", SignedEntityTypeMessage::Unknown),);
            assert_eq!(
                "Unknown",
                format!("{}", SignedEntityTypeDiscriminantsMessage::Unknown),
            );
        }
    }

    mod serialize_entity {
        use super::*;

        #[test]
        fn known_message_serializes_like_signed_entity_type() {
            for (signed_entity, _) in known_entity_and_discriminant_cases() {
                assert_same_json!(
                    value: &signed_entity,
                    value: &SignedEntityTypeMessage::Known(signed_entity)
                );
            }
        }

        #[test]
        fn discontinued_message_serializes_as_discontinued_variant_name() {
            for entity in DiscontinuedSignedEntityType::iter() {
                assert_same_json!(
                    json: &format!(r#""{entity}""#),
                    value: &SignedEntityTypeMessage::Discontinued(entity)
                );
            }
        }

        #[test]
        fn unknown_message_serializes_as_null() {
            assert_same_json!(json: "null", value: &SignedEntityTypeMessage::Unknown);
        }

        #[test]
        fn known_message_round_trips_through_json() {
            for (signed_entity, _) in known_entity_and_discriminant_cases() {
                let res = serialize_then_deserialize(&SignedEntityTypeMessage::Known(
                    signed_entity.clone(),
                ));
                assert_eq!(SignedEntityTypeMessage::Known(signed_entity), res);
            }
        }

        #[test]
        fn unknown_message_round_trips_through_json() {
            let res = serialize_then_deserialize(&SignedEntityTypeMessage::Unknown);
            assert_eq!(SignedEntityTypeMessage::Unknown, res);
        }

        #[test]
        fn discontinued_message_deserializes_as_unknown_after_serialization() {
            for entity in DiscontinuedSignedEntityType::iter() {
                let res =
                    serialize_then_deserialize(&SignedEntityTypeMessage::Discontinued(entity));
                assert_eq!(SignedEntityTypeMessage::Unknown, res);
            }
        }
    }

    mod serialize_discriminant {
        use super::*;

        #[test]
        fn known_discriminant_message_serializes_like_discriminant() {
            for (_, discriminant) in known_entity_and_discriminant_cases() {
                assert_same_json!(
                    value: &discriminant,
                    value: &SignedEntityTypeDiscriminantsMessage::Known(discriminant)
                );
            }
        }

        #[test]
        fn discontinued_discriminant_message_serializes_as_discontinued_variant_name() {
            for entity in DiscontinuedSignedEntityType::iter() {
                assert_same_json!(
                    json: &format!(r#""{entity}""#),
                    value: &SignedEntityTypeDiscriminantsMessage::Discontinued(entity)
                );
            }
        }

        #[test]
        fn unknown_discriminant_message_serializes_as_null() {
            assert_same_json!(json: "null", value: &SignedEntityTypeDiscriminantsMessage::Unknown);
        }

        #[test]
        fn discriminant_messages_round_trip_through_json() {
            let cases: Vec<SignedEntityTypeDiscriminantsMessage> =
                known_entity_and_discriminant_cases()
                    .into_iter()
                    .map(|(_, d)| SignedEntityTypeDiscriminantsMessage::Known(d))
                    .chain(
                        DiscontinuedSignedEntityType::iter()
                            .map(SignedEntityTypeDiscriminantsMessage::Discontinued),
                    )
                    .chain(vec![SignedEntityTypeDiscriminantsMessage::Unknown])
                    .collect();

            for case in cases {
                let res = serialize_then_deserialize(&case);
                assert_eq!(case, res);
            }
        }
    }

    mod serialize_discontinued_entity {
        use super::*;

        #[test]
        fn discontinued_signed_entity_type_serializes_to_variant_name() {
            for entity in DiscontinuedSignedEntityType::iter() {
                assert_same_json!(json: &format!(r#""{entity}""#), value: &entity);
            }
        }

        #[test]
        fn discontinued_signed_entity_type_round_trips_through_json() {
            for entity in DiscontinuedSignedEntityType::iter() {
                let json = serde_json::to_string(&entity).unwrap();
                let res = serde_json::from_str::<DiscontinuedSignedEntityType>(&json)
                    .unwrap_or_else(|e| panic!("Failed to deserialize `{json}`: {e}"));

                assert_eq!(entity, res);
            }
        }
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
            for (entity, json) in DiscontinuedSignedEntityType::iter().flat_map(|entity| {
                externally_tagged_payload_cases(entity)
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
            for entity in DiscontinuedSignedEntityType::iter() {
                let json = format!("\"{entity}\"");
                let res = serde_json::from_str::<SignedEntityTypeMessage>(&json)
                    .unwrap_or_else(|_| panic!("Failed to deserialize: {json}"));

                assert_eq!(SignedEntityTypeMessage::Unknown, res);
            }
        }

        #[test]
        fn malformed_json_returns_an_error() {
            for json in malformed_json_cases() {
                let res = serde_json::from_str::<SignedEntityTypeMessage>(json);

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
            for entity in DiscontinuedSignedEntityType::iter() {
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
            for json in
                DiscontinuedSignedEntityType::iter().flat_map(externally_tagged_payload_cases)
            {
                let res = serde_json::from_str::<SignedEntityTypeDiscriminantsMessage>(&json)
                    .unwrap_or_else(|_| panic!("Failed to deserialize: {json}"));

                assert_eq!(SignedEntityTypeDiscriminantsMessage::Unknown, res);
            }
        }

        #[test]
        fn malformed_json_returns_an_error() {
            for json in malformed_json_cases() {
                let res = serde_json::from_str::<SignedEntityTypeDiscriminantsMessage>(json);

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

    mod discriminant_from_str {
        use super::*;

        #[test]
        fn from_known_discriminants() {
            for (_, discriminant) in known_entity_and_discriminant_cases() {
                assert_eq!(
                    SignedEntityTypeDiscriminantsMessage::Known(discriminant),
                    SignedEntityTypeDiscriminantsMessage::from(discriminant.as_ref())
                );
            }

            for entity in DiscontinuedSignedEntityType::iter() {
                assert_eq!(
                    SignedEntityTypeDiscriminantsMessage::Discontinued(entity),
                    SignedEntityTypeDiscriminantsMessage::from(entity.as_ref())
                );
            }

            // special case included for completeness
            assert_eq!(
                SignedEntityTypeDiscriminantsMessage::from("Unknown"),
                SignedEntityTypeDiscriminantsMessage::Unknown
            );
        }

        #[test]
        fn all_other_strings_yield_unknown() {
            let discriminant = SignedEntityTypeDiscriminantsMessage::from("not_exist");
            assert_eq!(discriminant, SignedEntityTypeDiscriminantsMessage::Unknown);
        }
    }

    mod convert_iterable_into_known_discriminants {
        use super::*;

        #[test]
        fn converts_all_known_discriminants_message() {
            let discriminants_message: Vec<_> = known_entity_and_discriminant_cases()
                .into_iter()
                .map(|(_, discriminant)| discriminant.into())
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
        fn discards_unknown_and_discontinued_discriminants() {
            let discriminants: Vec<SignedEntityTypeDiscriminants> =
                SignedEntityTypeDiscriminantsMessage::into_known_discriminants(vec![
                    SignedEntityTypeDiscriminantsMessage::Discontinued(
                        DiscontinuedSignedEntityType::CardanoImmutableFilesFull,
                    ),
                    SignedEntityTypeDiscriminants::MithrilStakeDistribution.into(),
                    SignedEntityTypeDiscriminantsMessage::Unknown,
                    SignedEntityTypeDiscriminants::CardanoTransactions.into(),
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
        fn returns_empty_collection_when_input_contains_only_unknown_and_discontinued_discriminants()
         {
            let discriminants: Vec<SignedEntityTypeDiscriminants> =
                SignedEntityTypeDiscriminantsMessage::into_known_discriminants(vec![
                    SignedEntityTypeDiscriminantsMessage::Unknown,
                    SignedEntityTypeDiscriminantsMessage::Discontinued(
                        DiscontinuedSignedEntityType::CardanoImmutableFilesFull,
                    ),
                ]);

            assert_eq!(Vec::<SignedEntityTypeDiscriminants>::new(), discriminants);
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

            for entity in DiscontinuedSignedEntityType::iter() {
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

            for entity in DiscontinuedSignedEntityType::iter() {
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

            for entity in DiscontinuedSignedEntityType::iter() {
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

    mod fallible_conversions {
        use super::*;

        #[test]
        fn try_from_entity_message_to_entity_succeeds_for_known_values() {
            for (signed_entity, _) in known_entity_and_discriminant_cases() {
                assert_eq!(
                    SignedEntityType::try_from(SignedEntityTypeMessage::Known(
                        signed_entity.clone()
                    ))
                    .unwrap(),
                    signed_entity
                );
            }
        }

        #[test]
        fn try_from_entity_message_to_discriminant_succeeds_for_known_values() {
            for (signed_entity, discriminant) in known_entity_and_discriminant_cases() {
                assert_eq!(
                    SignedEntityTypeDiscriminants::try_from(SignedEntityTypeMessage::Known(
                        signed_entity
                    ))
                    .unwrap(),
                    discriminant,
                )
            }
        }

        #[test]
        fn try_from_discriminant_message_to_discriminant_succeeds_for_known_values() {
            for (_, discriminant) in known_entity_and_discriminant_cases() {
                assert_eq!(
                    SignedEntityTypeDiscriminants::try_from(
                        SignedEntityTypeDiscriminantsMessage::Known(discriminant)
                    )
                    .unwrap(),
                    discriminant,
                )
            }
        }

        #[test]
        fn try_from_message_to_entity_fails_for_unknown_values() {
            assert_eq!(
                SignedEntityType::try_from(SignedEntityTypeMessage::Unknown).unwrap_err(),
                IncompatibleSignedEntityTypeError::Unknown
            );
        }

        #[test]
        fn try_from_entity_message_to_discriminant_fails_for_unknown_values() {
            assert_eq!(
                SignedEntityTypeDiscriminants::try_from(SignedEntityTypeMessage::Unknown)
                    .unwrap_err(),
                IncompatibleSignedEntityTypeError::Unknown
            );
        }

        #[test]
        fn try_from_discriminant_message_to_discriminant_fails_for_unknown_values() {
            assert_eq!(
                SignedEntityTypeDiscriminants::try_from(
                    SignedEntityTypeDiscriminantsMessage::Unknown
                )
                .unwrap_err(),
                IncompatibleSignedEntityTypeError::Unknown
            );
        }

        #[test]
        fn try_from_message_to_entity_fails_for_discontinued_values() {
            for entity in DiscontinuedSignedEntityType::iter() {
                assert_eq!(
                    SignedEntityType::try_from(SignedEntityTypeMessage::Discontinued(entity))
                        .unwrap_err(),
                    IncompatibleSignedEntityTypeError::Discontinued(entity)
                );
            }
        }

        #[test]
        fn try_from_entity_message_to_discriminant_fails_for_discontinued_values() {
            for entity in DiscontinuedSignedEntityType::iter() {
                assert_eq!(
                    SignedEntityTypeDiscriminants::try_from(SignedEntityTypeMessage::Discontinued(
                        entity
                    ))
                    .unwrap_err(),
                    IncompatibleSignedEntityTypeError::Discontinued(entity)
                );
            }
        }

        #[test]
        fn try_from_discriminant_message_to_discriminant_fails_for_discontinued_values() {
            for entity in DiscontinuedSignedEntityType::iter() {
                assert_eq!(
                    SignedEntityTypeDiscriminants::try_from(
                        SignedEntityTypeDiscriminantsMessage::Discontinued(entity)
                    )
                    .unwrap_err(),
                    IncompatibleSignedEntityTypeError::Discontinued(entity)
                );
            }
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
            for (_, discriminant) in known_entity_and_discriminant_cases() {
                let discriminant_message =
                    SignedEntityTypeDiscriminantsMessage::Known(discriminant);

                assert_eq!(discriminant_message, discriminant);
                assert_eq!(discriminant, discriminant_message);
            }
        }

        #[test]
        fn message_equals_entity_and_reverse() {
            for (entity, _) in known_entity_and_discriminant_cases() {
                let message = SignedEntityTypeMessage::Known(entity.clone());

                assert_eq!(message, entity);
                assert_eq!(entity, message);
                assert_ne!(message, alter_entity(&entity));
                assert_ne!(alter_entity(&entity), message);
            }
        }

        #[test]
        fn unknown_messages_do_not_equal_entities() {
            for (entity, discriminant) in known_entity_and_discriminant_cases() {
                assert_ne!(entity, SignedEntityTypeMessage::Unknown);
                assert_ne!(SignedEntityTypeMessage::Unknown, entity);
                assert_ne!(discriminant, SignedEntityTypeDiscriminantsMessage::Unknown);
                assert_ne!(SignedEntityTypeDiscriminantsMessage::Unknown, discriminant);
            }
        }
    }
}
