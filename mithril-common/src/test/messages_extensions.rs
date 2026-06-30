//! A set of extension traits to add test utilities to this crate `messages`

use std::collections::BTreeSet;

use strum::IntoEnumIterator;

use crate::messages::{DiscontinuedSignedEntityType, SignedEntityTypeDiscriminantsMessage};

/// Extension trait adding test utilities to [SignedEntityTypeDiscriminantsMessage]
pub trait SignedEntityTypeDiscriminantsMessageTestExtension {
    /// `TEST ONLY` - Get all the discontinued discriminants
    fn all_discontinued() -> BTreeSet<SignedEntityTypeDiscriminantsMessage>;
}

impl SignedEntityTypeDiscriminantsMessageTestExtension for SignedEntityTypeDiscriminantsMessage {
    fn all_discontinued() -> BTreeSet<SignedEntityTypeDiscriminantsMessage> {
        DiscontinuedSignedEntityType::all()
            .into_iter()
            .map(SignedEntityTypeDiscriminantsMessage::Discontinued)
            .collect()
    }
}

/// Extension trait adding test utilities to [DiscontinuedSignedEntityType]
pub trait DiscontinuedSignedEntityTypeTestExtension {
    /// `TEST ONLY` - Get all the discontinued discriminants
    fn all() -> BTreeSet<DiscontinuedSignedEntityType>;
}

impl DiscontinuedSignedEntityTypeTestExtension for DiscontinuedSignedEntityType {
    fn all() -> BTreeSet<DiscontinuedSignedEntityType> {
        DiscontinuedSignedEntityType::iter().collect()
    }
}
