use strum::IntoEnumIterator;
use strum_macros::{Display, EnumIter, EnumString, FromRepr};

/// The signed entity type that represents a type of data signed by the Mithril protocol
/// Note: Each variant of this enum must be associated to an entry in the `signed_entity_type` tables of the signer/aggregator nodes.
///       The variant are identified by their discriminant (i.e. index in the enum), thus the modification of this type should only ever consist of appending new variants.
#[derive(Display, FromRepr, EnumString, EnumIter, Debug, Clone, Copy, PartialEq, Eq)]
#[strum(serialize_all = "PascalCase")]
pub enum SignedEntityType {
    /// Mithril stake distribution
    MithrilStakeDistribution,

    /// Cardano Stake Distribution
    CardanoStakeDistribution,

    /// Full Cardano Immutable Files
    CardanoImmutableFilesFull,
}

impl SignedEntityType {
    /// Retrieve the list of entity types
    pub fn entity_types() -> Vec<Self> {
        Self::iter().collect()
    }

    /// Retrieve a dummy enty (for test only)
    pub fn dummy() -> Self {
        Self::entity_types().first().unwrap().to_owned()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn from_repr() {
        let supported_entity_type = SignedEntityType::from_repr(SignedEntityType::dummy() as usize)
            .expect("This signed entity type should support conversion from representation.");

        assert_eq!(SignedEntityType::dummy(), supported_entity_type);
    }
}
