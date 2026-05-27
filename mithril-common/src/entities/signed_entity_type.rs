use std::collections::BTreeSet;
use std::str::FromStr;
use std::time::Duration;

use anyhow::anyhow;
use digest::Update;
use serde::{Deserialize, Serialize};
use sha2::Sha256;
use strum::{AsRefStr, Display, EnumDiscriminants, EnumIter, EnumString, IntoEnumIterator};

use crate::{
    StdResult,
    crypto_helper::{TryFromBytes, TryToBytes},
    entities::BlockNumberOffset,
};

use super::{BlockNumber, CardanoDbBeacon, Epoch};

/// Unique numeric identifier of a [`SignedEntityType`] variant, as stored in the database.
///
/// These values are **immutable**: existing mappings must never change or be reused.
///
/// Removed value (do not reuse): CardanoImmutableFilesFull = 2
pub type SignedEntityTypeId = u16;

/// Database representation of the SignedEntityType::MithrilStakeDistribution value
const ENTITY_TYPE_MITHRIL_STAKE_DISTRIBUTION: SignedEntityTypeId = 0;

/// Database representation of the SignedEntityType::CardanoStakeDistribution value
const ENTITY_TYPE_CARDANO_STAKE_DISTRIBUTION: SignedEntityTypeId = 1;

/// Database representation of the SignedEntityType::CardanoTransactions value
const ENTITY_TYPE_CARDANO_TRANSACTIONS: SignedEntityTypeId = 3;

/// Database representation of the SignedEntityType::CardanoDatabase value
const ENTITY_TYPE_CARDANO_DATABASE: SignedEntityTypeId = 4;

/// Database representation of the SignedEntityType::CardanoBlocksTransactions value
const ENTITY_TYPE_CARDANO_BLOCKS_TRANSACTIONS: SignedEntityTypeId = 5;

/// Database representation of the SignedEntityType::CardanoNodeLedgerState value
const ENTITY_TYPE_CARDANO_NODE_LEDGER_STATE: SignedEntityTypeId = 6;

/// The signed entity type that represents a type of data signed by the Mithril
/// protocol Note: Each variant of this enum must be associated to an entry in
/// the `signed_entity_type` table of the signer/aggregator nodes. The variant
/// are identified by their discriminant (i.e. index in the enum), thus the
/// modification of this type should only ever consist of appending new
/// variants.
// Important note: The order of the variants is important as it is used for the derived Ord trait
// of the `SignedEntityTypeDiscriminants` enum.
#[derive(Display, Debug, Clone, PartialEq, Eq, Serialize, Deserialize, EnumDiscriminants)]
#[strum(serialize_all = "PascalCase")]
#[strum_discriminants(doc = "The discriminants of the SignedEntityType enum.")]
#[strum_discriminants(derive(
    Display,
    EnumString,
    AsRefStr,
    Serialize,
    Deserialize,
    PartialOrd,
    Ord,
    EnumIter,
))]
pub enum SignedEntityType {
    /// Mithril stake distribution
    MithrilStakeDistribution(Epoch),

    /// Cardano Stake Distribution
    CardanoStakeDistribution(Epoch),

    /// Cardano Database
    CardanoDatabase(CardanoDbBeacon),

    /// Cardano Transactions
    CardanoTransactions(Epoch, BlockNumber),

    /// Cardano Blocks and Transactions
    CardanoBlocksTransactions(Epoch, BlockNumber, BlockNumberOffset),

    /// Cardano Node Ledger State
    CardanoNodeLedgerState(CardanoDbBeacon),
}

impl SignedEntityType {
    /// Create a new signed entity type for a genesis certificate (a [Self::MithrilStakeDistribution])
    pub fn genesis(epoch: Epoch) -> Self {
        Self::MithrilStakeDistribution(epoch)
    }

    /// Return the epoch from the signed entity.
    pub fn get_epoch(&self) -> Epoch {
        match self {
            Self::CardanoDatabase(b) => b.epoch,
            Self::CardanoStakeDistribution(e)
            | Self::MithrilStakeDistribution(e)
            | Self::CardanoTransactions(e, _)
            | Self::CardanoBlocksTransactions(e, _, _) => *e,
            Self::CardanoNodeLedgerState(b) => b.epoch,
        }
    }

    /// Return the epoch at which the signed entity type is signed.
    pub fn get_epoch_when_signed_entity_type_is_signed(&self) -> Epoch {
        match self {
            Self::CardanoDatabase(beacon) => beacon.epoch,
            Self::CardanoStakeDistribution(epoch) => epoch.next(),
            Self::MithrilStakeDistribution(epoch)
            | Self::CardanoTransactions(epoch, _)
            | Self::CardanoBlocksTransactions(epoch, _, _) => *epoch,
            Self::CardanoNodeLedgerState(beacon) => beacon.epoch,
        }
    }

    /// Get the database value from enum's instance
    pub fn index(&self) -> SignedEntityTypeId {
        match self {
            Self::MithrilStakeDistribution(..) => ENTITY_TYPE_MITHRIL_STAKE_DISTRIBUTION,
            Self::CardanoStakeDistribution(..) => ENTITY_TYPE_CARDANO_STAKE_DISTRIBUTION,
            Self::CardanoTransactions(..) => ENTITY_TYPE_CARDANO_TRANSACTIONS,
            Self::CardanoBlocksTransactions(..) => ENTITY_TYPE_CARDANO_BLOCKS_TRANSACTIONS,
            Self::CardanoDatabase(..) => ENTITY_TYPE_CARDANO_DATABASE,
            Self::CardanoNodeLedgerState(..) => ENTITY_TYPE_CARDANO_NODE_LEDGER_STATE,
        }
    }

    /// Return a JSON serialized value of the internal beacon
    pub fn get_json_beacon(&self) -> StdResult<String> {
        let value = match self {
            Self::CardanoDatabase(value) | Self::CardanoNodeLedgerState(value) => {
                serde_json::to_string(value)?
            }
            Self::CardanoStakeDistribution(value) | Self::MithrilStakeDistribution(value) => {
                serde_json::to_string(value)?
            }
            Self::CardanoTransactions(epoch, block_number) => {
                let json = serde_json::json!({
                    "epoch": epoch,
                    "block_number": block_number,
                });
                serde_json::to_string(&json)?
            }
            Self::CardanoBlocksTransactions(epoch, block_number, block_number_offset) => {
                let json = serde_json::json!({
                    "epoch": epoch,
                    "block_number": block_number,
                    "block_number_offset": block_number_offset,
                });
                serde_json::to_string(&json)?
            }
        };

        Ok(value)
    }

    /// Return the associated open message timeout
    pub fn get_open_message_timeout(&self) -> Option<Duration> {
        match self {
            Self::MithrilStakeDistribution(..) => Some(Duration::from_secs(3600)),
            Self::CardanoStakeDistribution(..) => Some(Duration::from_secs(1800)),
            Self::CardanoTransactions(..) => Some(Duration::from_secs(600)),
            Self::CardanoBlocksTransactions(..) => Some(Duration::from_secs(600)),
            Self::CardanoDatabase(..) => Some(Duration::from_secs(600)),
            Self::CardanoNodeLedgerState(..) => Some(Duration::from_secs(600)),
        }
    }

    pub(crate) fn feed_hash(&self, hasher: &mut Sha256) {
        // TODO: feed discriminant index to the hasher for all signed entity types (a migration of existing hashes will be needed)
        if matches!(self, Self::CardanoBlocksTransactions(..)) {
            // Leverage the database index value to differentiate types that share the same beacon
            hasher.update(&self.index().to_be_bytes());
        }

        match self {
            SignedEntityType::MithrilStakeDistribution(epoch)
            | SignedEntityType::CardanoStakeDistribution(epoch) => {
                hasher.update(&epoch.to_be_bytes())
            }
            SignedEntityType::CardanoDatabase(db_beacon)
            | SignedEntityType::CardanoNodeLedgerState(db_beacon) => {
                hasher.update(&db_beacon.epoch.to_be_bytes());
                hasher.update(&db_beacon.immutable_file_number.to_be_bytes());
            }
            SignedEntityType::CardanoTransactions(epoch, block_number) => {
                hasher.update(&epoch.to_be_bytes());
                hasher.update(&block_number.to_be_bytes());
            }
            SignedEntityType::CardanoBlocksTransactions(
                epoch,
                block_number,
                block_number_offset,
            ) => {
                hasher.update(&epoch.to_be_bytes());
                hasher.update(&block_number.to_be_bytes());
                hasher.update(&block_number_offset.to_be_bytes());
            }
        }
    }
}

impl TryFromBytes for SignedEntityType {
    fn try_from_bytes(bytes: &[u8]) -> StdResult<Self> {
        let (res, _) =
            bincode::serde::decode_from_slice::<Self, _>(bytes, bincode::config::standard())?;

        Ok(res)
    }
}

impl TryToBytes for SignedEntityType {
    fn to_bytes_vec(&self) -> StdResult<Vec<u8>> {
        bincode::serde::encode_to_vec(self, bincode::config::standard()).map_err(|e| e.into())
    }
}

/// Result of [SignedEntityTypeDiscriminants::parse_list]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParsedDiscriminants {
    /// The deduplicated set of discriminants that were parsed successfully.
    pub discriminants: BTreeSet<SignedEntityTypeDiscriminants>,
    /// The list of values that were ignored during parsing (e.g. unknown or malformed values).
    pub ignored_values: Vec<String>,
}

impl ParsedDiscriminants {
    /// Return true if the list of ignored values is not empty.
    pub fn has_ignored_values(&self) -> bool {
        !self.ignored_values.is_empty()
    }
}

impl SignedEntityTypeDiscriminants {
    /// Discriminants that are unstable and should be excluded from certain operations.
    // Note: Empty right now since all discriminants are stable
    const UNSTABLE_DISCRIMINANTS: [Self; 0] = [];

    /// Get all stable discriminants.
    ///
    /// Unstable discriminants are intentionally excluded.
    pub fn all() -> BTreeSet<Self> {
        Self::iter_all().collect()
    }

    /// Iterate over all stable discriminants.
    ///
    /// Unstable discriminants are intentionally excluded.
    fn iter_all() -> impl Iterator<Item = Self> {
        Self::iter().filter(|d| !Self::UNSTABLE_DISCRIMINANTS.contains(d))
    }

    /// Get the database value from enum's instance
    pub fn index(&self) -> SignedEntityTypeId {
        match self {
            Self::MithrilStakeDistribution => ENTITY_TYPE_MITHRIL_STAKE_DISTRIBUTION,
            Self::CardanoStakeDistribution => ENTITY_TYPE_CARDANO_STAKE_DISTRIBUTION,
            Self::CardanoTransactions => ENTITY_TYPE_CARDANO_TRANSACTIONS,
            Self::CardanoBlocksTransactions => ENTITY_TYPE_CARDANO_BLOCKS_TRANSACTIONS,
            Self::CardanoDatabase => ENTITY_TYPE_CARDANO_DATABASE,
            Self::CardanoNodeLedgerState => ENTITY_TYPE_CARDANO_NODE_LEDGER_STATE,
        }
    }

    /// Get the discriminant associated with the given id
    pub fn from_id(
        signed_entity_type_id: SignedEntityTypeId,
    ) -> StdResult<SignedEntityTypeDiscriminants> {
        match signed_entity_type_id {
            ENTITY_TYPE_MITHRIL_STAKE_DISTRIBUTION => Ok(Self::MithrilStakeDistribution),
            ENTITY_TYPE_CARDANO_STAKE_DISTRIBUTION => Ok(Self::CardanoStakeDistribution),
            ENTITY_TYPE_CARDANO_TRANSACTIONS => Ok(Self::CardanoTransactions),
            ENTITY_TYPE_CARDANO_BLOCKS_TRANSACTIONS => Ok(Self::CardanoBlocksTransactions),
            ENTITY_TYPE_CARDANO_DATABASE => Ok(Self::CardanoDatabase),
            ENTITY_TYPE_CARDANO_NODE_LEDGER_STATE => Ok(Self::CardanoNodeLedgerState),
            index => Err(anyhow!("Invalid entity_type_id {index}.")),
        }
    }

    /// Parse the deduplicated list of signed entity type discriminants from a comma-separated
    /// string.
    ///
    /// Empty values are ignored, and each value is trimmed before parsing.
    ///
    /// A value is valid if it matches one of the [SignedEntityTypeDiscriminants] variants,
    /// with the exact same casing.
    ///
    /// This method does not fail. Unknown or incorrectly formed values are returned separately
    /// in [ParsedDiscriminants::ignored_values].
    pub fn parse_list<T: AsRef<str>>(discriminants_string: T) -> ParsedDiscriminants {
        let mut discriminants = BTreeSet::new();
        let mut ignored_values = Vec::new();

        for name in discriminants_string
            .as_ref()
            .split(',')
            .map(str::trim)
            .filter(|s| !s.is_empty())
        {
            match Self::from_str(name) {
                Ok(discriminant) => {
                    discriminants.insert(discriminant);
                }
                Err(_) => {
                    ignored_values.push(name.to_string());
                }
            }
        }

        ParsedDiscriminants {
            discriminants,
            ignored_values,
        }
    }

    /// Parse the deduplicated list of signed entity type discriminants from a comma-separated
    /// string.
    ///
    /// Empty values are ignored, and each value is trimmed before parsing.
    ///
    /// A value is valid if it matches one of the [SignedEntityTypeDiscriminants] variants,
    /// with the exact same casing.
    ///
    /// This method does not fail. Unknown or incorrectly formed values are discarded.
    pub fn parse_list_lossy<T: AsRef<str>>(discriminants_string: T) -> BTreeSet<Self> {
        Self::parse_list(discriminants_string).discriminants
    }

    /// Parse the deduplicated list of signed entity type discriminants from a comma-separated
    /// string.
    ///
    /// Empty values are ignored, and each value is trimmed before parsing.
    ///
    /// A value is valid if it matches one of the [SignedEntityTypeDiscriminants] variants,
    /// with the exact same casing.
    ///
    /// Returns an error if any value is unknown or incorrectly formed.
    pub fn parse_list_strict<T: AsRef<str>>(discriminants_string: T) -> StdResult<BTreeSet<Self>> {
        let parsed_discriminants = Self::parse_list(discriminants_string);

        if parsed_discriminants.ignored_values.is_empty() {
            Ok(parsed_discriminants.discriminants)
        } else {
            Err(anyhow!(Self::format_parse_list_error(
                &parsed_discriminants.ignored_values
            )))
        }
    }

    fn format_parse_list_error(invalid_discriminants: &[String]) -> String {
        format!(
            r#"Invalid signed entity types discriminants: {}.

Accepted values are (case-sensitive): {}."#,
            invalid_discriminants.join(", "),
            Self::accepted_discriminants()
        )
    }

    fn accepted_discriminants() -> String {
        Self::iter_all().map(|d| d.to_string()).collect::<Vec<_>>().join(", ")
    }
}

#[cfg(test)]
mod tests {
    use digest::Digest;

    use crate::test::assert_same_json;

    use super::*;

    fn hash(signed_entity_type: SignedEntityType) -> String {
        let mut hasher = Sha256::new();
        signed_entity_type.feed_hash(&mut hasher);
        hex::encode(hasher.finalize())
    }

    #[test]
    fn get_epoch_when_signed_entity_type_is_signed_for_cardano_stake_distribution_return_epoch_with_offset()
     {
        let signed_entity_type = SignedEntityType::CardanoStakeDistribution(Epoch(3));

        assert_eq!(
            signed_entity_type.get_epoch_when_signed_entity_type_is_signed(),
            Epoch(4)
        );
    }

    #[test]
    fn get_epoch_when_signed_entity_type_is_signed_for_mithril_stake_distribution_return_epoch_stored_in_signed_entity_type()
     {
        let signed_entity_type = SignedEntityType::MithrilStakeDistribution(Epoch(3));
        assert_eq!(
            signed_entity_type.get_epoch_when_signed_entity_type_is_signed(),
            Epoch(3)
        );
    }

    #[test]
    fn get_epoch_when_signed_entity_type_is_signed_for_cardano_transactions_return_epoch_stored_in_signed_entity_type()
     {
        let signed_entity_type = SignedEntityType::CardanoTransactions(Epoch(3), BlockNumber(77));
        assert_eq!(
            signed_entity_type.get_epoch_when_signed_entity_type_is_signed(),
            Epoch(3)
        );
    }

    #[test]
    fn get_epoch_when_signed_entity_type_is_signed_for_cardano_blocks_transactions_return_epoch_stored_in_signed_entity_type()
     {
        let signed_entity_type = SignedEntityType::CardanoBlocksTransactions(
            Epoch(5),
            BlockNumber(77),
            BlockNumberOffset(5),
        );
        assert_eq!(
            signed_entity_type.get_epoch_when_signed_entity_type_is_signed(),
            Epoch(5)
        );
    }

    #[test]
    fn get_epoch_when_signed_entity_type_is_signed_for_cardano_database_return_epoch_stored_in_signed_entity_type()
     {
        let signed_entity_type = SignedEntityType::CardanoDatabase(CardanoDbBeacon::new(12, 987));
        assert_eq!(
            signed_entity_type.get_epoch_when_signed_entity_type_is_signed(),
            Epoch(12)
        );
    }

    #[test]
    fn get_epoch_when_signed_entity_type_is_signed_for_cardano_node_ledger_state_return_epoch_stored_in_signed_entity_type()
     {
        let signed_entity_type =
            SignedEntityType::CardanoNodeLedgerState(CardanoDbBeacon::new(42, 123));
        assert_eq!(
            signed_entity_type.get_epoch_when_signed_entity_type_is_signed(),
            Epoch(42)
        );
    }

    #[test]
    fn verify_signed_entity_type_properties_are_included_in_computed_hash() {
        let reference_hash = hash(SignedEntityType::MithrilStakeDistribution(Epoch(5)));
        assert_ne!(
            reference_hash,
            hash(SignedEntityType::MithrilStakeDistribution(Epoch(15)))
        );

        let reference_hash = hash(SignedEntityType::CardanoStakeDistribution(Epoch(5)));
        assert_ne!(
            reference_hash,
            hash(SignedEntityType::CardanoStakeDistribution(Epoch(15)))
        );

        let reference_hash = hash(SignedEntityType::CardanoTransactions(
            Epoch(35),
            BlockNumber(77),
        ));
        assert_ne!(
            reference_hash,
            hash(SignedEntityType::CardanoTransactions(
                Epoch(3),
                BlockNumber(77)
            ))
        );
        assert_ne!(
            reference_hash,
            hash(SignedEntityType::CardanoTransactions(
                Epoch(35),
                BlockNumber(98765)
            ))
        );

        let reference_hash = hash(SignedEntityType::CardanoBlocksTransactions(
            Epoch(35),
            BlockNumber(77),
            BlockNumberOffset(5),
        ));
        assert_ne!(
            reference_hash,
            hash(SignedEntityType::CardanoBlocksTransactions(
                Epoch(3),
                BlockNumber(77),
                BlockNumberOffset(5),
            ))
        );
        assert_ne!(
            reference_hash,
            hash(SignedEntityType::CardanoBlocksTransactions(
                Epoch(35),
                BlockNumber(98765),
                BlockNumberOffset(5),
            ))
        );
        assert_ne!(
            reference_hash,
            hash(SignedEntityType::CardanoBlocksTransactions(
                Epoch(35),
                BlockNumber(77),
                BlockNumberOffset(123456),
            ))
        );

        let reference_hash = hash(SignedEntityType::CardanoDatabase(CardanoDbBeacon::new(
            12, 987,
        )));
        assert_ne!(
            reference_hash,
            hash(SignedEntityType::CardanoDatabase(CardanoDbBeacon::new(
                98, 987
            )))
        );
        assert_ne!(
            reference_hash,
            hash(SignedEntityType::CardanoDatabase(CardanoDbBeacon::new(
                12, 123
            )))
        );
    }

    //TODO: uncomment those tests cases when feeding database index is generalized to all types
    // #[test]
    // fn signed_entity_with_shared_beacons_computes_different_hashes() {
    //     assert_ne!(
    //         hash(SignedEntityType::MithrilStakeDistribution(Epoch(15))),
    //         hash(SignedEntityType::CardanoStakeDistribution(Epoch(15)))
    //     );
    // }

    #[test]
    fn get_open_message_timeout() {
        assert_eq!(
            SignedEntityType::MithrilStakeDistribution(Epoch(1)).get_open_message_timeout(),
            Some(Duration::from_secs(3600))
        );
        assert_eq!(
            SignedEntityType::CardanoStakeDistribution(Epoch(1)).get_open_message_timeout(),
            Some(Duration::from_secs(1800))
        );
        assert_eq!(
            SignedEntityType::CardanoTransactions(Epoch(1), BlockNumber(1))
                .get_open_message_timeout(),
            Some(Duration::from_secs(600))
        );
        assert_eq!(
            SignedEntityType::CardanoBlocksTransactions(
                Epoch(1),
                BlockNumber(1),
                BlockNumberOffset(1)
            )
            .get_open_message_timeout(),
            Some(Duration::from_secs(600))
        );
        assert_eq!(
            SignedEntityType::CardanoDatabase(CardanoDbBeacon::new(1, 1))
                .get_open_message_timeout(),
            Some(Duration::from_secs(600))
        );
        assert_eq!(
            SignedEntityType::CardanoNodeLedgerState(CardanoDbBeacon::new(1, 1))
                .get_open_message_timeout(),
            Some(Duration::from_secs(600))
        );
    }

    #[test]
    fn serialize_beacon_to_json() {
        let cardano_stake_distribution_json = SignedEntityType::CardanoStakeDistribution(Epoch(25))
            .get_json_beacon()
            .unwrap();
        assert_same_json!("25", &cardano_stake_distribution_json);

        let cardano_transactions_json =
            SignedEntityType::CardanoTransactions(Epoch(35), BlockNumber(77))
                .get_json_beacon()
                .unwrap();
        assert_same_json!(
            r#"{"epoch":35,"block_number":77}"#,
            &cardano_transactions_json
        );

        let cardano_transactions_json = SignedEntityType::CardanoBlocksTransactions(
            Epoch(35),
            BlockNumber(77),
            BlockNumberOffset(5),
        )
        .get_json_beacon()
        .unwrap();
        assert_same_json!(
            r#"{"epoch":35,"block_number":77,"block_number_offset":5}"#,
            &cardano_transactions_json
        );

        let msd_json = SignedEntityType::MithrilStakeDistribution(Epoch(15))
            .get_json_beacon()
            .unwrap();
        assert_same_json!("15", &msd_json);

        let cardano_database_full_json =
            SignedEntityType::CardanoDatabase(CardanoDbBeacon::new(12, 987))
                .get_json_beacon()
                .unwrap();
        assert_same_json!(
            r#"{"epoch":12,"immutable_file_number":987}"#,
            &cardano_database_full_json
        );

        let cardano_node_ledger_state_json =
            SignedEntityType::CardanoNodeLedgerState(CardanoDbBeacon::new(42, 123))
                .get_json_beacon()
                .unwrap();
        assert_same_json!(
            r#"{"epoch":42,"immutable_file_number":123}"#,
            &cardano_node_ledger_state_json
        );
    }

    #[test]
    fn bytes_encoding() {
        let cardano_stake_distribution = SignedEntityType::CardanoStakeDistribution(Epoch(25));
        let cardano_stake_distribution_bytes = cardano_stake_distribution.to_bytes_vec().unwrap();
        let cardano_stake_distribution_from_bytes =
            SignedEntityType::try_from_bytes(&cardano_stake_distribution_bytes).unwrap();

        assert_eq!(
            cardano_stake_distribution,
            cardano_stake_distribution_from_bytes
        );
    }

    #[test]
    fn iter_all_discriminants() {
        let discriminants: Vec<SignedEntityTypeDiscriminants> =
            SignedEntityTypeDiscriminants::iter_all().collect();

        assert_eq!(
            discriminants,
            vec![
                SignedEntityTypeDiscriminants::MithrilStakeDistribution,
                SignedEntityTypeDiscriminants::CardanoStakeDistribution,
                SignedEntityTypeDiscriminants::CardanoDatabase,
                SignedEntityTypeDiscriminants::CardanoTransactions,
                SignedEntityTypeDiscriminants::CardanoBlocksTransactions,
                SignedEntityTypeDiscriminants::CardanoNodeLedgerState,
            ]
        );
    }

    #[test]
    fn iter_all_discriminants_remove_unstable_values() {
        assert!(
            SignedEntityTypeDiscriminants::iter_all()
                .all(|d| !SignedEntityTypeDiscriminants::UNSTABLE_DISCRIMINANTS.contains(&d))
        );
    }

    // Expected ord:
    // MithrilStakeDistribution < CardanoStakeDistribution < CardanoDatabase < CardanoTransactions < CardanoBlocksTransactions
    #[test]
    fn ordering_discriminant() {
        let mut list = vec![
            SignedEntityTypeDiscriminants::CardanoStakeDistribution,
            SignedEntityTypeDiscriminants::CardanoDatabase,
            SignedEntityTypeDiscriminants::CardanoTransactions,
            SignedEntityTypeDiscriminants::CardanoBlocksTransactions,
            SignedEntityTypeDiscriminants::MithrilStakeDistribution,
        ];
        list.sort();

        assert_eq!(
            list,
            vec![
                SignedEntityTypeDiscriminants::MithrilStakeDistribution,
                SignedEntityTypeDiscriminants::CardanoStakeDistribution,
                SignedEntityTypeDiscriminants::CardanoDatabase,
                SignedEntityTypeDiscriminants::CardanoTransactions,
                SignedEntityTypeDiscriminants::CardanoBlocksTransactions,
            ]
        );
    }

    #[test]
    fn ordering_discriminant_with_duplicate() {
        let mut list = vec![
            SignedEntityTypeDiscriminants::CardanoDatabase,
            SignedEntityTypeDiscriminants::CardanoStakeDistribution,
            SignedEntityTypeDiscriminants::CardanoDatabase,
            SignedEntityTypeDiscriminants::MithrilStakeDistribution,
            SignedEntityTypeDiscriminants::CardanoTransactions,
            SignedEntityTypeDiscriminants::CardanoStakeDistribution,
            SignedEntityTypeDiscriminants::CardanoBlocksTransactions,
            SignedEntityTypeDiscriminants::MithrilStakeDistribution,
            SignedEntityTypeDiscriminants::MithrilStakeDistribution,
        ];
        list.sort();

        assert_eq!(
            list,
            vec![
                SignedEntityTypeDiscriminants::MithrilStakeDistribution,
                SignedEntityTypeDiscriminants::MithrilStakeDistribution,
                SignedEntityTypeDiscriminants::MithrilStakeDistribution,
                SignedEntityTypeDiscriminants::CardanoStakeDistribution,
                SignedEntityTypeDiscriminants::CardanoStakeDistribution,
                SignedEntityTypeDiscriminants::CardanoDatabase,
                SignedEntityTypeDiscriminants::CardanoDatabase,
                SignedEntityTypeDiscriminants::CardanoTransactions,
                SignedEntityTypeDiscriminants::CardanoBlocksTransactions,
            ]
        );
    }

    mod parse_list {
        use super::*;

        impl ParsedDiscriminants {
            fn new<const N: usize, T: Into<String>>(
                discriminants: [SignedEntityTypeDiscriminants; N],
                ignored: Vec<T>,
            ) -> Self {
                Self {
                    discriminants: BTreeSet::from(discriminants),
                    ignored_values: ignored.into_iter().map(Into::into).collect(),
                }
            }

            fn new_no_ignored<const N: usize>(
                discriminants: [SignedEntityTypeDiscriminants; N],
            ) -> Self {
                Self::new(discriminants, Vec::<String>::new())
            }
        }

        #[test]
        fn empty_string_should_return_empty_set() {
            for discriminants_str in ["", "  ", "   "] {
                let parse_result = SignedEntityTypeDiscriminants::parse_list(discriminants_str);

                assert_eq!(ParsedDiscriminants::new_no_ignored([]), parse_result);
            }
        }

        #[test]
        fn comma_separated_list_should_return_set_of_discriminants() {
            let discriminants_str = "MithrilStakeDistribution,CardanoDatabase";
            let parse_result = SignedEntityTypeDiscriminants::parse_list(discriminants_str);

            assert_eq!(
                ParsedDiscriminants::new_no_ignored([
                    SignedEntityTypeDiscriminants::MithrilStakeDistribution,
                    SignedEntityTypeDiscriminants::CardanoDatabase,
                ]),
                parse_result
            );
        }

        #[test]
        fn whitespaces_should_be_ignored() {
            let discriminants_str =
                "MithrilStakeDistribution    ,  CardanoDatabase  ,   CardanoTransactions   ";
            let parse_result = SignedEntityTypeDiscriminants::parse_list(discriminants_str);

            assert_eq!(
                ParsedDiscriminants::new_no_ignored([
                    SignedEntityTypeDiscriminants::MithrilStakeDistribution,
                    SignedEntityTypeDiscriminants::CardanoTransactions,
                    SignedEntityTypeDiscriminants::CardanoDatabase,
                ]),
                parse_result
            );
        }

        #[test]
        fn duplicate_discriminants_should_be_discarded() {
            let discriminants_str =
                "CardanoTransactions,CardanoTransactions,CardanoTransactions,CardanoTransactions";
            let parse_result = SignedEntityTypeDiscriminants::parse_list(discriminants_str);

            assert_eq!(
                ParsedDiscriminants::new_no_ignored([
                    SignedEntityTypeDiscriminants::CardanoTransactions
                ]),
                parse_result
            );
        }

        #[test]
        fn should_be_case_sensitive() {
            let discriminants_str = "mithrilstakedistribution,CARDANODATABASE";
            let parse_result = SignedEntityTypeDiscriminants::parse_list(discriminants_str);

            assert_eq!(
                ParsedDiscriminants::new(
                    [],
                    vec!["mithrilstakedistribution".to_string(), "CARDANODATABASE".to_string()]
                ),
                parse_result
            );
        }

        #[test]
        fn should_not_return_unknown_signed_entity_types() {
            let discriminants_str = "Unknown";
            let parse_result = SignedEntityTypeDiscriminants::parse_list(discriminants_str);

            assert_eq!(
                ParsedDiscriminants::new([], vec!["Unknown".to_string()]),
                parse_result
            );
        }

        #[test]
        fn separates_valid_discriminants_from_invalid_values() {
            let discriminants_str = "CardanoTransactions,Invalid,MithrilStakeDistribution";
            let parse_result = SignedEntityTypeDiscriminants::parse_list(discriminants_str);

            assert_eq!(
                ParsedDiscriminants::new(
                    [
                        SignedEntityTypeDiscriminants::MithrilStakeDistribution,
                        SignedEntityTypeDiscriminants::CardanoTransactions,
                    ],
                    vec!["Invalid".to_string()]
                ),
                parse_result
            );
        }

        #[test]
        fn lossy_parsing_discards_invalid_values() {
            let discriminants_str = "CardanoTransactions,Invalid,MithrilStakeDistribution";
            let parse_result = SignedEntityTypeDiscriminants::parse_list_lossy(discriminants_str);

            assert_eq!(
                BTreeSet::from([
                    SignedEntityTypeDiscriminants::MithrilStakeDistribution,
                    SignedEntityTypeDiscriminants::CardanoTransactions,
                ],),
                parse_result
            );
        }

        #[test]
        fn parsed_discriminant_has_ignored_values() {
            assert!(!ParsedDiscriminants::new_no_ignored([]).has_ignored_values());
            assert!(ParsedDiscriminants::new([], vec!["ignored".to_string()]).has_ignored_values());
        }

        #[test]
        fn strict_parsing_fails_on_invalid_values() {
            let discriminants_str = "CardanoTransactions,Invalid,MithrilStakeDistribution";
            let error =
                SignedEntityTypeDiscriminants::parse_list_strict(discriminants_str).unwrap_err();

            assert_eq!(
                SignedEntityTypeDiscriminants::format_parse_list_error(&["Invalid".to_string()]),
                error.to_string()
            );
        }

        #[test]
        fn parse_list_error_is_human_readable() {
            let invalid_discriminants = vec!["Unknown".to_string(), "Invalid".to_string()];
            let error =
                SignedEntityTypeDiscriminants::format_parse_list_error(&invalid_discriminants);

            assert_eq!(
                format!(
                    r#"Invalid signed entity types discriminants: Unknown, Invalid.

Accepted values are (case-sensitive): {}."#,
                    SignedEntityTypeDiscriminants::accepted_discriminants()
                ),
                error
            );
        }
    }

    #[test]
    fn discriminants_index_returns_correct_database_value() {
        assert_eq!(
            SignedEntityTypeDiscriminants::MithrilStakeDistribution.index(),
            ENTITY_TYPE_MITHRIL_STAKE_DISTRIBUTION
        );
        assert_eq!(
            SignedEntityTypeDiscriminants::CardanoStakeDistribution.index(),
            ENTITY_TYPE_CARDANO_STAKE_DISTRIBUTION
        );
        assert_eq!(
            SignedEntityTypeDiscriminants::CardanoTransactions.index(),
            ENTITY_TYPE_CARDANO_TRANSACTIONS
        );
        assert_eq!(
            SignedEntityTypeDiscriminants::CardanoBlocksTransactions.index(),
            ENTITY_TYPE_CARDANO_BLOCKS_TRANSACTIONS
        );
        assert_eq!(
            SignedEntityTypeDiscriminants::CardanoDatabase.index(),
            ENTITY_TYPE_CARDANO_DATABASE
        );
        assert_eq!(
            SignedEntityTypeDiscriminants::CardanoNodeLedgerState.index(),
            ENTITY_TYPE_CARDANO_NODE_LEDGER_STATE
        );
    }
}
