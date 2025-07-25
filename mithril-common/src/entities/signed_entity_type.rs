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
};

use super::{BlockNumber, CardanoDbBeacon, Epoch};

/// Database representation of the SignedEntityType::MithrilStakeDistribution value
const ENTITY_TYPE_MITHRIL_STAKE_DISTRIBUTION: usize = 0;

/// Database representation of the SignedEntityType::CardanoStakeDistribution value
const ENTITY_TYPE_CARDANO_STAKE_DISTRIBUTION: usize = 1;

/// Database representation of the SignedEntityType::CardanoImmutableFilesFull value
const ENTITY_TYPE_CARDANO_IMMUTABLE_FILES_FULL: usize = 2;

/// Database representation of the SignedEntityType::CardanoTransactions value
const ENTITY_TYPE_CARDANO_TRANSACTIONS: usize = 3;

/// Database representation of the SignedEntityType::CardanoDatabase value
const ENTITY_TYPE_CARDANO_DATABASE: usize = 4;

/// The signed entity type that represents a type of data signed by the Mithril
/// protocol Note: Each variant of this enum must be associated to an entry in
/// the `signed_entity_type` table of the signer/aggregator nodes. The variant
/// are identified by their discriminant (i.e. index in the enum), thus the
/// modification of this type should only ever consist of appending new
/// variants.
// Important note: The order of the variants is important as it is used for the derived Ord trait.
#[derive(Display, Debug, Clone, PartialEq, Eq, Serialize, Deserialize, EnumDiscriminants)]
#[strum(serialize_all = "PascalCase")]
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

    /// Full Cardano Immutable Files
    CardanoImmutableFilesFull(CardanoDbBeacon),

    /// Cardano Database
    CardanoDatabase(CardanoDbBeacon),

    /// Cardano Transactions
    CardanoTransactions(Epoch, BlockNumber),
}

impl SignedEntityType {
    /// Create a new signed entity type for a genesis certificate (a [Self::MithrilStakeDistribution])
    pub fn genesis(epoch: Epoch) -> Self {
        Self::MithrilStakeDistribution(epoch)
    }

    /// Return the epoch from the signed entity.
    pub fn get_epoch(&self) -> Epoch {
        match self {
            Self::CardanoImmutableFilesFull(b) | Self::CardanoDatabase(b) => b.epoch,
            Self::CardanoStakeDistribution(e)
            | Self::MithrilStakeDistribution(e)
            | Self::CardanoTransactions(e, _) => *e,
        }
    }

    /// Return the epoch at which the signed entity type is signed.
    pub fn get_epoch_when_signed_entity_type_is_signed(&self) -> Epoch {
        match self {
            Self::CardanoImmutableFilesFull(beacon) | Self::CardanoDatabase(beacon) => beacon.epoch,
            Self::CardanoStakeDistribution(epoch) => epoch.next(),
            Self::MithrilStakeDistribution(epoch) | Self::CardanoTransactions(epoch, _) => *epoch,
        }
    }

    /// Get the database value from enum's instance
    pub fn index(&self) -> usize {
        match self {
            Self::MithrilStakeDistribution(_) => ENTITY_TYPE_MITHRIL_STAKE_DISTRIBUTION,
            Self::CardanoStakeDistribution(_) => ENTITY_TYPE_CARDANO_STAKE_DISTRIBUTION,
            Self::CardanoImmutableFilesFull(_) => ENTITY_TYPE_CARDANO_IMMUTABLE_FILES_FULL,
            Self::CardanoTransactions(_, _) => ENTITY_TYPE_CARDANO_TRANSACTIONS,
            Self::CardanoDatabase(_) => ENTITY_TYPE_CARDANO_DATABASE,
        }
    }

    /// Return a JSON serialized value of the internal beacon
    pub fn get_json_beacon(&self) -> StdResult<String> {
        let value = match self {
            Self::CardanoImmutableFilesFull(value) | Self::CardanoDatabase(value) => {
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
        };

        Ok(value)
    }

    /// Return the associated open message timeout
    pub fn get_open_message_timeout(&self) -> Option<Duration> {
        match self {
            Self::MithrilStakeDistribution(_) | Self::CardanoImmutableFilesFull(_) => None,
            Self::CardanoStakeDistribution(_) => Some(Duration::from_secs(600)),
            Self::CardanoTransactions(_, _) => Some(Duration::from_secs(1800)),
            Self::CardanoDatabase(_) => Some(Duration::from_secs(1800)),
        }
    }

    pub(crate) fn feed_hash(&self, hasher: &mut Sha256) {
        match self {
            SignedEntityType::MithrilStakeDistribution(epoch)
            | SignedEntityType::CardanoStakeDistribution(epoch) => {
                hasher.update(&epoch.to_be_bytes())
            }
            SignedEntityType::CardanoImmutableFilesFull(db_beacon)
            | SignedEntityType::CardanoDatabase(db_beacon) => {
                hasher.update(&db_beacon.epoch.to_be_bytes());
                hasher.update(&db_beacon.immutable_file_number.to_be_bytes());
            }
            SignedEntityType::CardanoTransactions(epoch, block_number) => {
                hasher.update(&epoch.to_be_bytes());
                hasher.update(&block_number.to_be_bytes())
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

impl SignedEntityTypeDiscriminants {
    /// Get all the discriminants
    pub fn all() -> BTreeSet<Self> {
        SignedEntityTypeDiscriminants::iter().collect()
    }

    /// Get the database value from enum's instance
    pub fn index(&self) -> usize {
        match self {
            Self::MithrilStakeDistribution => ENTITY_TYPE_MITHRIL_STAKE_DISTRIBUTION,
            Self::CardanoStakeDistribution => ENTITY_TYPE_CARDANO_STAKE_DISTRIBUTION,
            Self::CardanoImmutableFilesFull => ENTITY_TYPE_CARDANO_IMMUTABLE_FILES_FULL,
            Self::CardanoTransactions => ENTITY_TYPE_CARDANO_TRANSACTIONS,
            Self::CardanoDatabase => ENTITY_TYPE_CARDANO_DATABASE,
        }
    }

    /// Get the discriminant associated with the given id
    pub fn from_id(signed_entity_type_id: usize) -> StdResult<SignedEntityTypeDiscriminants> {
        match signed_entity_type_id {
            ENTITY_TYPE_MITHRIL_STAKE_DISTRIBUTION => Ok(Self::MithrilStakeDistribution),
            ENTITY_TYPE_CARDANO_STAKE_DISTRIBUTION => Ok(Self::CardanoStakeDistribution),
            ENTITY_TYPE_CARDANO_IMMUTABLE_FILES_FULL => Ok(Self::CardanoImmutableFilesFull),
            ENTITY_TYPE_CARDANO_TRANSACTIONS => Ok(Self::CardanoTransactions),
            ENTITY_TYPE_CARDANO_DATABASE => Ok(Self::CardanoDatabase),
            index => Err(anyhow!("Invalid entity_type_id {index}.")),
        }
    }

    /// Parse the deduplicated list of signed entity types discriminants from a comma separated
    /// string.
    ///
    /// Unknown or incorrectly formed values are ignored.
    pub fn parse_list<T: AsRef<str>>(discriminants_string: T) -> StdResult<BTreeSet<Self>> {
        let mut discriminants = BTreeSet::new();
        let mut invalid_discriminants = Vec::new();

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
                    invalid_discriminants.push(name);
                }
            }
        }

        if invalid_discriminants.is_empty() {
            Ok(discriminants)
        } else {
            Err(anyhow!(Self::format_parse_list_error(
                invalid_discriminants
            )))
        }
    }

    fn format_parse_list_error(invalid_discriminants: Vec<&str>) -> String {
        format!(
            r#"Invalid signed entity types discriminants: {}.

Accepted values are (case-sensitive): {}."#,
            invalid_discriminants.join(", "),
            Self::accepted_discriminants()
        )
    }

    fn accepted_discriminants() -> String {
        Self::iter().map(|d| d.to_string()).collect::<Vec<_>>().join(", ")
    }
}

#[cfg(test)]
mod tests {
    use digest::Digest;

    use crate::test::assert_same_json;

    use super::*;

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
    fn get_epoch_when_signed_entity_type_is_signed_for_cardano_immutable_files_full_return_epoch_stored_in_signed_entity_type()
     {
        let signed_entity_type =
            SignedEntityType::CardanoImmutableFilesFull(CardanoDbBeacon::new(3, 100));
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
    fn get_epoch_when_signed_entity_type_is_signed_for_cardano_database_return_epoch_stored_in_signed_entity_type()
     {
        let signed_entity_type = SignedEntityType::CardanoDatabase(CardanoDbBeacon::new(12, 987));
        assert_eq!(
            signed_entity_type.get_epoch_when_signed_entity_type_is_signed(),
            Epoch(12)
        );
    }

    #[test]
    fn verify_signed_entity_type_properties_are_included_in_computed_hash() {
        fn hash(signed_entity_type: SignedEntityType) -> String {
            let mut hasher = Sha256::new();
            signed_entity_type.feed_hash(&mut hasher);
            hex::encode(hasher.finalize())
        }

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

        let reference_hash = hash(SignedEntityType::CardanoImmutableFilesFull(
            CardanoDbBeacon::new(5, 100),
        ));
        assert_ne!(
            reference_hash,
            hash(SignedEntityType::CardanoImmutableFilesFull(
                CardanoDbBeacon::new(20, 100)
            ))
        );
        assert_ne!(
            reference_hash,
            hash(SignedEntityType::CardanoImmutableFilesFull(
                CardanoDbBeacon::new(5, 507)
            ))
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

        let cardano_immutable_files_full_json =
            SignedEntityType::CardanoImmutableFilesFull(CardanoDbBeacon::new(5, 100))
                .get_json_beacon()
                .unwrap();
        assert_same_json!(
            r#"{"epoch":5,"immutable_file_number":100}"#,
            &cardano_immutable_files_full_json
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

    // Expected ord:
    // MithrilStakeDistribution < CardanoStakeDistribution < CardanoImmutableFilesFull < CardanoDatabase < CardanoTransactions
    #[test]
    fn ordering_discriminant() {
        let mut list = vec![
            SignedEntityTypeDiscriminants::CardanoStakeDistribution,
            SignedEntityTypeDiscriminants::CardanoDatabase,
            SignedEntityTypeDiscriminants::CardanoTransactions,
            SignedEntityTypeDiscriminants::CardanoImmutableFilesFull,
            SignedEntityTypeDiscriminants::MithrilStakeDistribution,
        ];
        list.sort();

        assert_eq!(
            list,
            vec![
                SignedEntityTypeDiscriminants::MithrilStakeDistribution,
                SignedEntityTypeDiscriminants::CardanoStakeDistribution,
                SignedEntityTypeDiscriminants::CardanoImmutableFilesFull,
                SignedEntityTypeDiscriminants::CardanoDatabase,
                SignedEntityTypeDiscriminants::CardanoTransactions,
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
            SignedEntityTypeDiscriminants::CardanoImmutableFilesFull,
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
                SignedEntityTypeDiscriminants::CardanoImmutableFilesFull,
                SignedEntityTypeDiscriminants::CardanoDatabase,
                SignedEntityTypeDiscriminants::CardanoDatabase,
                SignedEntityTypeDiscriminants::CardanoTransactions,
            ]
        );
    }

    #[test]
    fn parse_signed_entity_types_discriminants_discriminant_without_values() {
        let discriminants_str = "";
        let discriminants = SignedEntityTypeDiscriminants::parse_list(discriminants_str).unwrap();

        assert_eq!(BTreeSet::new(), discriminants);

        let discriminants_str = "     ";
        let discriminants = SignedEntityTypeDiscriminants::parse_list(discriminants_str).unwrap();

        assert_eq!(BTreeSet::new(), discriminants);
    }

    #[test]
    fn parse_signed_entity_types_discriminants_with_correctly_formed_values() {
        let discriminants_str = "MithrilStakeDistribution,CardanoImmutableFilesFull";
        let discriminants = SignedEntityTypeDiscriminants::parse_list(discriminants_str).unwrap();

        assert_eq!(
            BTreeSet::from([
                SignedEntityTypeDiscriminants::MithrilStakeDistribution,
                SignedEntityTypeDiscriminants::CardanoImmutableFilesFull,
            ]),
            discriminants
        );
    }

    #[test]
    fn parse_signed_entity_types_discriminants_should_trim_values() {
        let discriminants_str =
            "MithrilStakeDistribution    ,  CardanoImmutableFilesFull  ,   CardanoTransactions   ";
        let discriminants = SignedEntityTypeDiscriminants::parse_list(discriminants_str).unwrap();

        assert_eq!(
            BTreeSet::from([
                SignedEntityTypeDiscriminants::MithrilStakeDistribution,
                SignedEntityTypeDiscriminants::CardanoTransactions,
                SignedEntityTypeDiscriminants::CardanoImmutableFilesFull,
            ]),
            discriminants
        );
    }

    #[test]
    fn parse_signed_entity_types_discriminants_should_remove_duplicates() {
        let discriminants_str =
            "CardanoTransactions,CardanoTransactions,CardanoTransactions,CardanoTransactions";
        let discriminant = SignedEntityTypeDiscriminants::parse_list(discriminants_str).unwrap();

        assert_eq!(
            BTreeSet::from([SignedEntityTypeDiscriminants::CardanoTransactions]),
            discriminant
        );
    }

    #[test]
    fn parse_signed_entity_types_discriminants_should_be_case_sensitive() {
        let discriminants_str = "mithrilstakedistribution,CARDANOIMMUTABLEFILESFULL";
        let error = SignedEntityTypeDiscriminants::parse_list(discriminants_str).unwrap_err();

        assert_eq!(
            SignedEntityTypeDiscriminants::format_parse_list_error(vec![
                "mithrilstakedistribution",
                "CARDANOIMMUTABLEFILESFULL"
            ]),
            error.to_string()
        );
    }

    #[test]
    fn parse_signed_entity_types_discriminants_should_not_return_unknown_signed_entity_types() {
        let discriminants_str = "Unknown";
        let error = SignedEntityTypeDiscriminants::parse_list(discriminants_str).unwrap_err();

        assert_eq!(
            SignedEntityTypeDiscriminants::format_parse_list_error(vec!["Unknown"]),
            error.to_string()
        );
    }

    #[test]
    fn parse_signed_entity_types_discriminants_should_fail_if_there_is_at_least_one_invalid_value()
    {
        let discriminants_str = "CardanoTransactions,Invalid,MithrilStakeDistribution";
        let error = SignedEntityTypeDiscriminants::parse_list(discriminants_str).unwrap_err();

        assert_eq!(
            SignedEntityTypeDiscriminants::format_parse_list_error(vec!["Invalid"]),
            error.to_string()
        );
    }

    #[test]
    fn parse_list_error_format_to_an_useful_message() {
        let invalid_discriminants = vec!["Unknown", "Invalid"];
        let error = SignedEntityTypeDiscriminants::format_parse_list_error(invalid_discriminants);

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
