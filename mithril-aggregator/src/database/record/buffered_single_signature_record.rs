use chrono::{DateTime, Utc};

use mithril_common::entities::{
    Epoch, HexEncodedSingleSignature, LotteryIndex, SignedEntityTypeDiscriminants, SingleSignatures,
};
use mithril_common::{StdError, StdResult};
use mithril_persistence::sqlite::{HydrationError, Projection, SqLiteEntity};

/// `BufferedSingleSignatureRecord` record is the representation of a buffered single_signature
/// that may be used for upcoming open messages.
#[derive(Debug, PartialEq, Clone)]
pub struct BufferedSingleSignatureRecord {
    /// Party id.
    pub party_id: String,

    /// Epoch at which the buffered single signature was created
    pub epoch: Epoch,

    /// Signed entity type discriminant.
    pub signed_entity_type_id: SignedEntityTypeDiscriminants,

    /// Lottery indexes
    pub lottery_indexes: Vec<LotteryIndex>,

    /// The STM single signature of the message
    pub signature: HexEncodedSingleSignature,

    /// Date and time when the single_signature was created
    pub created_at: DateTime<Utc>,
}

impl BufferedSingleSignatureRecord {
    pub(crate) fn try_from_single_signatures(
        other: &SingleSignatures,
        signed_entity_id: SignedEntityTypeDiscriminants,
        epoch: Epoch,
    ) -> StdResult<Self> {
        let record = BufferedSingleSignatureRecord {
            signed_entity_type_id: signed_entity_id,
            party_id: other.party_id.to_owned(),
            epoch,
            lottery_indexes: other.won_indexes.to_owned(),
            signature: other.signature.to_json_hex()?,
            created_at: Utc::now(),
        };

        Ok(record)
    }
}

#[cfg(test)]
impl BufferedSingleSignatureRecord {
    pub(crate) fn fake<T: Into<String>>(
        party_id: T,
        discriminants: SignedEntityTypeDiscriminants,
    ) -> Self {
        use mithril_common::test_utils::fake_keys;

        let signature = fake_keys::single_signature()[0].try_into().unwrap();
        Self::try_from_single_signatures(
            &SingleSignatures {
                party_id: party_id.into(),
                signature,
                won_indexes: vec![10, 15],
                signed_message: None,
            },
            discriminants,
            Epoch(12),
        )
        .unwrap()
    }

    pub(crate) fn fakes<T: Into<String> + Clone>(
        input: &[(T, SignedEntityTypeDiscriminants)],
    ) -> Vec<Self> {
        input
            .iter()
            .map(|(party_id, discriminants)| Self::fake(party_id.clone(), *discriminants))
            .collect()
    }

    /// Returns a copy of the record with the date replaced by "1st of January 1970".
    pub(crate) fn with_stripped_date(&self) -> Self {
        Self {
            created_at: DateTime::<Utc>::default(),
            ..self.clone()
        }
    }
}

impl TryFrom<BufferedSingleSignatureRecord> for SingleSignatures {
    type Error = StdError;

    fn try_from(value: BufferedSingleSignatureRecord) -> Result<Self, Self::Error> {
        let signatures = SingleSignatures {
            party_id: value.party_id,
            won_indexes: value.lottery_indexes,
            signature: value.signature.try_into()?,
            signed_message: None,
        };

        Ok(signatures)
    }
}

impl SqLiteEntity for BufferedSingleSignatureRecord {
    fn hydrate(row: sqlite::Row) -> Result<Self, HydrationError>
    where
        Self: Sized,
    {
        let party_id = row.read::<&str, _>(0).to_string();
        let epoch = row.read::<i64, _>(1);
        let signed_entity_type_id = usize::try_from(row.read::<i64, _>(2)).map_err(|e| {
            panic!("Integer field signed_entity_type_id cannot be turned into usize: {e}")
        })?;
        let lottery_indexes_str = row.read::<&str, _>(3);
        let signature = row.read::<&str, _>(4).to_string();
        let created_at = row.read::<&str, _>(5);

        let record = Self {
            party_id,
            epoch: Epoch(
                epoch.try_into().map_err(|e| {
                    HydrationError::InvalidData(format!(
                        "Could not cast i64 ({epoch}) to u64. Error: '{e}'"
                    ))
                })?,
            ),
            signed_entity_type_id: SignedEntityTypeDiscriminants::from_id(signed_entity_type_id).map_err(
                |e| {
                    HydrationError::InvalidData(format!(
                        "Could not turn i64 ({signed_entity_type_id}) to SignedEntityTypeDiscriminants. Error: '{e}'"
                    ))
                },
            )?,
            lottery_indexes: serde_json::from_str(lottery_indexes_str).map_err(|e| {
                HydrationError::InvalidData(format!(
                    "Could not turn string '{lottery_indexes_str}' to Vec<LotteryIndex>. Error: {e}"
                ))
            })?,
            signature,
            created_at: DateTime::parse_from_rfc3339(created_at)
                .map_err(|e| {
                    HydrationError::InvalidData(format!(
                        "Could not turn string '{created_at}' to rfc3339 Datetime. Error: {e}"
                    ))
                })?
                .with_timezone(&Utc),
        };

        Ok(record)
    }

    fn get_projection() -> Projection {
        let mut projection = Projection::default();
        projection.add_field("party_id", "{:buffered_single_signature:}.party_id", "text");
        projection.add_field("epoch", "{:buffered_single_signature:}.epoch", "integer");
        projection.add_field(
            "signed_entity_type_id",
            "{:buffered_single_signature:}.signed_entity_type_id",
            "integer",
        );
        projection.add_field(
            "lottery_indexes",
            "{:buffered_single_signature:}.lottery_indexes",
            "text",
        );
        projection.add_field(
            "signature",
            "{:buffered_single_signature:}.signature",
            "text",
        );
        projection.add_field(
            "created_at",
            "{:buffered_single_signature:}.created_at",
            "text",
        );

        projection
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::test_utils::fake_data;

    use super::*;

    #[test]
    fn test_convert_single_signatures() {
        let single_signature = fake_data::single_signatures(vec![1, 3, 4, 6, 7, 9]);
        let single_signature_record = BufferedSingleSignatureRecord::try_from_single_signatures(
            &single_signature,
            SignedEntityTypeDiscriminants::CardanoTransactions,
            Epoch(1),
        )
        .unwrap();
        let single_signature_returned = single_signature_record.try_into().unwrap();

        assert_eq!(single_signature, single_signature_returned);
    }
}
