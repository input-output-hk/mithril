use chrono::{DateTime, Utc};

use mithril_common::entities::{
    HexEncodedSingleSignature, LotteryIndex, SignedEntityTypeDiscriminants, SingleSignature,
};
use mithril_common::{StdError, StdResult};
use mithril_persistence::sqlite::{HydrationError, Projection, SqLiteEntity};

/// `BufferedSingleSignatureRecord` record is the representation of a buffered single_signature
/// that may be used for upcoming open messages.
#[derive(Debug, PartialEq, Clone)]
pub struct BufferedSingleSignatureRecord {
    /// Party id.
    pub party_id: String,

    /// Signed entity type discriminant.
    pub signed_entity_type_id: SignedEntityTypeDiscriminants,

    /// Lottery indexes
    pub lottery_indexes: Vec<LotteryIndex>,

    /// The STM single signature of the message
    pub signature: HexEncodedSingleSignature,

    /// Date and time when the buffered single signature was created
    pub created_at: DateTime<Utc>,
}

impl BufferedSingleSignatureRecord {
    pub(crate) fn try_from_single_signature(
        other: &SingleSignature,
        signed_entity_id: SignedEntityTypeDiscriminants,
    ) -> StdResult<Self> {
        let record = BufferedSingleSignatureRecord {
            signed_entity_type_id: signed_entity_id,
            party_id: other.party_id.to_owned(),
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
        discriminant: SignedEntityTypeDiscriminants,
    ) -> Self {
        // Note: due to the unique constraint on the signature column, we want to make sure that
        // the signature are different for party_id/discriminant pairs.
        // We can't just reuse fake_data::single_signature as they are static.
        Self::try_from_single_signature(
            &SingleSignature::fake(party_id.into(), discriminant.to_string()),
            discriminant,
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

impl TryFrom<BufferedSingleSignatureRecord> for SingleSignature {
    type Error = StdError;

    fn try_from(value: BufferedSingleSignatureRecord) -> Result<Self, Self::Error> {
        let signature = SingleSignature {
            party_id: value.party_id,
            won_indexes: value.lottery_indexes,
            signature: value.signature.try_into()?,
            authentication_status: Default::default(),
        };

        Ok(signature)
    }
}

impl SqLiteEntity for BufferedSingleSignatureRecord {
    fn hydrate(row: sqlite::Row) -> Result<Self, HydrationError>
    where
        Self: Sized,
    {
        let signed_entity_type_id = usize::try_from(row.read::<i64, _>(0)).map_err(|e| {
            panic!("Integer field signed_entity_type_id cannot be turned into usize: {e}")
        })?;
        let party_id = row.read::<&str, _>(1).to_string();
        let lottery_indexes_str = row.read::<&str, _>(2);
        let signature = row.read::<&str, _>(3).to_string();
        let created_at = row.read::<&str, _>(4);

        let record = Self {
            party_id,
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
        projection.add_field(
            "signed_entity_type_id",
            "{:buffered_single_signature:}.signed_entity_type_id",
            "integer",
        );
        projection.add_field("party_id", "{:buffered_single_signature:}.party_id", "text");
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

/// Test only - strip the date from the given records to make them comparable.
#[cfg(test)]
pub(crate) fn strip_buffered_sigs_date(
    records: &[BufferedSingleSignatureRecord],
) -> Vec<BufferedSingleSignatureRecord> {
    records
        .iter()
        .map(BufferedSingleSignatureRecord::with_stripped_date)
        .collect::<Vec<_>>()
}

#[cfg(test)]
mod tests {
    use mithril_common::entities::SignedEntityTypeDiscriminants::{
        CardanoTransactions, MithrilStakeDistribution,
    };
    use mithril_common::test::double::fake_data;

    use super::*;

    #[test]
    fn test_convert_single_signature() {
        let single_signature = fake_data::single_signature(vec![1, 3, 4, 6, 7, 9]);
        let single_signature_record = BufferedSingleSignatureRecord::try_from_single_signature(
            &single_signature,
            CardanoTransactions,
        )
        .unwrap();
        let single_signature_returned = single_signature_record.try_into().unwrap();

        assert_eq!(single_signature, single_signature_returned);
    }

    #[test]
    fn building_fake_generate_different_protocol_single_signature() {
        assert_eq!(
            BufferedSingleSignatureRecord::fake("party_1", CardanoTransactions).signature,
            BufferedSingleSignatureRecord::fake("party_1", CardanoTransactions).signature
        );

        assert_ne!(
            BufferedSingleSignatureRecord::fake("party_1", CardanoTransactions).signature,
            BufferedSingleSignatureRecord::fake("party_2", CardanoTransactions).signature
        );
        assert_ne!(
            BufferedSingleSignatureRecord::fake("party_1", CardanoTransactions).signature,
            BufferedSingleSignatureRecord::fake("party_1", MithrilStakeDistribution).signature
        );
    }
}
