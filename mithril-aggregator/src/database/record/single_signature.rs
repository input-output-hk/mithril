use chrono::{DateTime, Utc};
use uuid::Uuid;

use mithril_common::entities::{Epoch, HexEncodedSingleSignature, LotteryIndex, SingleSignatures};
use mithril_common::{StdError, StdResult};
use mithril_persistence::sqlite::{HydrationError, Projection, SqLiteEntity};

/// SingleSignature record is the representation of a stored single_signature.
#[derive(Debug, PartialEq, Clone)]
pub struct SingleSignatureRecord {
    /// Open message id.
    pub open_message_id: Uuid,

    /// Signer id.
    pub signer_id: String,

    /// Registration epoch setting id
    pub registration_epoch_setting_id: Epoch,

    /// Lottery indexes
    pub lottery_indexes: Vec<LotteryIndex>,

    /// The STM single signature of the message
    pub signature: HexEncodedSingleSignature,

    /// Date and time when the single_signature was created
    pub created_at: DateTime<Utc>,
}

impl SingleSignatureRecord {
    pub(crate) fn try_from_single_signatures(
        other: &SingleSignatures,
        open_message_id: &Uuid,
        registration_epoch_setting_id: Epoch,
    ) -> StdResult<Self> {
        let record = SingleSignatureRecord {
            open_message_id: open_message_id.to_owned(),
            signer_id: other.party_id.to_owned(),
            registration_epoch_setting_id,
            lottery_indexes: other.won_indexes.to_owned(),
            signature: other.signature.to_json_hex()?,
            created_at: Utc::now(),
        };

        Ok(record)
    }
}

impl TryFrom<SingleSignatureRecord> for SingleSignatures {
    type Error = StdError;

    fn try_from(value: SingleSignatureRecord) -> Result<Self, Self::Error> {
        let signatures = SingleSignatures {
            party_id: value.signer_id,
            won_indexes: value.lottery_indexes,
            signature: value.signature.try_into()?,
        };

        Ok(signatures)
    }
}

impl SqLiteEntity for SingleSignatureRecord {
    fn hydrate(row: sqlite::Row) -> Result<Self, HydrationError>
    where
        Self: Sized,
    {
        let open_message_id = row.read::<&str, _>(0);
        let open_message_id = Uuid::parse_str(open_message_id).map_err(|e| {
            HydrationError::InvalidData(format!(
                "Invalid UUID in single_signature.open_message_id: '{open_message_id}'. Error: {e}"
            ))
        })?;
        let signer_id = row.read::<&str, _>(1).to_string();
        let registration_epoch_setting_id_int = row.read::<i64, _>(2);
        let lottery_indexes_str = row.read::<&str, _>(3);
        let signature = row.read::<&str, _>(4).to_string();
        let created_at = row.read::<&str, _>(5);

        let single_signature_record = Self {
            open_message_id,
            signer_id,
            registration_epoch_setting_id: Epoch(
                registration_epoch_setting_id_int.try_into().map_err(|e| {
                    HydrationError::InvalidData(format!(
                        "Could not cast i64 ({registration_epoch_setting_id_int}) to u64. Error: '{e}'"
                    ))
                })?,
            ),
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

        Ok(single_signature_record)
    }

    fn get_projection() -> Projection {
        let mut projection = Projection::default();
        projection.add_field(
            "open_message_id",
            "{:single_signature:}.open_message_id",
            "text",
        );
        projection.add_field("signer_id", "{:single_signature:}.signer_id", "text");
        projection.add_field(
            "registration_epoch_setting_id",
            "{:single_signature:}.registration_epoch_setting_id",
            "integer",
        );
        projection.add_field(
            "lottery_indexes",
            "{:single_signature:}.lottery_indexes",
            "text",
        );
        projection.add_field("signature", "{:single_signature:}.signature", "text");
        projection.add_field("created_at", "{:single_signature:}.created_at", "text");

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
        let open_message_id = Uuid::parse_str("193d1442-e89b-43cf-9519-04d8db9a12ff").unwrap();
        let single_signature_record = SingleSignatureRecord::try_from_single_signatures(
            &single_signature,
            &open_message_id,
            Epoch(1),
        )
        .unwrap();
        let single_signature_returned = single_signature_record.try_into().unwrap();

        assert_eq!(single_signature, single_signature_returned);
    }
}
