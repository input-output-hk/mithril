use chrono::{DateTime, Utc};

use mithril_common::crypto_helper::KESPeriod;
use mithril_common::entities::{
    Epoch, HexEncodedOpCert, HexEncodedVerificationKey, HexEncodedVerificationKeySignature, Signer,
    SignerWithStake, Stake,
};
use mithril_persistence::sqlite::{HydrationError, Projection, SqLiteEntity};

/// SignerRegistration record is the representation of a stored signer_registration.
#[derive(Debug, PartialEq, Clone)]
pub struct SignerRegistrationRecord {
    /// Signer id.
    pub signer_id: String,

    /// Epoch of creation of the signer_registration.
    pub epoch_setting_id: Epoch,

    /// Verification key of the signer
    pub verification_key: HexEncodedVerificationKey,

    /// Signature of the verification key of the signer
    pub verification_key_signature: Option<HexEncodedVerificationKeySignature>,

    /// Operational certificate of the stake pool operator associated to the signer
    pub operational_certificate: Option<HexEncodedOpCert>,

    /// The kes period used to compute the verification key signature
    pub kes_period: Option<KESPeriod>,

    /// The stake associated to the signer
    pub stake: Option<Stake>,

    /// Date and time when the signer_registration was created
    pub created_at: DateTime<Utc>,
}

impl SignerRegistrationRecord {
    pub(crate) fn from_signer_with_stake(other: SignerWithStake, epoch: Epoch) -> Self {
        SignerRegistrationRecord {
            signer_id: other.party_id,
            epoch_setting_id: epoch,
            verification_key: other.verification_key.to_json_hex().unwrap(),
            verification_key_signature: other
                .verification_key_signature
                .map(|k| k.to_json_hex().unwrap()),
            operational_certificate: other
                .operational_certificate
                .map(|o| o.to_json_hex().unwrap()),
            kes_period: other.kes_period,
            stake: Some(other.stake),
            created_at: Utc::now(),
        }
    }
}

impl From<SignerRegistrationRecord> for Signer {
    fn from(other: SignerRegistrationRecord) -> Self {
        Self {
            party_id: other.signer_id,
            verification_key: other.verification_key.try_into().unwrap(),
            verification_key_signature: other
                .verification_key_signature
                .map(|k| (k.try_into().unwrap())),
            operational_certificate: other
                .operational_certificate
                .map(|o| (o.try_into().unwrap())),
            kes_period: other.kes_period,
        }
    }
}

impl From<SignerRegistrationRecord> for SignerWithStake {
    fn from(other: SignerRegistrationRecord) -> Self {
        Self {
            party_id: other.signer_id,
            verification_key: other.verification_key.try_into().unwrap(),
            verification_key_signature: other
                .verification_key_signature
                .map(|k| (k.try_into().unwrap())),
            operational_certificate: other
                .operational_certificate
                .map(|o| (o.try_into().unwrap())),
            kes_period: other.kes_period,
            stake: other.stake.unwrap_or_default(),
        }
    }
}

impl SqLiteEntity for SignerRegistrationRecord {
    fn hydrate(row: sqlite::Row) -> Result<Self, HydrationError>
    where
        Self: Sized,
    {
        let signer_id = row.read::<&str, _>(0).to_string();
        let epoch_setting_id_int = row.read::<i64, _>(1);
        let verification_key = row.read::<&str, _>(2).to_string();
        let verification_key_signature = row.read::<Option<&str>, _>(3).map(|s| s.to_owned());
        let operational_certificate = row.read::<Option<&str>, _>(4).map(|s| s.to_owned());
        let kes_period_int = row.read::<Option<i64>, _>(5);
        let stake_int = row.read::<Option<i64>, _>(6);
        let created_at = row.read::<&str, _>(7);

        let signer_registration_record = Self {
            signer_id,
            epoch_setting_id: Epoch(epoch_setting_id_int.try_into().map_err(|e| {
                HydrationError::InvalidData(format!(
                    "Could not cast i64 ({epoch_setting_id_int}) to u64. Error: '{e}'"
                ))
            })?),
            verification_key,
            verification_key_signature,
            operational_certificate,
            kes_period: match kes_period_int {
                Some(kes_period_int) => Some(kes_period_int.try_into().map_err(|e| {
                    HydrationError::InvalidData(format!(
                        "Could not cast i64 ({kes_period_int}) to u64. Error: '{e}'"
                    ))
                })?),
                None => None,
            },
            stake: match stake_int {
                Some(stake_int) => Some(stake_int.try_into().map_err(|e| {
                    HydrationError::InvalidData(format!(
                        "Could not cast i64 ({stake_int}) to u64. Error: '{e}'"
                    ))
                })?),
                None => None,
            },
            created_at: DateTime::parse_from_rfc3339(created_at)
                .map_err(|e| {
                    HydrationError::InvalidData(format!(
                        "Could not turn string '{created_at}' to rfc3339 Datetime. Error: {e}"
                    ))
                })?
                .with_timezone(&Utc),
        };

        Ok(signer_registration_record)
    }

    fn get_projection() -> Projection {
        let mut projection = Projection::default();
        projection.add_field("signer_id", "{:signer_registration:}.signer_id", "text");
        projection.add_field(
            "epoch_setting_id",
            "{:signer_registration:}.epoch_setting_id",
            "integer",
        );
        projection.add_field(
            "verification_key",
            "{:signer_registration:}.verification_key",
            "text",
        );
        projection.add_field(
            "verification_key_signature",
            "{:signer_registration:}.verification_key_signature",
            "text",
        );
        projection.add_field(
            "operational_certificate",
            "{:signer_registration:}.operational_certificate",
            "text",
        );
        projection.add_field(
            "kes_period",
            "{:signer_registration:}.kes_period",
            "integer",
        );
        projection.add_field("stake", "{:signer_registration:}.stake", "integer");
        projection.add_field("created_at", "{:signer_registration:}.created_at", "text");

        projection
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::test_utils::MithrilFixtureBuilder;

    use super::*;

    #[test]
    fn test_convert_signer_registrations() {
        let fixture = MithrilFixtureBuilder::default().with_signers(5).build();
        let signer_with_stakes = fixture.signers_with_stake();

        let mut signer_registration_records: Vec<SignerRegistrationRecord> = Vec::new();
        for signer_with_stake in signer_with_stakes.clone() {
            signer_registration_records.push(SignerRegistrationRecord::from_signer_with_stake(
                signer_with_stake,
                Epoch(1),
            ));
        }
        let mut signer_with_stakes_new: Vec<SignerWithStake> = Vec::new();
        for signer_registration_record in signer_registration_records {
            signer_with_stakes_new.push(signer_registration_record.into());
        }
        assert_eq!(signer_with_stakes, signer_with_stakes_new);
    }
}
