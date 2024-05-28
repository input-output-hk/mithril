use sqlite::Value;

use mithril_common::{entities::Epoch, StdResult};
use mithril_persistence::sqlite::{Query, SourceAlias, SqLiteEntity, WhereCondition};

use crate::database::record::SignerRegistrationRecord;

/// Simple queries to retrieve [SignerRegistrationRecord] from the sqlite database.
pub struct GetSignerRegistrationRecordQuery {
    condition: WhereCondition,
}

impl GetSignerRegistrationRecordQuery {
    #[cfg(test)]
    pub fn all() -> Self {
        Self {
            condition: WhereCondition::default(),
        }
    }

    /// Query to get SignerRegistrationRecords for given signer id and epoch.
    pub fn by_signer_id_and_epoch(signer_id: String, epoch: Epoch) -> StdResult<Self> {
        let epoch: i64 = epoch.try_into()?;
        let condition =
            WhereCondition::new("signer_id = ?*", vec![Value::String(signer_id)]).and_where(
                WhereCondition::new("epoch_setting_id = ?*", vec![Value::Integer(epoch)]),
            );

        Ok(Self { condition })
    }

    /// Query to get SignerRegistrationRecords for a given Epoch.
    pub fn by_epoch(epoch: Epoch) -> StdResult<Self> {
        let epoch: i64 = epoch.try_into()?;
        let condition = WhereCondition::new("epoch_setting_id = ?*", vec![Value::Integer(epoch)]);

        Ok(Self { condition })
    }
}

impl Query for GetSignerRegistrationRecordQuery {
    type Entity = SignerRegistrationRecord;

    fn filters(&self) -> WhereCondition {
        self.condition.clone()
    }

    fn get_definition(&self, condition: &str) -> String {
        let aliases = SourceAlias::new(&[("{:signer_registration:}", "sr")]);
        let projection = Self::Entity::get_projection().expand(aliases);
        format!("select {projection} from signer_registration as sr where {condition} order by ROWID desc")
    }
}

#[cfg(test)]
mod tests {
    use chrono::{DateTime, Utc};

    use mithril_common::entities::SignerWithStake;
    use mithril_common::test_utils::MithrilFixtureBuilder;
    use mithril_persistence::sqlite::ConnectionExtensions;

    use crate::database::test_helper::{insert_signer_registrations, main_db_connection};

    use super::*;

    fn reset_created_at(
        signer_registration_records: Vec<SignerRegistrationRecord>,
    ) -> Vec<SignerRegistrationRecord> {
        signer_registration_records
            .into_iter()
            .map(|mut sr| {
                sr.created_at = DateTime::<Utc>::default();
                sr
            })
            .collect::<Vec<_>>()
    }

    #[test]
    fn test_get_signer_registration_records() {
        let fixture = MithrilFixtureBuilder::default().with_signers(5).build();
        let signer_with_stakes = fixture.signers_with_stake();
        let signer_with_stakes_by_epoch: Vec<(Epoch, Vec<SignerWithStake>)> = (0..=3)
            .map(|e| (Epoch(e), signer_with_stakes.clone()))
            .collect();

        let connection = main_db_connection().unwrap();
        insert_signer_registrations(&connection, signer_with_stakes_by_epoch.clone()).unwrap();

        let signer_registration_records: Vec<SignerRegistrationRecord> = connection
            .fetch_collect(GetSignerRegistrationRecordQuery::by_epoch(Epoch(1)).unwrap())
            .unwrap();
        let expected_signer_registration_records: Vec<SignerRegistrationRecord> =
            signer_with_stakes_by_epoch[1]
                .1
                .iter()
                .map(|sr| SignerRegistrationRecord::from_signer_with_stake(sr.clone(), Epoch(1)))
                .rev()
                .collect();
        assert_eq!(
            reset_created_at(expected_signer_registration_records),
            reset_created_at(signer_registration_records)
        );

        let signer_registration_records: Vec<SignerRegistrationRecord> = connection
            .fetch_collect(GetSignerRegistrationRecordQuery::by_epoch(Epoch(3)).unwrap())
            .unwrap();
        let expected_signer_registration_records: Vec<SignerRegistrationRecord> =
            signer_with_stakes_by_epoch[3]
                .1
                .iter()
                .map(|sr| SignerRegistrationRecord::from_signer_with_stake(sr.clone(), Epoch(3)))
                .rev()
                .collect();
        assert_eq!(
            reset_created_at(expected_signer_registration_records),
            reset_created_at(signer_registration_records)
        );

        let cursor = connection
            .fetch(GetSignerRegistrationRecordQuery::by_epoch(Epoch(5)).unwrap())
            .unwrap();
        assert_eq!(0, cursor.count());

        let signer_registration_records: Vec<SignerRegistrationRecord> = connection
            .fetch_collect(GetSignerRegistrationRecordQuery::all())
            .unwrap();
        let expected_signer_registration_records: Vec<SignerRegistrationRecord> =
            signer_with_stakes_by_epoch
                .iter()
                .fold(
                    Vec::<SignerRegistrationRecord>::new(),
                    |mut acc, (epoch, signer_with_stakes)| {
                        acc.extend(
                            signer_with_stakes
                                .iter()
                                .map(|sr| {
                                    SignerRegistrationRecord::from_signer_with_stake(
                                        sr.clone(),
                                        *epoch,
                                    )
                                })
                                .collect::<Vec<SignerRegistrationRecord>>(),
                        );
                        acc
                    },
                )
                .into_iter()
                .rev()
                .collect();
        assert_eq!(
            reset_created_at(expected_signer_registration_records),
            reset_created_at(signer_registration_records)
        );
    }
}
