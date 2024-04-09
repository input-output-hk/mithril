use sqlite::Value;

use mithril_common::{entities::Epoch, StdResult};
use mithril_persistence::sqlite::{
    EntityCursor, Provider, SourceAlias, SqLiteEntity, SqliteConnection, WhereCondition,
};

use crate::database::record::SignerRegistrationRecord;

/// Simple queries to retrieve [SignerRegistrationRecord] from the sqlite database.
pub struct GetSignerRegistrationRecordProvider<'client> {
    client: &'client SqliteConnection,
}

impl<'client> GetSignerRegistrationRecordProvider<'client> {
    /// Create a new provider
    pub fn new(client: &'client SqliteConnection) -> Self {
        Self { client }
    }

    fn condition_by_signer_id(&self, signer_id: String) -> StdResult<WhereCondition> {
        Ok(WhereCondition::new(
            "signer_id = ?*",
            vec![Value::String(signer_id)],
        ))
    }

    fn condition_by_epoch(&self, epoch: &Epoch) -> StdResult<WhereCondition> {
        let epoch: i64 = epoch.try_into()?;

        Ok(WhereCondition::new(
            "epoch_setting_id = ?*",
            vec![Value::Integer(epoch)],
        ))
    }

    /// Get SignerRegistrationRecords for given signer id and epoch.
    pub fn get_by_signer_id_and_epoch(
        &self,
        signer_id: String,
        epoch: &Epoch,
    ) -> StdResult<EntityCursor<SignerRegistrationRecord>> {
        let filters = self
            .condition_by_signer_id(signer_id)?
            .and_where(self.condition_by_epoch(epoch)?);
        let signer_registration_record = self.find(filters)?;

        Ok(signer_registration_record)
    }

    /// Get SignerRegistrationRecords for a given Epoch.
    pub fn get_by_epoch(&self, epoch: &Epoch) -> StdResult<EntityCursor<SignerRegistrationRecord>> {
        let filters = self.condition_by_epoch(epoch)?;
        let signer_registration_record = self.find(filters)?;

        Ok(signer_registration_record)
    }

    #[cfg(test)]
    /// Get all SignerRegistrationRecords.
    pub fn get_all(&self) -> StdResult<EntityCursor<SignerRegistrationRecord>> {
        let filters = WhereCondition::default();
        let signer_registration_record = self.find(filters)?;

        Ok(signer_registration_record)
    }
}

impl<'client> Provider<'client> for GetSignerRegistrationRecordProvider<'client> {
    type Entity = SignerRegistrationRecord;

    fn get_connection(&'client self) -> &'client SqliteConnection {
        self.client
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

    use crate::database::test_helper::{insert_signer_registrations, main_db_connection};

    use super::*;

    #[test]
    fn test_get_signer_registration_records() {
        let fixture = MithrilFixtureBuilder::default().with_signers(5).build();
        let signer_with_stakes = fixture.signers_with_stake();
        let signer_with_stakes_by_epoch: Vec<(Epoch, Vec<SignerWithStake>)> = (0..=3)
            .map(|e| (Epoch(e), signer_with_stakes.clone()))
            .collect();

        let connection = main_db_connection().unwrap();
        insert_signer_registrations(&connection, signer_with_stakes_by_epoch.clone()).unwrap();

        let provider = GetSignerRegistrationRecordProvider::new(&connection);

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

        let signer_registration_records: Vec<SignerRegistrationRecord> =
            provider.get_by_epoch(&Epoch(1)).unwrap().collect();
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

        let signer_registration_records: Vec<SignerRegistrationRecord> =
            provider.get_by_epoch(&Epoch(3)).unwrap().collect();
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

        let cursor = provider.get_by_epoch(&Epoch(5)).unwrap();
        assert_eq!(0, cursor.count());

        let signer_registration_records: Vec<SignerRegistrationRecord> =
            provider.get_all().unwrap().collect();
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
