use sqlite::Value;

use mithril_common::{entities::Epoch, StdResult};
use mithril_persistence::sqlite::{
    EntityCursor, Provider, SourceAlias, SqLiteEntity, SqliteConnection, WhereCondition,
};

use crate::database::record::SignerRegistrationRecord;

/// Simple queries to retrieve [SignerRegistrationRecord] from the sqlite database.
pub(crate) struct GetSignerRegistrationRecordProvider<'client> {
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

/// Query to insert or replace [SignerRegistrationRecord] in the sqlite database
pub(crate) struct InsertOrReplaceSignerRegistrationRecordProvider<'conn> {
    connection: &'conn SqliteConnection,
}

impl<'conn> InsertOrReplaceSignerRegistrationRecordProvider<'conn> {
    /// Create a new instance
    pub fn new(connection: &'conn SqliteConnection) -> Self {
        Self { connection }
    }

    pub(crate) fn get_insert_or_replace_condition(
        &self,
        signer_registration_record: SignerRegistrationRecord,
    ) -> WhereCondition {
        WhereCondition::new(
            "(signer_id, epoch_setting_id, verification_key, verification_key_signature, operational_certificate, kes_period, stake, created_at) values (?*, ?*, ?*, ?*, ?*, ?*, ?*, ?*)",
            vec![
                Value::String(signer_registration_record.signer_id),
                Value::Integer(
                    signer_registration_record.epoch_setting_id.try_into().unwrap(),
                ),
                Value::String(signer_registration_record.verification_key),
                signer_registration_record
                            .verification_key_signature
                            .map(Value::String)
                            .unwrap_or(Value::Null),
                signer_registration_record
                            .operational_certificate
                            .map(Value::String)
                            .unwrap_or(Value::Null),
                signer_registration_record
                            .kes_period
                            .map(|k| Value::Integer(i64::from(k)))
                            .unwrap_or(Value::Null),
                signer_registration_record
                            .stake
                            .map(|s| Value::Integer(i64::try_from(s).unwrap()))
                            .unwrap_or(Value::Null),
                            Value::String(signer_registration_record.created_at.to_rfc3339()),
            ],
        )
    }

    pub(crate) fn persist(
        &self,
        signer_registration_record: SignerRegistrationRecord,
    ) -> StdResult<SignerRegistrationRecord> {
        let filters = self.get_insert_or_replace_condition(signer_registration_record.clone());

        let entity = self.find(filters)?.next().unwrap_or_else(|| {
            panic!(
                "No entity returned by the persister, signer_registration_record = {signer_registration_record:?}"
            )
        });

        Ok(entity)
    }
}

impl<'conn> Provider<'conn> for InsertOrReplaceSignerRegistrationRecordProvider<'conn> {
    type Entity = SignerRegistrationRecord;

    fn get_connection(&'conn self) -> &'conn SqliteConnection {
        self.connection
    }

    fn get_definition(&self, condition: &str) -> String {
        // it is important to alias the fields with the same name as the table
        // since the table cannot be aliased in a RETURNING statement in SQLite.
        let projection = Self::Entity::get_projection().expand(SourceAlias::new(&[(
            "{:signer_registration:}",
            "signer_registration",
        )]));

        format!("insert or replace into signer_registration {condition} returning {projection}")
    }
}

/// Query to delete old [SignerRegistrationRecord] from the sqlite database
pub(crate) struct DeleteSignerRegistrationRecordProvider<'conn> {
    connection: &'conn SqliteConnection,
}

impl<'conn> Provider<'conn> for DeleteSignerRegistrationRecordProvider<'conn> {
    type Entity = SignerRegistrationRecord;

    fn get_connection(&'conn self) -> &'conn SqliteConnection {
        self.connection
    }

    fn get_definition(&self, condition: &str) -> String {
        // it is important to alias the fields with the same name as the table
        // since the table cannot be aliased in a RETURNING statement in SQLite.
        let projection = Self::Entity::get_projection().expand(SourceAlias::new(&[(
            "{:signer_registration:}",
            "signer_registration",
        )]));

        format!("delete from signer_registration where {condition} returning {projection}")
    }
}

impl<'conn> DeleteSignerRegistrationRecordProvider<'conn> {
    /// Create a new instance
    pub fn new(connection: &'conn SqliteConnection) -> Self {
        Self { connection }
    }

    /// Create the SQL condition to prune data older than the given Epoch.
    fn get_prune_condition(&self, epoch_threshold: Epoch) -> WhereCondition {
        let epoch_threshold = Value::Integer(epoch_threshold.try_into().unwrap());

        WhereCondition::new("epoch_setting_id < ?*", vec![epoch_threshold])
    }

    /// Prune the epoch setting data older than the given epoch.
    pub fn prune(
        &self,
        epoch_threshold: Epoch,
    ) -> StdResult<EntityCursor<SignerRegistrationRecord>> {
        let filters = self.get_prune_condition(epoch_threshold);

        self.find(filters)
    }
}

#[cfg(test)]
mod tests {
    use chrono::{DateTime, Utc};

    use mithril_common::entities::SignerWithStake;
    use sqlite::Connection;

    use mithril_common::test_utils::MithrilFixtureBuilder;

    use crate::database::test_helper::{
        apply_all_migrations_to_db, disable_foreign_key_support, insert_signer_registrations,
    };

    use super::*;

    pub fn setup_signer_registration_db(
        connection: &SqliteConnection,
        signer_with_stakes_by_epoch: Vec<(Epoch, Vec<SignerWithStake>)>,
    ) -> StdResult<()> {
        apply_all_migrations_to_db(connection)?;
        disable_foreign_key_support(connection)?;
        insert_signer_registrations(connection, signer_with_stakes_by_epoch)
    }

    #[test]
    fn test_get_signer_registration_records() {
        let fixture = MithrilFixtureBuilder::default().with_signers(5).build();
        let signer_with_stakes = fixture.signers_with_stake();
        let signer_with_stakes_by_epoch: Vec<(Epoch, Vec<SignerWithStake>)> = (0..=3)
            .map(|e| (Epoch(e), signer_with_stakes.clone()))
            .collect();

        let connection = Connection::open_thread_safe(":memory:").unwrap();
        setup_signer_registration_db(&connection, signer_with_stakes_by_epoch.clone()).unwrap();

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

    #[test]
    fn test_update_signer_registration_record() {
        let fixture = MithrilFixtureBuilder::default().with_signers(5).build();
        let signer_with_stakes = fixture.signers_with_stake();
        let signer_with_stakes_copy = signer_with_stakes
            .iter()
            .map(|s| {
                let mut s_new = s.clone();
                s_new.stake += 10;
                s_new
            })
            .collect::<Vec<SignerWithStake>>();

        let connection = Connection::open_thread_safe(":memory:").unwrap();
        setup_signer_registration_db(&connection, Vec::new()).unwrap();

        let provider = InsertOrReplaceSignerRegistrationRecordProvider::new(&connection);

        for signer_with_stake in signer_with_stakes {
            let signer_registration_record =
                SignerRegistrationRecord::from_signer_with_stake(signer_with_stake, Epoch(1));
            let signer_registration_record_saved = provider
                .persist(signer_registration_record.clone())
                .unwrap();
            assert_eq!(signer_registration_record, signer_registration_record_saved);
        }

        for signer_with_stake in signer_with_stakes_copy {
            let signer_registration_record =
                SignerRegistrationRecord::from_signer_with_stake(signer_with_stake, Epoch(1));
            let signer_registration_record_saved = provider
                .persist(signer_registration_record.clone())
                .unwrap();
            assert_eq!(signer_registration_record, signer_registration_record_saved);
        }
    }
}
