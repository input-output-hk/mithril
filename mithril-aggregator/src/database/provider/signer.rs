use std::iter::repeat;

use sqlite::Value;

use mithril_common::StdResult;
use mithril_persistence::sqlite::{
    EntityCursor, Provider, SourceAlias, SqLiteEntity, SqliteConnection, WhereCondition,
};

use crate::database::record::SignerRecord;

/// Simple queries to retrieve [SignerRecord] from the sqlite database.
pub(crate) struct SignerRecordProvider<'client> {
    client: &'client SqliteConnection,
}

impl<'client> SignerRecordProvider<'client> {
    /// Create a new provider
    pub fn new(client: &'client SqliteConnection) -> Self {
        Self { client }
    }

    #[cfg(test)]
    fn condition_by_signer_id(&self, signer_id: String) -> StdResult<WhereCondition> {
        Ok(WhereCondition::new(
            "signer_id = ?*",
            vec![Value::String(signer_id)],
        ))
    }

    #[cfg(test)]
    /// Get SignerRecords for a given signer id.
    pub fn get_by_signer_id(&self, signer_id: String) -> StdResult<EntityCursor<SignerRecord>> {
        let filters = self.condition_by_signer_id(signer_id)?;
        let signer_record = self.find(filters)?;

        Ok(signer_record)
    }

    /// Get all SignerRecords.
    pub fn get_all(&self) -> StdResult<EntityCursor<SignerRecord>> {
        let filters = WhereCondition::default();
        let signer_record = self.find(filters)?;

        Ok(signer_record)
    }
}

impl<'client> Provider<'client> for SignerRecordProvider<'client> {
    type Entity = SignerRecord;

    fn get_connection(&'client self) -> &'client SqliteConnection {
        self.client
    }

    fn get_definition(&self, condition: &str) -> String {
        let aliases = SourceAlias::new(&[("{:signer:}", "s")]);
        let projection = Self::Entity::get_projection().expand(aliases);
        format!("select {projection} from signer as s where {condition} order by ROWID desc")
    }
}

/// Query to register a [SignerRecord] in the sqlite database
///
/// If it already exists it's `last_registered_at` and `updated_at` fields will be updated.
pub(crate) struct RegisterSignerRecordProvider<'conn> {
    connection: &'conn SqliteConnection,
}

impl<'conn> RegisterSignerRecordProvider<'conn> {
    /// Create a new instance
    pub fn new(connection: &'conn SqliteConnection) -> Self {
        Self { connection }
    }

    fn get_register_condition(&self, signer_record: SignerRecord) -> WhereCondition {
        WhereCondition::new(
            "(signer_id, pool_ticker, created_at, updated_at, last_registered_at) values (?*, ?*, ?*, ?*, ?*)",
            vec![
                Value::String(signer_record.signer_id),
                signer_record
                    .pool_ticker
                    .map(Value::String)
                    .unwrap_or(Value::Null),
                Value::String(signer_record.created_at.to_rfc3339()),
                Value::String(signer_record.updated_at.to_rfc3339()),
                signer_record
                    .last_registered_at
                    .map(|d| Value::String(d.to_rfc3339()))
                    .unwrap_or(Value::Null),
            ],
        )
    }

    pub(crate) fn persist(&self, signer_record: SignerRecord) -> StdResult<SignerRecord> {
        let filters = self.get_register_condition(signer_record.clone());

        let entity = self.find(filters)?.next().unwrap_or_else(|| {
            panic!("No entity returned by the persister, signer_record = {signer_record:?}")
        });

        Ok(entity)
    }
}

impl<'conn> Provider<'conn> for RegisterSignerRecordProvider<'conn> {
    type Entity = SignerRecord;

    fn get_connection(&'conn self) -> &'conn SqliteConnection {
        self.connection
    }

    fn get_definition(&self, condition: &str) -> String {
        // it is important to alias the fields with the same name as the table
        // since the table cannot be aliased in a RETURNING statement in SQLite.
        let projection =
            Self::Entity::get_projection().expand(SourceAlias::new(&[("{:signer:}", "signer")]));

        format!(
            "insert into signer {condition} on conflict (signer_id) do update set \
            updated_at = excluded.updated_at, last_registered_at = excluded.last_registered_at returning {projection}"
        )
    }
}

/// Query used by the [signer importer][crate::tools::SignersImporter] to register a [SignerRecord]
/// in the sqlite database.
///
/// If it already exists it's `pool_ticker` and `updated_at` fields will be updated.
pub(crate) struct ImportSignerRecordProvider<'conn> {
    connection: &'conn SqliteConnection,
}

impl<'conn> ImportSignerRecordProvider<'conn> {
    /// Create a new instance
    pub fn new(connection: &'conn SqliteConnection) -> Self {
        Self { connection }
    }

    pub(crate) fn get_import_condition(&self, signer_records: Vec<SignerRecord>) -> WhereCondition {
        let columns = "(signer_id, pool_ticker, created_at, updated_at, last_registered_at)";
        let values_columns: Vec<&str> = repeat("(?*, ?*, ?*, ?*, ?*)")
            .take(signer_records.len())
            .collect();
        let values = signer_records
            .into_iter()
            .flat_map(|signer_record| {
                vec![
                    Value::String(signer_record.signer_id),
                    signer_record
                        .pool_ticker
                        .map(Value::String)
                        .unwrap_or(Value::Null),
                    Value::String(signer_record.created_at.to_rfc3339()),
                    Value::String(signer_record.updated_at.to_rfc3339()),
                    signer_record
                        .last_registered_at
                        .map(|d| Value::String(d.to_rfc3339()))
                        .unwrap_or(Value::Null),
                ]
            })
            .collect();

        WhereCondition::new(
            format!("{columns} values {}", values_columns.join(", ")).as_str(),
            values,
        )
    }

    pub(crate) fn persist(&self, signer_record: SignerRecord) -> StdResult<SignerRecord> {
        let filters = self.get_import_condition(vec![signer_record.clone()]);

        let entity = self.find(filters)?.next().unwrap_or_else(|| {
            panic!("No entity returned by the persister, signer_record = {signer_record:?}")
        });

        Ok(entity)
    }

    pub(crate) fn persist_many(
        &self,
        signer_records: Vec<SignerRecord>,
    ) -> StdResult<Vec<SignerRecord>> {
        let filters = self.get_import_condition(signer_records);

        Ok(self.find(filters)?.collect())
    }
}

impl<'conn> Provider<'conn> for ImportSignerRecordProvider<'conn> {
    type Entity = SignerRecord;

    fn get_connection(&'conn self) -> &'conn SqliteConnection {
        self.connection
    }

    fn get_definition(&self, condition: &str) -> String {
        // it is important to alias the fields with the same name as the table
        // since the table cannot be aliased in a RETURNING statement in SQLite.
        let projection =
            Self::Entity::get_projection().expand(SourceAlias::new(&[("{:signer:}", "signer")]));

        format!(
            "insert into signer {condition} on conflict(signer_id) do update \
            set pool_ticker = excluded.pool_ticker, updated_at = excluded.updated_at returning {projection}"
        )
    }
}

#[cfg(test)]
mod tests {
    use chrono::Duration;
    use sqlite::Connection;

    use mithril_common::StdResult;

    use crate::database::test_helper::{apply_all_migrations_to_db, insert_signers};

    use super::*;

    pub fn setup_signer_db(
        connection: &SqliteConnection,
        records: Vec<SignerRecord>,
    ) -> StdResult<()> {
        apply_all_migrations_to_db(connection)?;
        insert_signers(connection, records)
    }

    #[test]
    fn projection() {
        let projection = SignerRecord::get_projection();
        let aliases = SourceAlias::new(&[("{:signer:}", "s")]);

        assert_eq!(
            "s.signer_id as signer_id, s.pool_ticker as pool_ticker, s.created_at as created_at, \
             s.updated_at as updated_at, s.last_registered_at as last_registered_at"
                .to_string(),
            projection.expand(aliases)
        );
    }

    #[test]
    fn get_signer_record_by_signer_id() {
        let connection = Connection::open_thread_safe(":memory:").unwrap();
        let provider = SignerRecordProvider::new(&connection);
        let condition = provider
            .condition_by_signer_id("signer-123".to_string())
            .unwrap();
        let (filter, values) = condition.expand();

        assert_eq!("signer_id = ?1".to_string(), filter);
        assert_eq!(vec![Value::String("signer-123".to_string())], values);
    }

    #[test]
    fn insert_signer_record() {
        let signer_record = SignerRecord::fake_records(1).first().unwrap().to_owned();
        let connection = Connection::open_thread_safe(":memory:").unwrap();
        let provider = RegisterSignerRecordProvider::new(&connection);
        let condition = provider.get_register_condition(signer_record.clone());
        let (values, params) = condition.expand();

        assert_eq!(
            "(signer_id, pool_ticker, created_at, updated_at, last_registered_at) values (?1, ?2, ?3, ?4, ?5)".to_string(),
            values
        );
        assert_eq!(
            vec![
                Value::String(signer_record.signer_id),
                Value::String(signer_record.pool_ticker.unwrap()),
                Value::String(signer_record.created_at.to_rfc3339()),
                Value::String(signer_record.updated_at.to_rfc3339()),
                Value::String(signer_record.last_registered_at.unwrap().to_rfc3339()),
            ],
            params
        );
    }

    #[test]
    fn update_signer_record() {
        let signer_records = SignerRecord::fake_records(2);
        let connection = Connection::open_thread_safe(":memory:").unwrap();
        let provider = ImportSignerRecordProvider::new(&connection);
        let condition = provider.get_import_condition(signer_records.clone());
        let (values, params) = condition.expand();

        assert_eq!(
            "(signer_id, pool_ticker, created_at, updated_at, last_registered_at) values (?1, ?2, ?3, ?4, ?5), (?6, ?7, ?8, ?9, ?10)",
            &values
        );
        assert_eq!(
            vec![
                Value::String(signer_records[0].signer_id.to_owned()),
                Value::String(signer_records[0].pool_ticker.to_owned().unwrap()),
                Value::String(signer_records[0].created_at.to_rfc3339()),
                Value::String(signer_records[0].updated_at.to_rfc3339()),
                Value::String(signer_records[0].last_registered_at.unwrap().to_rfc3339()),
                Value::String(signer_records[1].signer_id.to_owned()),
                Value::String(signer_records[1].pool_ticker.to_owned().unwrap()),
                Value::String(signer_records[1].created_at.to_rfc3339()),
                Value::String(signer_records[1].updated_at.to_rfc3339()),
                Value::String(signer_records[1].last_registered_at.unwrap().to_rfc3339()),
            ],
            params
        );
    }

    #[test]
    fn test_get_signer_records() {
        let signer_records_fake = SignerRecord::fake_records(5);

        let connection = Connection::open_thread_safe(":memory:").unwrap();
        setup_signer_db(&connection, signer_records_fake.clone()).unwrap();

        let provider = SignerRecordProvider::new(&connection);

        let signer_records: Vec<SignerRecord> = provider
            .get_by_signer_id(signer_records_fake[0].signer_id.to_owned())
            .unwrap()
            .collect();
        let expected_signer_records: Vec<SignerRecord> = vec![signer_records_fake[0].to_owned()];
        assert_eq!(expected_signer_records, signer_records);

        let signer_records: Vec<SignerRecord> = provider
            .get_by_signer_id(signer_records_fake[2].signer_id.to_owned())
            .unwrap()
            .collect();
        let expected_signer_records: Vec<SignerRecord> = vec![signer_records_fake[2].to_owned()];
        assert_eq!(expected_signer_records, signer_records);

        let cursor = provider
            .get_by_signer_id("signer-id-not-registered".to_string())
            .unwrap();
        assert_eq!(0, cursor.count());

        let signer_records: Vec<SignerRecord> = provider.get_all().unwrap().collect();
        let expected_signer_records: Vec<SignerRecord> =
            signer_records_fake.into_iter().rev().collect();
        assert_eq!(expected_signer_records, signer_records);
    }

    #[test]
    fn test_insert_signer_record() {
        let signer_records_fake = SignerRecord::fake_records(5);

        let connection = Connection::open_thread_safe(":memory:").unwrap();
        setup_signer_db(&connection, Vec::new()).unwrap();

        let provider = RegisterSignerRecordProvider::new(&connection);

        for signer_record in signer_records_fake.clone() {
            let signer_record_saved = provider.persist(signer_record.clone()).unwrap();
            assert_eq!(signer_record, signer_record_saved);
        }

        for mut signer_record in signer_records_fake {
            signer_record.updated_at += Duration::try_hours(1).unwrap();
            let signer_record_saved = provider.persist(signer_record.clone()).unwrap();
            assert_eq!(signer_record, signer_record_saved);
        }
    }

    #[test]
    fn test_update_signer_record() {
        let signer_records_fake = SignerRecord::fake_records(5);

        let connection = Connection::open_thread_safe(":memory:").unwrap();
        setup_signer_db(&connection, signer_records_fake.clone()).unwrap();

        let provider = ImportSignerRecordProvider::new(&connection);

        for signer_record in signer_records_fake.clone() {
            let signer_record_saved = provider.persist(signer_record.clone()).unwrap();
            assert_eq!(signer_record, signer_record_saved);
        }

        for mut signer_record in signer_records_fake {
            signer_record.pool_ticker = Some(format!("new-pool-{}", signer_record.signer_id));
            signer_record.updated_at += Duration::try_hours(1).unwrap();
            let signer_record_saved = provider.persist(signer_record.clone()).unwrap();
            assert_eq!(signer_record, signer_record_saved);
        }
    }

    #[test]
    fn test_update_many_signer_records() {
        let mut signer_records_fake = SignerRecord::fake_records(5);
        signer_records_fake.sort_by(|a, b| a.signer_id.cmp(&b.signer_id));

        let connection = Connection::open_thread_safe(":memory:").unwrap();
        setup_signer_db(&connection, signer_records_fake.clone()).unwrap();

        let provider = ImportSignerRecordProvider::new(&connection);
        let mut saved_records = provider.persist_many(signer_records_fake.clone()).unwrap();
        saved_records.sort_by(|a, b| a.signer_id.cmp(&b.signer_id));
        assert_eq!(signer_records_fake, saved_records);

        for signer_record in signer_records_fake.iter_mut() {
            signer_record.pool_ticker = Some(format!("new-pool-{}", signer_record.signer_id));
            signer_record.updated_at += Duration::try_hours(1).unwrap();
        }
        let mut saved_records = provider.persist_many(signer_records_fake.clone()).unwrap();
        saved_records.sort_by(|a, b| a.signer_id.cmp(&b.signer_id));
        assert_eq!(signer_records_fake, saved_records);
    }
}
