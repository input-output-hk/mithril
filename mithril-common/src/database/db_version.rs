use anyhow::anyhow;
use chrono::{DateTime, NaiveDateTime, Utc};
use sqlite::{Connection, Row, Value};
use std::{
    cmp::Ordering,
    fmt::{Debug, Display},
};

use crate::{
    sqlite::{HydrationError, Projection, Provider, SourceAlias, SqLiteEntity, WhereCondition},
    StdResult,
};

use super::DbVersion;

/// Application using a database
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ApplicationNodeType {
    /// Aggregator node type
    Aggregator,

    /// Signer node type
    Signer,
}

impl ApplicationNodeType {
    /// [ApplicationNodeType] constructor.
    pub fn new(node_type: &str) -> StdResult<Self> {
        match node_type {
            "aggregator" => Ok(Self::Aggregator),
            "signer" => Ok(Self::Signer),
            _ => Err(anyhow!("unknown node type '{node_type}'")),
        }
    }
}

impl Display for ApplicationNodeType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Aggregator => write!(f, "aggregator"),
            Self::Signer => write!(f, "signer"),
        }
    }
}

/// Entity related to the `db_version` database table.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct DatabaseVersion {
    /// Version of the database structure.
    pub version: DbVersion,

    /// Name of the application.
    pub application_type: ApplicationNodeType,

    /// Date of the last version upgrade
    pub updated_at: DateTime<Utc>,
}

impl SqLiteEntity for DatabaseVersion {
    fn hydrate(row: Row) -> Result<Self, HydrationError> {
        let version = row.read::<i64, _>(0);
        let application_type = row.read::<&str, _>(1);
        let updated_at = row.read::<&str, _>(2);

        Ok(Self {
            version,
            application_type: ApplicationNodeType::new(application_type)
                .map_err(|e| HydrationError::InvalidData(format!("{e}")))?,
            updated_at: match DateTime::parse_from_rfc3339(updated_at) {
                Ok(date) => Ok(date.with_timezone(&Utc)),
                // todo: remove this fallback when aggregators & signers have been migrated
                // Fallback to previous date format for compatibility
                Err(_) => NaiveDateTime::parse_from_str(updated_at, "%Y-%m-%d %H:%M:%S")
                    .map_err(|e| {
                        HydrationError::InvalidData(format!(
                            "Could not turn string '{updated_at}' to rfc3339 Datetime. Error: {e}"
                        ))
                    })
                    .map(|d| d.and_utc()),
            }?,
        })
    }

    fn get_projection() -> Projection {
        let mut projection = Projection::default();
        projection.add_field("version", "{:db_version:}.version", "text");
        projection.add_field(
            "application_type",
            "{:db_version:}.application_type",
            "text",
        );
        projection.add_field("updated_at", "{:db_version:}.updated_at", "timestamp");

        projection
    }
}

impl PartialOrd for DatabaseVersion {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        if self.application_type != other.application_type {
            None
        } else {
            self.version.partial_cmp(&other.version)
        }
    }
}

/// Provider for the [DatabaseVersion] entities using the `DatabaseVersionProjection`.
pub struct DatabaseVersionProvider<'conn> {
    connection: &'conn Connection,
}

impl<'conn> DatabaseVersionProvider<'conn> {
    /// [DatabaseVersionProvider] constructor.
    pub fn new(connection: &'conn Connection) -> Self {
        Self { connection }
    }

    /// Method to create the table at the beginning of the migration procedure.
    /// This code is temporary and should not last.
    pub fn create_table_if_not_exists(
        &self,
        application_type: &ApplicationNodeType,
    ) -> StdResult<()> {
        let connection = self.get_connection();
        let sql = "select exists(select name from sqlite_master where type='table' and name='db_version') as table_exists";
        let table_exists = connection
            .prepare(sql)?
            .iter()
            .next()
            .unwrap()
            .unwrap()
            .read::<i64, _>(0)
            == 1;

        if !table_exists {
            let sql = format!("
create table db_version (application_type text not null primary key, version integer not null, updated_at text not null);
insert into db_version (application_type, version, updated_at) values ('{application_type}', 0, '{}');
", Utc::now().to_rfc3339());
            connection.execute(sql)?;
        }

        Ok(())
    }

    /// Read the application version from the database.
    pub fn get_application_version(
        &self,
        application_type: &ApplicationNodeType,
    ) -> StdResult<Option<DatabaseVersion>> {
        let filters = WhereCondition::new(
            "application_type = ?*",
            vec![Value::String(format!("{application_type}"))],
        );
        let result = self.find(filters)?.next();

        Ok(result)
    }
}

impl<'conn> Provider<'conn> for DatabaseVersionProvider<'conn> {
    type Entity = DatabaseVersion;

    fn get_connection(&'conn self) -> &Connection {
        self.connection
    }

    fn get_definition(&self, condition: &str) -> String {
        let aliases = SourceAlias::new(&[("{:db_version:}", "db_version")]);
        let projection = Self::Entity::get_projection().expand(aliases);

        format!(
            r#"
select {projection}
from db_version
where {condition}
"#
        )
    }
}

/// Write [Provider] for the [DatabaseVersion] entities.
/// This will perform an UPSERT and return the updated entity.
pub struct DatabaseVersionUpdater<'conn> {
    connection: &'conn Connection,
}

impl<'conn> DatabaseVersionUpdater<'conn> {
    /// [DatabaseVersionUpdater] constructor.
    pub fn new(connection: &'conn Connection) -> Self {
        Self { connection }
    }

    /// Persist the given entity and return the projection of the saved entity.
    pub fn save(&self, version: DatabaseVersion) -> StdResult<DatabaseVersion> {
        let filters = WhereCondition::new(
            "",
            vec![
                Value::String(format!("{}", version.application_type)),
                Value::Integer(version.version),
                Value::String(version.updated_at.to_rfc3339()),
            ],
        );
        let entity = self
            .find(filters)?
            .next()
            .ok_or(anyhow!("No data returned after insertion"))?;

        Ok(entity)
    }
}

impl<'conn> Provider<'conn> for DatabaseVersionUpdater<'conn> {
    type Entity = DatabaseVersion;

    fn get_connection(&'conn self) -> &Connection {
        self.connection
    }

    fn get_definition(&self, _condition: &str) -> String {
        let aliases = SourceAlias::new(&[("{:db_version:}", "db_version")]);
        let projection = Self::Entity::get_projection().expand(aliases);

        format!(
            r#"
insert into db_version (application_type, version, updated_at) values (?, ?, ?)
  on conflict (application_type) do update set version = excluded.version, updated_at = excluded.updated_at
returning {projection}
"#
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_projection() {
        let projection = DatabaseVersion::get_projection();
        let aliases = SourceAlias::new(&[("{:db_version:}", "whatever")]);

        assert_eq!(
            "whatever.version as version, whatever.application_type as application_type, whatever.updated_at as updated_at"
                .to_string(),
            projection.expand(aliases)
        );
    }

    #[test]
    fn test_definition() {
        let connection = Connection::open(":memory:").unwrap();
        let provider = DatabaseVersionProvider::new(&connection);

        assert_eq!(
            r#"
select db_version.version as version, db_version.application_type as application_type, db_version.updated_at as updated_at
from db_version
where true
"#,
            provider.get_definition("true")
        )
    }

    #[test]
    fn test_updated_entity() {
        let connection = Connection::open(":memory:").unwrap();
        let provider = DatabaseVersionUpdater::new(&connection);

        assert_eq!(
            r#"
insert into db_version (application_type, version, updated_at) values (?, ?, ?)
  on conflict (application_type) do update set version = excluded.version, updated_at = excluded.updated_at
returning db_version.version as version, db_version.application_type as application_type, db_version.updated_at as updated_at
"#,
            provider.get_definition("true")
        )
    }
}
