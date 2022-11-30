use std::{
    cmp::Ordering,
    collections::HashMap,
    error::Error,
    fmt::{Debug, Display},
};

use chrono::NaiveDateTime;
use sqlite::{Connection, Row, Value};

use crate::sqlite::{HydrationError, Projection, Provider, SqLiteEntity};

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
    pub fn new(node_type: &str) -> Result<Self, Box<dyn Error>> {
        match node_type {
            "aggregator" => Ok(Self::Aggregator),
            "signer" => Ok(Self::Signer),
            _ => Err(format!("unknown node type '{}'", node_type).into()),
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

    /// Date of the last version upgrade, Sqlite does not store timezone
    /// information hence we have to use a `Chrono::NaiveDateTime` here.
    pub updated_at: NaiveDateTime,
}

impl SqLiteEntity for DatabaseVersion {
    fn hydrate(row: Row) -> Result<Self, HydrationError> {
        Ok(Self {
            version: row.get::<i64, _>(0),
            application_type: ApplicationNodeType::new(&row.get::<String, _>(1))
                .map_err(|e| HydrationError::InvalidData(format!("{}", e)))?,
            updated_at: NaiveDateTime::parse_from_str(
                &row.get::<String, _>(2),
                "%Y-%m-%d %H:%M:%S",
            )
            .map_err(|e| HydrationError::InvalidData(format!("{}", e)))?,
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
    projection: Projection,
}

impl<'conn> DatabaseVersionProvider<'conn> {
    /// [DatabaseVersionProvider] constructor.
    pub fn new(connection: &'conn Connection) -> Self {
        Self {
            connection,
            projection: DatabaseVersion::get_projection(),
        }
    }

    /// Method to create the table at the beginning of the migration procedure.
    /// This code is temporary and should not last.
    pub fn create_table_if_not_exists(
        &self,
        application_type: &ApplicationNodeType,
    ) -> Result<(), Box<dyn Error>> {
        let connection = self.get_connection();
        let sql = "select exists(select name from sqlite_master where type='table' and name='db_version') as table_exists";
        let table_exists = connection
            .prepare(sql)?
            .into_cursor()
            .bind(&[])?
            .next()
            .unwrap()?
            .get::<i64, _>(0)
            == 1;

        if !table_exists {
            let sql = format!("
create table db_version (application_type text not null primary key, version integer not null, updated_at timestamp not null default CURRENT_TIMESTAMP);
insert into db_version (application_type, version) values ('{application_type}', 0);
");
            connection.execute(sql)?;
        }

        Ok(())
    }

    /// Read the application version from the database.
    pub fn get_application_version(
        &self,
        application_type: &ApplicationNodeType,
    ) -> Result<Option<DatabaseVersion>, Box<dyn Error>> {
        let condition = "application_type = ?";
        let params = [Value::String(format!("{}", application_type))];
        let result = self.find(Some(condition), &params)?.next();

        Ok(result)
    }
}

impl<'conn> Provider<'conn> for DatabaseVersionProvider<'conn> {
    type Entity = DatabaseVersion;

    fn get_projection(&self) -> &Projection {
        &self.projection
    }

    fn get_connection(&'conn self) -> &Connection {
        self.connection
    }

    fn get_definition(&self, condition: Option<&str>) -> String {
        let where_clause = condition.unwrap_or("true");
        let mut aliases = HashMap::new();
        let _ = aliases.insert("{:db_version:}".to_string(), "db_version".to_string());
        let projection = self.get_projection().expand(aliases);

        format!(
            r#"
select {projection}
from db_version
where {where_clause}
"#
        )
    }
}

/// Write [Provider] for the [DatabaseVersion] entities.
/// This will perform an UPSERT and return the updated entity.
pub struct DatabaseVersionUpdater<'conn> {
    connection: &'conn Connection,
    projection: Projection,
}

impl<'conn> DatabaseVersionUpdater<'conn> {
    /// [DatabaseVersionUpdater] constructor.
    pub fn new(connection: &'conn Connection) -> Self {
        Self {
            connection,
            projection: DatabaseVersion::get_projection(),
        }
    }

    /// Persist the given entity and return the projection of the saved entity.
    pub fn save(&self, version: DatabaseVersion) -> Result<DatabaseVersion, Box<dyn Error>> {
        let params = [
            Value::String(format!("{}", version.application_type)),
            Value::Integer(version.version),
        ];
        let entity = self
            .find(None, &params)?
            .next()
            .ok_or("No data returned after insertion")?;

        Ok(entity)
    }
}

impl<'conn> Provider<'conn> for DatabaseVersionUpdater<'conn> {
    type Entity = DatabaseVersion;

    fn get_projection(&self) -> &Projection {
        &self.projection
    }

    fn get_connection(&'conn self) -> &Connection {
        self.connection
    }

    fn get_definition(&self, condition: Option<&str>) -> String {
        let _where_clause = condition.unwrap_or("true");
        let mut aliases = HashMap::new();
        let _ = aliases.insert("{:db_version:}".to_string(), "db_version".to_string());
        let projection = self.get_projection().expand(aliases);

        format!(
            r#"
insert into db_version (application_type, version) values (?, ?)
  on conflict (application_type) do update set version = excluded.version, updated_at = CURRENT_TIMESTAMP
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
        let mut aliases: HashMap<String, String> = HashMap::new();
        let _ = aliases.insert("{:db_version:}".to_string(), "whatever".to_string());

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
            provider.get_definition(None)
        )
    }

    #[test]
    fn test_updated_entity() {
        let connection = Connection::open(":memory:").unwrap();
        let provider = DatabaseVersionUpdater::new(&connection);

        assert_eq!(
            r#"
insert into db_version (application_type, version) values (?, ?)
  on conflict (application_type) do update set version = excluded.version, updated_at = CURRENT_TIMESTAMP
returning db_version.version as version, db_version.application_type as application_type, db_version.updated_at as updated_at
"#,
            provider.get_definition(None)
        )
    }
}
