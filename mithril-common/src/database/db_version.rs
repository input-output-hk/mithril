use std::{collections::HashMap, error::Error, fmt::Display};

use chrono::NaiveDateTime;
use semver::Version;
use sqlite::{Connection, Row, Value};

use crate::sqlite::{HydrationError, Projection, ProjectionField, Provider, SqLiteEntity};

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

/// Entity related to the `app_version` database table.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ApplicationVersion {
    /// Semver of the database structure.
    pub semver: Version,

    /// Name of the application.
    pub application_type: ApplicationNodeType,

    /// Date of the last version upgrade, Sqlite does not store timezone
    /// information hence we have to use a `Chrono::NaiveDateTime` here.
    pub updated_at: NaiveDateTime,
}

impl SqLiteEntity for ApplicationVersion {
    fn hydrate(row: Row) -> Result<Self, HydrationError> {
        Ok(Self {
            semver: Version::parse(&row.get::<String, _>(0))
                .map_err(|e| HydrationError::InvalidData(format!("{}", e)))?,
            application_type: ApplicationNodeType::new(&row.get::<String, _>(1))
                .map_err(|e| HydrationError::InvalidData(format!("{}", e)))?,
            updated_at: NaiveDateTime::parse_from_str(
                &row.get::<String, _>(2),
                "%Y-%m-%d %H:%M:%S",
            )
            .map_err(|e| HydrationError::InvalidData(format!("{}", e)))?,
        })
    }
}

/// Projection dedicated to [ApplicationVersion] entities.
struct ApplicationVersionProjection {
    fields: Vec<ProjectionField>,
}

impl Projection for ApplicationVersionProjection {
    fn set_field(&mut self, field: ProjectionField) {
        self.fields.push(field);
    }

    fn get_fields(&self) -> &Vec<ProjectionField> {
        &self.fields
    }
}
impl ApplicationVersionProjection {
    pub fn new() -> Self {
        let mut projection = Self { fields: Vec::new() };
        projection.add_field("semver", "{:app_version:}.semver", "text");
        projection.add_field(
            "application_type",
            "{:app_version:}.application_type",
            "text",
        );
        projection.add_field("updated_at", "{:app_version:}.updated_at", "timestamp");

        projection
    }
}

/// Provider for the [ApplicationVersion] entities using the `ApplicationVersionProjection`.
pub struct VersionProvider<'conn> {
    connection: &'conn Connection,
    projection: ApplicationVersionProjection,
}

impl<'conn> VersionProvider<'conn> {
    /// [VersionProvider] constructor.
    pub fn new(connection: &'conn Connection) -> Self {
        Self {
            connection,
            projection: ApplicationVersionProjection::new(),
        }
    }

    /// Method to create the table at the beginning of the migration procedure.
    /// This code is temporary and should not last.
    pub fn create_table_if_not_exists(&self) -> Result<(), Box<dyn Error>> {
        let connection = self.get_connection();
        let sql = "select exists(select name from sqlite_master where type='table' and name='app_version') as table_exists";
        let table_exists = connection
            .prepare(sql)?
            .into_cursor()
            .bind(&[])?
            .next()
            .unwrap()?
            .get::<i64, _>(0)
            == 1;

        if !table_exists {
            let sql = r#"
create table app_version (application_type text not null primary key, semver text not null, updated_at timestamp not null default CURRENT_TIMESTAMP)
"#;
            connection.execute(sql)?;
        }

        Ok(())
    }

    /// Read the application version from the database.
    pub fn get_application_version(
        &self,
        application_type: &ApplicationNodeType,
    ) -> Result<Option<ApplicationVersion>, Box<dyn Error>> {
        let condition = "application_type = ?";
        let params = [Value::String(format!("{}", application_type))];
        let result = self.find(Some(condition), &params)?.next();

        Ok(result)
    }
}

impl<'conn> Provider<'conn> for VersionProvider<'conn> {
    type Entity = ApplicationVersion;

    fn get_projection(&self) -> &dyn Projection {
        &self.projection
    }

    fn get_connection(&'conn self) -> &Connection {
        self.connection
    }

    fn get_definition(&self, condition: Option<&str>) -> String {
        let where_clause = condition.unwrap_or("true");
        let mut aliases = HashMap::new();
        let _ = aliases.insert("{:app_version:}".to_string(), "app_version".to_string());
        let projection = self.get_projection().expand(aliases);

        format!(
            r#"
select {projection}
from app_version
where {where_clause}
"#
        )
    }
}

/// Write [Provider] for the [ApplicationVersion] entities.
/// This will perform an UPSERT and return the updated entity.
pub struct VersionUpdaterProvider<'conn> {
    connection: &'conn Connection,
    projection: ApplicationVersionProjection,
}

impl<'conn> VersionUpdaterProvider<'conn> {
    /// [VersionUpdaterProvider] constructor.
    pub fn new(connection: &'conn Connection) -> Self {
        Self {
            connection,
            projection: ApplicationVersionProjection::new(),
        }
    }

    /// Persist the given entity and return the projection of the saved entity.
    pub fn save(&self, version: ApplicationVersion) -> Result<ApplicationVersion, Box<dyn Error>> {
        let params = [
            Value::String(format!("{}", version.application_type)),
            Value::String(version.semver.to_string()),
        ];
        let entity = self
            .find(None, &params)?
            .next()
            .ok_or("No data returned after insertion")?;

        Ok(entity)
    }
}

impl<'conn> Provider<'conn> for VersionUpdaterProvider<'conn> {
    type Entity = ApplicationVersion;

    fn get_projection(&self) -> &dyn Projection {
        &self.projection
    }

    fn get_connection(&'conn self) -> &Connection {
        self.connection
    }

    fn get_definition(&self, condition: Option<&str>) -> String {
        let _where_clause = condition.unwrap_or("true");
        let mut aliases = HashMap::new();
        let _ = aliases.insert("{:app_version:}".to_string(), "app_version".to_string());
        let projection = self.get_projection().expand(aliases);

        format!(
            r#"
insert into app_version (application_type, semver) values (?, ?)
  on conflict (application_type) do update set semver = excluded.semver, updated_at = CURRENT_TIMESTAMP
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
        let projection = ApplicationVersionProjection::new();
        let mut aliases: HashMap<String, String> = HashMap::new();
        let _ = aliases.insert("{:app_version:}".to_string(), "whatever".to_string());

        assert_eq!(
            "whatever.semver as semver, whatever.application_type as application_type, whatever.updated_at as updated_at"
                .to_string(),
            projection.expand(aliases)
        );
    }

    #[test]
    fn test_definition() {
        let connection = Connection::open(":memory:").unwrap();
        let provider = VersionProvider::new(&connection);

        assert_eq!(
            r#"
select app_version.semver as semver, app_version.application_type as application_type, app_version.updated_at as updated_at
from app_version
where true
"#,
            provider.get_definition(None)
        )
    }

    #[test]
    fn test_updated_entity() {
        let connection = Connection::open(":memory:").unwrap();
        let provider = VersionUpdaterProvider::new(&connection);

        assert_eq!(
            r#"
insert into app_version (application_type, semver) values (?, ?)
  on conflict (application_type) do update set semver = excluded.semver, updated_at = CURRENT_TIMESTAMP
returning app_version.semver as semver, app_version.application_type as application_type, app_version.updated_at as updated_at
"#,
            provider.get_definition(None)
        )
    }
}
