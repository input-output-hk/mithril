use std::collections::HashMap;

use sqlite::{Connection, Row};

use super::sqlite::{Entity, Projection, ProjectionField, Provider};

#[derive(Debug, PartialEq)]
struct VersionEntity {
    database_version: String,
}

impl Entity for VersionEntity {
    fn hydrate(row: Row) -> Self {
        Self {
            database_version: row.get::<String, _>(0),
        }
    }
}

struct DbVersionProjection {
    fields: Vec<ProjectionField>,
}

impl Projection for DbVersionProjection {
    fn set_field(&mut self, field: ProjectionField) {
        self.fields.push(field);
    }

    fn get_fields(&self) -> &Vec<ProjectionField> {
        &self.fields
    }
}
impl DbVersionProjection {
    pub fn new() -> Self {
        let mut projection = Self { fields: Vec::new() };
        projection.add_field("db_version", "{:version:}.version", "text");

        projection
    }
}

struct VersionProvider {
    connection: Connection,
    projection: DbVersionProjection,
}

impl VersionProvider {
    pub fn new(connection: Connection) -> Self {
        Self {
            connection,
            projection: DbVersionProjection::new(),
        }
    }
}

impl<'conn> Provider<'conn> for VersionProvider {
    type Entity = VersionEntity;

    fn get_projection(&self) -> &dyn Projection {
        &self.projection
    }

    fn get_connection(&'conn self) -> &Connection {
        &self.connection
    }

    fn get_definition(&self, condition: Option<&str>) -> String {
        let where_clause = condition.unwrap_or("true");
        let mut aliases = HashMap::new();
        let _ = aliases.insert("{:version:}".to_string(), "db_version".to_string());
        let projection = self.get_projection().expand(aliases);

        format!("select {projection} from db_version where {where_clause}")
    }
}

struct VersionUpdatedProvider {
    connection: Connection,
    projection: DbVersionProjection,
}

impl VersionUpdatedProvider {
    pub fn new(connection: Connection) -> Self {
        Self {
            connection,
            projection: DbVersionProjection::new(),
        }
    }
}

impl<'conn> Provider<'conn> for VersionUpdatedProvider {
    type Entity = VersionEntity;

    fn get_projection(&self) -> &dyn Projection {
        &self.projection
    }

    fn get_connection(&'conn self) -> &Connection {
        &self.connection
    }

    fn get_definition(&self, condition: Option<&str>) -> String {
        let _where_clause = condition.unwrap_or("true");
        let mut aliases = HashMap::new();
        let _ = aliases.insert("{:version:}".to_string(), "db_version".to_string());
        let projection = self.get_projection().expand(aliases);

        format!("insert into db_version values (?1) returning {projection}")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_projection() {
        let projection = DbVersionProjection::new();
        let mut aliases: HashMap<String, String> = HashMap::new();
        let _ = aliases.insert("{:version:}".to_string(), "whatever".to_string());

        assert_eq!(
            "whatever.version as db_version".to_string(),
            projection.expand(aliases)
        );
    }

    #[test]
    fn test_definition() {
        let connection = Connection::open(":memory:").unwrap();
        let provider = VersionProvider::new(connection);

        assert_eq!(
            "select db_version.version as db_version from db_version where true",
            provider.get_definition(None)
        )
    }

    #[test]
    fn test_updated_entity() {
        let connection = Connection::open(":memory:").unwrap();
        let provider = VersionUpdatedProvider::new(connection);

        assert_eq!(
            "insert into db_version values (?1) returning db_version.version as db_version",
            provider.get_definition(None)
        )
    }
}
