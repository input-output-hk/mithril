use std::error::Error;

use sqlite::{Connection, Row};

use super::sqlite::{Entity, EntityCursor, Provider};

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

struct VersionProvider {
    connection: Connection,
}

impl VersionProvider {
    pub fn new(connection: Connection) -> Self {
        Self { connection }
    }
}

impl<'conn> Provider<'conn> for VersionProvider {
    type Entity = VersionEntity;

    fn get_connection(&'conn self) -> &Connection {
        &self.connection
    }

    fn find(
        &'conn self,
        _condition: &str,
    ) -> Result<EntityCursor<'conn, VersionEntity>, Box<dyn Error>> {
        let sql = "select database_version from db_version";

        self.query(sql, &[])
    }
}
