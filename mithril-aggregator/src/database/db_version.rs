use std::{env::current_dir, error::Error, marker::PhantomData};

use sqlite::{Connection, Cursor, Result as SqliteResult, Row};

trait Entity {
    fn hydrate(row: Row) -> Self;
}
struct EntityCursor<'a, T> {
    cursor: Cursor<'a>,
    phantom: PhantomData<T>,
}

impl<'a, T> EntityCursor<'a, T> {
    pub fn new(cursor: Cursor<'a>) -> Self {
        Self {
            cursor,
            phantom: PhantomData,
        }
    }
}

impl<'a, T> Iterator for EntityCursor<'a, T>
where
    T: Entity,
{
    type Item = T;

    fn next(&mut self) -> Option<T> {
        self.cursor.next().map(|res| T::hydrate(res.unwrap()))
    }
}
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

    pub fn find(&self) -> Result<EntityCursor<'_, VersionEntity>, Box<dyn Error>> {
        let sql = "select database_version from db_version";
        let cursor = self.connection.prepare(sql)?.into_cursor().bind(&[])?;

        Ok(EntityCursor::new(cursor))
    }
}
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    pub fn simple_test() {
        let connection = Connection::open(":memory:").unwrap();
        connection
            .execute(
                "
            drop table if exists db_version;
            create table db_version(database_version text not null);
            insert into db_version(database_version) values ('0.1.0');
            ",
            )
            .unwrap();
        let provider = VersionProvider::new(connection);

        for entity in provider.find().unwrap() {
            assert_eq!(
                VersionEntity {
                    database_version: "0.1.0".to_string()
                },
                entity
            );
        }
    }
}
