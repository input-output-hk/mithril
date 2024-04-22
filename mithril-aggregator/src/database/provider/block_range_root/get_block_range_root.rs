use mithril_persistence::sqlite::{Provider, SourceAlias, SqLiteEntity, SqliteConnection};

use crate::database::record::BlockRangeRootRecord;

/// Simple queries to retrieve [BlockRangeRootRecord] from the sqlite database.
pub struct GetBlockRangeRootProvider<'client> {
    connection: &'client SqliteConnection,
}

impl<'client> GetBlockRangeRootProvider<'client> {
    #[cfg(test)]
    /// Create a new instance
    pub fn new(connection: &'client SqliteConnection) -> Self {
        Self { connection }
    }
}

#[cfg(test)]
impl mithril_persistence::sqlite::GetAllCondition for GetBlockRangeRootProvider<'_> {}

impl<'client> Provider<'client> for GetBlockRangeRootProvider<'client> {
    type Entity = BlockRangeRootRecord;

    fn get_connection(&'client self) -> &'client SqliteConnection {
        self.connection
    }

    fn get_definition(&self, condition: &str) -> String {
        let aliases = SourceAlias::new(&[("{:block_range_root:}", "block_range_root")]);
        let projection = Self::Entity::get_projection().expand(aliases);

        format!("select {projection} from block_range_root where {condition} order by rowid")
    }
}
