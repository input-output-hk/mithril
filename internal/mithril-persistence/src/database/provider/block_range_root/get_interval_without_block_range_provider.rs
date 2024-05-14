use crate::database::record::IntervalWithoutBlockRangeRootRecord;
use crate::sqlite::{Provider, SourceAlias, SqLiteEntity, SqliteConnection, WhereCondition};

/// Query that return the interval of block numbers that does not have a block range root.
pub struct GetIntervalWithoutBlockRangeRootProvider<'client> {
    connection: &'client SqliteConnection,
}

impl<'client> GetIntervalWithoutBlockRangeRootProvider<'client> {
    /// Create a new instance
    pub fn new(connection: &'client SqliteConnection) -> Self {
        Self { connection }
    }

    pub fn get_interval_without_block_range_condition(&self) -> WhereCondition {
        WhereCondition::default()
    }
}

impl<'client> Provider<'client> for GetIntervalWithoutBlockRangeRootProvider<'client> {
    type Entity = IntervalWithoutBlockRangeRootRecord;

    fn get_connection(&'client self) -> &'client SqliteConnection {
        self.connection
    }

    fn get_definition(&self, condition: &str) -> String {
        let aliases = SourceAlias::new(&[
            ("{:interval_start:}", "interval_start"),
            ("{:interval_end:}", "interval_end"),
        ]);
        let projection = Self::Entity::get_projection().expand(aliases);

        format!(
            "select {projection} from \
        (select max(end) as start from block_range_root where {condition}) as interval_start, \
        (select max(block_number) as end from cardano_tx where {condition}) as interval_end"
        )
    }
}
