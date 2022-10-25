use sqlite::Row;

pub trait Entity {
    fn hydrate(row: Row) -> Self;
}
