use std::marker::PhantomData;

use sqlite::Cursor;

use super::SqLiteEntity;

pub struct EntityCursor<'a, T> {
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
    T: SqLiteEntity,
{
    type Item = T;

    /// Spawning entities from Result iterator.
    /// This iterator will crash the application if an error occures during this process.
    /// This is intended because it prevents inconsistent data to spread accross the application.
    fn next(&mut self) -> Option<T> {
        self.cursor
            .next()
            .map(|res| T::hydrate(res.unwrap()).unwrap())
    }
}
