use std::marker::PhantomData;

use sqlite::Cursor;

use super::Entity;

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
    T: Entity,
{
    type Item = T;

    fn next(&mut self) -> Option<T> {
        self.cursor.next().map(|res| T::hydrate(res.unwrap()))
    }
}
