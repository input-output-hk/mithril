use std::{iter::repeat, sync::Arc};

use anyhow::Context;
use mithril_common::{
    crypto_helper::{Bytes, MKTreeLeafIndexer, MKTreeLeafPosition, MKTreeNode, MKTreeStorer},
    StdResult,
};

/// A Merkle tree store with Sqlite backend
/// * This store is used to store the Merkle tree nodes in a Sqlite database with in memory mode.
/// * This store is not suited for proving, it is only used to store the compute the root of a Merkle tree.
/// * This store is slower than the in memory store but uses less memory which is important to minimize the signer footprint on the SPO infrastructure.
pub struct MKTreeStoreSqlite {
    inner_store: sqlite::ConnectionThreadSafe,
}

impl MKTreeStoreSqlite {
    fn build() -> StdResult<Self> {
        Ok(Self {
            inner_store: Self::create_connection()?,
        })
    }

    fn create_connection() -> StdResult<sqlite::ConnectionThreadSafe> {
        let connection = sqlite::Connection::open_thread_safe(":memory:")?;
        connection.execute("pragma shrink_memory; pragma temp_store = FILE;")?;
        connection.execute(
            "create table merkle_tree (
                position integer, 
                element blob, 
                primary key (position)
            )",
        )?;

        Ok(connection)
    }

    fn get_element_at_position(&self, position: u64) -> StdResult<Option<Arc<MKTreeNode>>> {
        let query = "SELECT element FROM merkle_tree WHERE position = ?";
        let mut statement = self.inner_store.prepare(query)?;
        statement.bind((1, position as i64)).unwrap();
        let result = if let Ok(sqlite::State::Row) = statement.next() {
            Some(Arc::new(MKTreeNode::new(
                statement.read::<Bytes, _>("element")?,
            )))
        } else {
            None
        };

        Ok(result)
    }

    fn insert_elements_from_position(
        &self,
        position: u64,
        elements: Vec<Arc<MKTreeNode>>,
    ) -> StdResult<()> {
        let values_columns: Vec<&str> = repeat("(?, ?)").take(elements.len()).collect();
        let values: Vec<sqlite::Value> = elements
            .into_iter()
            .enumerate()
            .flat_map(|(i, elem)| {
                vec![
                    sqlite::Value::Integer((position + i as u64) as i64),
                    sqlite::Value::Binary((**elem).to_vec()),
                ]
            })
            .collect();
        let query = format!(
            "INSERT INTO merkle_tree(position, element) VALUES {}",
            values_columns.join(", ")
        );
        let mut statement = self.inner_store.prepare(query)?;
        statement.bind::<&[(_, sqlite::Value)]>(
            values
                .into_iter()
                .enumerate()
                .map(|(i, v)| (i + 1, v))
                .collect::<Vec<_>>()
                .as_slice(),
        )?;
        statement.next()?;

        Ok(())
    }
}

impl Clone for MKTreeStoreSqlite {
    fn clone(&self) -> Self {
        unimplemented!("Clone is not implemented for MKTreeStoreSqlite")
    }
}

impl MKTreeLeafIndexer for MKTreeStoreSqlite {
    fn set_leaf_position(&self, _pos: MKTreeLeafPosition, _node: Arc<MKTreeNode>) -> StdResult<()> {
        Ok(())
    }

    fn get_leaf_position(&self, _node: &MKTreeNode) -> Option<MKTreeLeafPosition> {
        unimplemented!("get_leaf_position is not implemented for MKTreeStoreSqlite")
    }

    fn total_leaves(&self) -> usize {
        unimplemented!("total_leaves is not implemented for MKTreeStoreSqlite")
    }

    fn leaves(&self) -> Vec<MKTreeNode> {
        unimplemented!("leaves is not implemented for MKTreeStoreSqlite")
    }
}

impl MKTreeStorer for MKTreeStoreSqlite {
    fn build() -> StdResult<Self> {
        Self::build()
    }

    fn get_elem(&self, pos: u64) -> StdResult<Option<Arc<MKTreeNode>>> {
        self.get_element_at_position(pos).with_context(|| {
            format!("MKTreeStoreSqlite failed to retrieve element at position {pos}")
        })
    }

    fn append(&self, pos: u64, elems: Vec<Arc<MKTreeNode>>) -> StdResult<()> {
        self.insert_elements_from_position(pos, elems)
            .with_context(|| {
                format!("MKTreeStoreSqlite failed to insert elements from position {pos}")
            })
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::crypto_helper::MKTree;

    use super::*;

    #[test]
    fn test_golden_merkle_root() {
        let leaves = vec!["golden-1", "golden-2", "golden-3", "golden-4", "golden-5"];
        let mktree =
            MKTree::<MKTreeStoreSqlite>::new(&leaves).expect("MKTree creation should not fail");
        let mkroot = mktree
            .compute_root()
            .expect("MKRoot generation should not fail");

        assert_eq!(
            "3bbced153528697ecde7345a22e50115306478353619411523e804f2323fd921",
            mkroot.to_hex()
        );
    }
}
