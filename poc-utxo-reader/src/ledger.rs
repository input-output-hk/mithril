use crate::{entities::*, errors::*};
use sqlite::{Connection, Value};
use std::collections::BTreeMap;
use std::iter::repeat;

pub struct Ledger {
    connection: Connection,
}

impl Ledger {
    /// Ledger factory
    pub fn new(connection: Connection) -> StdResult<Self> {
        Self::setup_db(&connection)?;
        Ok(Self { connection })
    }

    pub fn setup_db(connection: &Connection) -> StdResult<()> {
        let query = r#"
/* Create 'block' table */        
CREATE TABLE IF NOT EXISTS block (
    number                integer      not null,
    slot_number           integer      not null,
    era                   text         not null,
    immutable_file_number integer      not null,
    primary key (number)
);

/* Create 'tx' table */
CREATE TABLE IF NOT EXISTS tx (
    hash          text         not null,
    block_number  integer      not null,
    primary key (hash),
    foreign key (block_number) references block(number)
);

/* Create 'tx_out' table */
CREATE TABLE IF NOT EXISTS tx_out (
    tx_hash       text         not null,
    tx_index      integer      not null,
    address       text         not null,
    quantity      integer      not null,
    data_hash     text,
    primary key (tx_hash, tx_index),
    foreign key (tx_hash) references tx(hash)
);

/* Create 'tx_in' table */
CREATE TABLE IF NOT EXISTS tx_in (
    tx_hash       text         not null,
    tx_index      integer      not null,
    primary key (tx_hash, tx_index),
    foreign key (tx_hash) references tx(hash)
);
        "#;
        Ok(connection.execute(query)?)
    }

    /// Save a set of blocks to the ledger
    pub fn save_blocks(&self, blocks: &[Block]) -> StdResult<()> {
        let chunk_size = 500;

        let blocks: Vec<_> = blocks
            .iter()
            .filter(|block| !block.transactions.is_empty())
            .collect();
        for blocks_chunks in blocks.chunks(chunk_size) {
            self.store_block_records(blocks_chunks)?;
        }

        let txs: Vec<_> = blocks
            .iter()
            .flat_map(|block| block.transactions.as_slice())
            .collect();
        for txs_chunks in txs.chunks(chunk_size) {
            self.store_tx_records(txs_chunks)?;
        }

        let txs_in: Vec<_> = blocks
            .iter()
            .flat_map(|block| block.transactions.as_slice())
            .flat_map(|tx| tx.inputs.as_slice())
            .collect();
        for txs_in_chunks in txs_in.chunks(chunk_size) {
            self.store_tx_input_records(txs_in_chunks)?;
        }

        let txs_out: Vec<_> = blocks
            .iter()
            .flat_map(|block| block.transactions.as_slice())
            .flat_map(|tx| tx.outputs.as_slice())
            .collect();
        for txs_out_chunks in txs_out.chunks(chunk_size) {
            self.store_tx_output_records(txs_out_chunks)?;
        }

        Ok(())
    }

    // Store block records in the database
    pub(crate) fn store_block_records(&self, blocks: &[&Block]) -> StdResult<()> {
        if blocks.is_empty() {
            return Ok(());
        }

        let values_columns: Vec<&str> = repeat("(?, ?, ?, ?)").take(blocks.len()).collect();
        let query = format!(
            "INSERT OR REPLACE INTO block (number, slot_number, era, immutable_file_number) VALUES {}",
            values_columns.join(", ")
        );
        let mut statement = self.connection.prepare(query)?;
        let mut bind_idx = 0;
        for block in blocks {
            statement.bind::<&[(_, Value)]>(&[
                (bind_idx + 1, Value::Integer(block.number as i64)),
                (bind_idx + 2, Value::Integer(block.slot_number as i64)),
                (bind_idx + 3, Value::String(block.era.to_owned())),
                (
                    bind_idx + 4,
                    Value::Integer(block.immutable_file_number as i64),
                ),
            ])?;
            bind_idx += 4;
        }
        statement.next()?;

        Ok(())
    }

    // Store tx records of a block in the database
    pub(crate) fn store_tx_records(&self, txs: &[&Transaction]) -> StdResult<()> {
        if txs.is_empty() {
            return Ok(());
        }

        let values_columns: Vec<&str> = repeat("(?, ?)").take(txs.len()).collect();
        let query = format!(
            "INSERT OR REPLACE INTO tx (hash, block_number) VALUES {}",
            values_columns.join(", ")
        );
        let mut statement = self.connection.prepare(query)?;
        let mut bind_idx = 0;
        for tx in txs {
            statement.bind::<&[(_, Value)]>(&[
                (bind_idx + 1, Value::String(tx.hash.to_owned())),
                (bind_idx + 2, Value::Integer(tx.block_number as i64)),
            ])?;
            bind_idx += 2;
        }
        statement.next()?;

        Ok(())
    }

    // Store tx_out records in the database
    pub(crate) fn store_tx_output_records(&self, txs_out: &[&TransactionOutput]) -> StdResult<()> {
        if txs_out.is_empty() {
            return Ok(());
        }

        let values_columns: Vec<&str> = repeat("(?, ?, ?, ?, ?)").take(txs_out.len()).collect();
        let query = format!(
            "INSERT OR REPLACE INTO tx_out (tx_hash, tx_index, address, quantity, data_hash) VALUES {}",
            values_columns.join(", ")
        );
        let mut statement = self.connection.prepare(query)?;
        let mut bind_idx = 0;
        for tx_out in txs_out {
            statement.bind::<&[(_, Value)]>(&[
                (
                    bind_idx + 1,
                    Value::String(tx_out.output_ref.hash.to_owned()),
                ),
                (bind_idx + 2, Value::Integer(tx_out.output_ref.index as i64)),
                (bind_idx + 3, Value::String(tx_out.address.to_owned())),
                (bind_idx + 4, Value::Integer(tx_out.quantity as i64)),
                (
                    bind_idx + 5,
                    tx_out
                        .data_hash
                        .as_ref()
                        .map(|d| Value::String(d.to_string()))
                        .unwrap_or(Value::Null),
                ),
            ])?;
            bind_idx += 5;
        }
        statement.next()?;

        Ok(())
    }

    // Store a tx_in record in the database
    pub(crate) fn store_tx_input_records(&self, txs_in: &[&TransactionInput]) -> StdResult<()> {
        if txs_in.is_empty() {
            return Ok(());
        }

        let values_columns: Vec<&str> = repeat("(?, ?)").take(txs_in.len()).collect();
        let query = format!(
            "INSERT OR REPLACE INTO tx_in (tx_hash, tx_index) VALUES {}",
            values_columns.join(", ")
        );
        let mut statement = self.connection.prepare(query)?;
        let mut bind_idx = 0;
        for tx_in in txs_in {
            statement.bind::<&[(_, Value)]>(&[
                (
                    bind_idx + 1,
                    Value::String(tx_in.output_ref.hash.to_owned()),
                ),
                (bind_idx + 2, Value::Integer(tx_in.output_ref.index as i64)),
            ])?;
            bind_idx += 2;
        }
        statement.next()?;

        Ok(())
    }

    /// Get all addresses transaction history
    pub fn get_utxos_for_all_addresses(
        &self,
        immutable_file_number: &ImmutableFileNumber,
    ) -> StdResult<BTreeMap<Address, Vec<UTxO>>> {
        let query = r#"
SELECT DISTINCT tx_out.address,
    tx.hash AS tx_hash,
    tx_out.tx_index,
    tx_out.quantity,
    tx_out.data_hash
FROM tx_out
    INNER JOIN tx ON tx.hash = tx_out.tx_hash
    INNER JOIN block AS block_out ON tx.block_number = block_out.number
    LEFT JOIN tx_in ON tx_out.tx_hash = tx_in.tx_hash
    LEFT JOIN tx tx2 ON tx2.hash = tx_in.tx_hash
    LEFT JOIN block block_in ON block_in.number = tx2.block_number
WHERE block_out.immutable_file_number <= ?1
    AND (tx_in.tx_hash IS NULL OR block_in.immutable_file_number > ?1)
ORDER BY tx_out.rowid;        
        "#;
        let mut statement = self.connection.prepare(query)?;
        statement.bind::<&[(_, Value)]>(&[(1, Value::Integer(*immutable_file_number as i64))])?;

        let utxos: Vec<_> = statement
            .into_iter()
            .map(|row| row.unwrap())
            .map(|row| UTxO {
                quantity: row.read::<i64, _>("quantity") as i128,
                hash: row.read::<&str, _>("tx_hash").to_string(),
                index: row.read::<i64, _>("tx_index") as u64,
                address: row.read::<&str, _>("address").to_string(),
                data_hash: row
                    .read::<Option<&str>, _>("data_hash")
                    .map(|s| s.to_string()),
            })
            .collect();

        let mut utxos_by_address: BTreeMap<Address, Vec<UTxO>> = BTreeMap::new();
        for utxo in utxos {
            if let Some(utxos_for_address) = utxos_by_address.get_mut(&utxo.address) {
                utxos_for_address.push(utxo);
            } else {
                utxos_by_address.insert(utxo.address.clone(), vec![utxo]);
            }
        }

        Ok(utxos_by_address)
    }

    /// Get transaction history for an address
    pub fn get_utxos_for_address(
        &self,
        address: &Address,
        immutable_file_number: &ImmutableFileNumber,
    ) -> StdResult<Vec<UTxO>> {
        let query = r#"
SELECT DISTINCT tx_out.address,
    tx.hash AS tx_hash,
    tx_out.tx_index,
    tx_out.quantity,
    tx_out.data_hash
FROM tx_out
    INNER JOIN tx ON tx.hash = tx_out.tx_hash
    INNER JOIN block AS block_out ON tx.block_number = block_out.number
    LEFT JOIN tx_in ON tx_out.tx_hash = tx_in.tx_hash
    LEFT JOIN tx tx2 ON tx2.hash = tx_in.tx_hash
    LEFT JOIN block block_in ON block_in.number = tx2.block_number
WHERE block_out.immutable_file_number <= ?1
    AND (tx_in.tx_hash IS NULL OR block_in.immutable_file_number > ?1)
    AND tx_out.address = ?2
ORDER BY tx_out.rowid;        
        "#;
        let mut statement = self.connection.prepare(query)?;
        statement.bind::<&[(_, Value)]>(&[
            (1, Value::Integer(*immutable_file_number as i64)),
            (2, Value::String(address.to_owned())),
        ])?;

        Ok(statement
            .into_iter()
            .map(|row| row.unwrap())
            .map(|row| UTxO {
                address: row.read::<&str, _>("address").to_string(),
                hash: row.read::<&str, _>("tx_hash").to_string(),
                index: row.read::<i64, _>("tx_index") as u64,
                quantity: row.read::<i64, _>("quantity") as i128,
                data_hash: row
                    .read::<Option<&str>, _>("data_hash")
                    .map(|s| s.to_string()),
            })
            .collect())
    }

    /// Get balance for an address
    pub fn get_balance_for_address(
        &self,
        address: &Address,
        immutable_file_number: &ImmutableFileNumber,
    ) -> StdResult<Lovelace> {
        let balance = self
            .get_utxos_for_address(address, immutable_file_number)?
            .iter()
            .fold(0, |acc, record| acc + record.quantity);

        Ok(balance)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use sqlite::Connection;

    fn fake_transaction_output(
        address: Address,
        amount: Lovelace,
        tx_hash: TransactionHash,
        tx_index: TransactionIndex,
    ) -> TransactionOutput {
        TransactionOutput {
            output_ref: TransactionOutputRef {
                hash: tx_hash.clone(),
                index: tx_index,
            },
            address,
            quantity: amount,
            data_hash: Some(format!("data-hash-{tx_hash}-{tx_index}")),
        }
    }

    fn fake_transaction(
        tx_inputs: &[TransactionInput],
        tx_outputs: &[TransactionOutput],
        tx_hash: TransactionHash,
        block_number: BlockNumber,
    ) -> Transaction {
        Transaction {
            block_number,
            hash: tx_hash,
            inputs: tx_inputs.to_vec(),
            outputs: tx_outputs
                .iter()
                .map(|tx_output| tx_output.to_owned())
                .collect::<Vec<TransactionOutput>>(),
        }
    }

    #[test]
    fn ledger_should_return_correct_balance() {
        let connection = Connection::open(":memory:").unwrap();
        let ledger = Ledger::new(connection).unwrap();
        let max_immutable_file_number_query = (u32::MAX - 1) as usize;

        let block_number1 = 1;
        let address1 = "addr_test_123".to_string();
        let address2 = "addr_test_456".to_string();
        let address3 = "addr_test_7896".to_string();

        let balance = ledger
            .get_balance_for_address(&address1, &max_immutable_file_number_query)
            .unwrap();
        assert_eq!(0, balance);

        let tx_hash1 = "hash-789".to_string();
        let tx_output1 = fake_transaction_output(address1.clone(), 100, "hash-123".to_string(), 2);
        let tx_output2 = fake_transaction_output(address2.clone(), 250, "hash-456".to_string(), 1);
        let tx_output3 = fake_transaction_output(address3.clone(), 350, tx_hash1.clone(), 3);
        let tx1 = fake_transaction(
            &[tx_output1.output_ref.into(), tx_output2.output_ref.into()],
            &[tx_output3],
            tx_hash1.clone(),
            block_number1,
        );
        let block1 = Block {
            era: "era1".to_string(),
            number: block_number1,
            slot_number: 123,
            immutable_file_number: 1,
            transactions: vec![tx1],
        };
        ledger
            .save_blocks(&[block1])
            .expect("save transaction should not fail");
        let balance1 = ledger
            .get_balance_for_address(&address1, &max_immutable_file_number_query)
            .unwrap();
        assert_eq!(0, balance1);
        let balance2 = ledger
            .get_balance_for_address(&address2, &max_immutable_file_number_query)
            .unwrap();
        assert_eq!(0, balance2);
        let balance3 = ledger
            .get_balance_for_address(&address3, &max_immutable_file_number_query)
            .unwrap();
        assert_eq!(350, balance3);

        let block_number2 = 2;
        let tx_hash2 = "hash-000".to_string();
        let tx_output4 = fake_transaction_output(address3.clone(), 350, tx_hash1.clone(), 0);
        let tx_output5 = fake_transaction_output(address1.clone(), 250, tx_hash2.clone(), 0);
        let tx_output6 = fake_transaction_output(address3.clone(), 100, tx_hash2.clone(), 1);
        let tx2 = fake_transaction(
            &[tx_output4.output_ref.into()],
            &[tx_output5, tx_output6],
            tx_hash2,
            block_number2,
        );
        let block2 = Block {
            era: "era1".to_string(),
            number: block_number2,
            slot_number: 456,
            immutable_file_number: 2,
            transactions: vec![tx2],
        };
        ledger
            .save_blocks(&[block2])
            .expect("save transaction should not fail");
        let balance1 = ledger
            .get_balance_for_address(&address1, &max_immutable_file_number_query)
            .unwrap();
        assert_eq!(250, balance1);
        let balance2 = ledger
            .get_balance_for_address(&address2, &max_immutable_file_number_query)
            .unwrap();
        assert_eq!(0, balance2);
        let balance3 = ledger
            .get_balance_for_address(&address3, &max_immutable_file_number_query)
            .unwrap();
        assert_eq!(100, balance3);
    }
}
