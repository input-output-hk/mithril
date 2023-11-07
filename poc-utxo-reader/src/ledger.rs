use std::collections::BTreeMap;

use crate::{entities::*, errors::*};
use sqlite::{ConnectionWithFullMutex, Value};

pub struct Ledger {
    connection: ConnectionWithFullMutex,
}

impl Ledger {
    /// Ledger factory
    pub fn new(connection: ConnectionWithFullMutex) -> StdResult<Self> {
        Self::setup_db(&connection)?;
        Ok(Self { connection })
    }

    pub fn setup_db(connection: &ConnectionWithFullMutex) -> StdResult<()> {
        let query = r#"
/* Create 'block' table */        
CREATE TABLE IF NOT EXISTS block (
    number        integer      not null,
    slot_number   integer      not null,
    era           text         not null,
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
    data_hash     text         not null,
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
        for block in blocks {
            self.save_block(block)?;
        }

        Ok(())
    }

    /// Save a block to the ledger
    pub fn save_block(&self, block: &Block) -> StdResult<()> {
        if !block.transactions.is_empty() {
            self.store_block_record(block)?;
        }

        for tx in &block.transactions {
            self.save_transaction(tx, block.number)?;
        }

        Ok(())
    }

    /// Save a transaction to the ledger
    pub fn save_transaction(&self, tx: &Transaction, block_number: BlockNumber) -> StdResult<()> {
        self.store_tx_record(tx, block_number)?;

        for tx_input in &tx.inputs {
            self.store_tx_in_record(tx_input)?;
        }
        for (tx_output_index, tx_output) in &tx.outputs {
            let tx_output_ref = TransactionOutputRef {
                hash: tx.hash.to_owned(),
                index: *tx_output_index,
            };
            self.store_tx_out_record(tx_output, &tx_output_ref)?;
        }

        Ok(())
    }

    // Store a block record in the database
    pub(crate) fn store_block_record(&self, block: &Block) -> StdResult<()> {
        let query = "INSERT OR REPLACE INTO block (number, slot_number, era) VALUES (?1, ?2, ?3)";
        let mut statement = self.connection.prepare(query)?;
        statement.bind::<&[(_, Value)]>(&[
            (1, Value::Integer(block.number as i64)),
            (2, Value::Integer(block.slot_number as i64)),
            (3, block.era.to_owned().into()),
        ])?;
        statement.next()?;

        Ok(())
    }

    // Store a tx record in the database
    pub(crate) fn store_tx_record(
        &self,
        tx: &Transaction,
        block_number: BlockNumber,
    ) -> StdResult<()> {
        let query = "INSERT OR REPLACE INTO tx (hash, block_number) VALUES (?1, ?2)";
        let mut statement = self.connection.prepare(query)?;
        statement.bind::<&[(_, Value)]>(&[
            (1, tx.hash.to_owned().into()),
            (2, Value::Integer(block_number as i64)),
        ])?;
        statement.next()?;

        Ok(())
    }

    // Store a tx_out record in the database
    pub(crate) fn store_tx_out_record(
        &self,
        tx_out: &TransactionOutput,
        tx_out_ref: &TransactionOutputRef,
    ) -> StdResult<()> {
        let query = "INSERT OR REPLACE INTO tx_out (tx_hash, tx_index, address, quantity, data_hash) VALUES (?1, ?2, ?3, ?4, ?5)";
        let mut statement = self.connection.prepare(query)?;
        statement.bind::<&[(_, Value)]>(&[
            (1, tx_out_ref.hash.to_owned().into()),
            (2, Value::Integer(tx_out_ref.index as i64)),
            (3, tx_out.address.to_owned().into()),
            (4, Value::Integer(tx_out.quantity as i64)),
            (5, tx_out.data_hash.to_owned().into()),
        ])?;
        statement.next()?;

        Ok(())
    }

    // Store a tx_in record in the database
    pub(crate) fn store_tx_in_record(&self, tx_in: &TransactionInput) -> StdResult<()> {
        let query = "INSERT OR REPLACE INTO tx_in (tx_hash, tx_index) VALUES (?1, ?2)";
        let mut statement = self.connection.prepare(query)?;
        statement.bind::<&[(_, Value)]>(&[
            (1, tx_in.output_ref.hash.to_owned().into()),
            (2, Value::Integer(tx_in.output_ref.index as i64)),
        ])?;
        statement.next()?;

        Ok(())
    }

    /// Get all addresses transaction history
    pub fn get_utxos_for_all_addresses(
        &self,
        block_number: &BlockNumber,
    ) -> StdResult<BTreeMap<Address, Vec<UnspentTransactionOutput>>> {
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
WHERE block_out.number <= ?1
    AND (tx_in.tx_hash IS NULL OR block_in.number > ?1)
ORDER BY tx_out.rowid;        
        "#;
        let mut statement = self.connection.prepare(query)?;
        statement.bind::<&[(_, Value)]>(&[(1, Value::Integer(*block_number as i64))])?;

        let utxos: Vec<_> = statement
            .into_iter()
            .map(|row| row.unwrap())
            .map(|row| UnspentTransactionOutput {
                address: row.read::<&str, _>("address").to_string(),
                tx_hash: row.read::<&str, _>("tx_hash").to_string(),
                tx_index: row.read::<i64, _>("tx_index") as u64,
                quantity: row.read::<i64, _>("quantity") as i128,
                data_hash: row.read::<&str, _>("data_hash").to_string(),
            })
            .collect();

        let mut utxos_by_address: BTreeMap<Address, Vec<UnspentTransactionOutput>> =
            BTreeMap::new();
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
        block_number: &BlockNumber,
    ) -> StdResult<Vec<UnspentTransactionOutput>> {
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
WHERE block_out.number <= ?1
    AND (tx_in.tx_hash IS NULL OR block_in.number > ?1)
    AND tx_out.address = ?2
ORDER BY tx_out.rowid;        
        "#;
        let mut statement = self.connection.prepare(query)?;
        statement.bind::<&[(_, Value)]>(&[
            (1, Value::Integer(*block_number as i64)),
            (2, address.to_owned().into()),
        ])?;

        Ok(statement
            .into_iter()
            .map(|row| row.unwrap())
            .map(|row| UnspentTransactionOutput {
                address: row.read::<&str, _>("address").to_string(),
                tx_hash: row.read::<&str, _>("tx_hash").to_string(),
                tx_index: row.read::<i64, _>("tx_index") as u64,
                quantity: row.read::<i64, _>("quantity") as i128,
                data_hash: row.read::<&str, _>("data_hash").to_string(),
            })
            .collect())
    }

    /// Get balance for an address
    pub fn get_balance_for_address(
        &self,
        address: &Address,
        block_number: &BlockNumber,
    ) -> StdResult<Lovelace> {
        let balance = self
            .get_utxos_for_address(address, block_number)?
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
    ) -> (TransactionOutputRef, TransactionOutput) {
        let tx_output = TransactionOutput {
            address,
            quantity: amount,
            data_hash: format!("data-hash-{tx_hash}-{tx_index}"),
        };
        let tx_output_ref = TransactionOutputRef {
            hash: tx_hash,
            index: tx_index,
        };
        (tx_output_ref, tx_output)
    }

    fn fake_transaction(
        tx_inputs: &[TransactionInput],
        tx_outputs: &[TransactionOutput],
        tx_hash: TransactionHash,
    ) -> Transaction {
        Transaction {
            hash: tx_hash,
            inputs: tx_inputs.to_vec(),
            outputs: tx_outputs
                .iter()
                .enumerate()
                .map(|(idx, tx_output)| (idx as u64, tx_output.to_owned()))
                .collect::<Vec<(TransactionIndex, TransactionOutput)>>(),
        }
    }

    #[test]
    fn ledger_should_return_correct_balance() {
        let connection = Connection::open_with_full_mutex(":memory:").unwrap();
        let ledger = Ledger::new(connection).unwrap();

        let address1 = "addr_test_123".to_string();
        let address2 = "addr_test_456".to_string();
        let address3 = "addr_test_7896".to_string();

        let balance = ledger
            .get_balance_for_address(&address1, &u64::MAX)
            .unwrap();
        assert_eq!(0, balance);

        let tx_hash1 = "hash-789".to_string();
        let (tx_output_ref1, _tx_output1) =
            fake_transaction_output(address1.clone(), 100, "hash-123".to_string(), 2);
        let (tx_output_ref2, _tx_output2) =
            fake_transaction_output(address2.clone(), 250, "hash-456".to_string(), 1);
        let (_tx_output_ref3, tx_output3) =
            fake_transaction_output(address3.clone(), 350, tx_hash1.clone(), 3);
        let tx1 = fake_transaction(
            &[tx_output_ref1.into(), tx_output_ref2.into()],
            &[tx_output3],
            tx_hash1.clone(),
        );
        ledger
            .save_transaction(&tx1, 1)
            .expect("save transaction should not fail");
        let balance1 = ledger
            .get_balance_for_address(&address1, &u64::MAX)
            .unwrap();
        assert_eq!(0, balance1);
        let balance2 = ledger
            .get_balance_for_address(&address2, &u64::MAX)
            .unwrap();
        assert_eq!(0, balance2);
        let balance3 = ledger
            .get_balance_for_address(&address3, &u64::MAX)
            .unwrap();
        assert_eq!(350, balance3);

        let tx_hash2 = "hash-000".to_string();
        let (tx_output_ref4, _tx_output4) =
            fake_transaction_output(address3.clone(), 350, tx_hash1.clone(), 0);
        let (_tx_output_ref5, tx_output5) =
            fake_transaction_output(address1.clone(), 250, tx_hash2.clone(), 0);
        let (_tx_output_ref6, tx_output6) =
            fake_transaction_output(address3.clone(), 100, tx_hash2.clone(), 1);
        let tx2 = fake_transaction(
            &[tx_output_ref4.into()],
            &[tx_output5, tx_output6],
            tx_hash2,
        );
        ledger
            .save_transaction(&tx2, 1)
            .expect("save transaction should not fail");
        let balance1 = ledger
            .get_balance_for_address(&address1, &u64::MAX)
            .unwrap();
        assert_eq!(250, balance1);
        let balance2 = ledger
            .get_balance_for_address(&address2, &u64::MAX)
            .unwrap();
        assert_eq!(0, balance2);
        let balance3 = ledger
            .get_balance_for_address(&address3, &u64::MAX)
            .unwrap();
        assert_eq!(100, balance3);
    }
}
