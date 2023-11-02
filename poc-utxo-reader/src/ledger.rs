use crate::{entities::*, errors::*};
use anyhow::anyhow;
use std::collections::{BTreeMap, HashMap};

pub type AddressesTransactionsHistory = BTreeMap<Address, Vec<TransactionAddressRecord>>;
pub type UnspentTransactionxOutputs = HashMap<TransactionOutputRef, TransactionOutput>;

#[derive(Debug, Default)]
pub struct Ledger {
    address_transactions: AddressesTransactionsHistory,
    utxos: UnspentTransactionxOutputs,
}

impl Ledger {
    /// Save a set of transactions to the ledger
    pub fn save_transactions(&mut self, txs: &[Transaction]) -> StdResult<()> {
        for tx in txs {
            self.save_transaction(tx)?;
        }

        Ok(())
    }

    /// Save a transaction to the ledger
    pub fn save_transaction(&mut self, tx: &Transaction) -> StdResult<()> {
        for tx_input in &tx.inputs {
            if let Some(tx_output) = self.get_utxo(&tx_input.output_ref) {
                self.add_transaction_record_for_address(
                    tx_output.address.to_owned(),
                    TransactionAddressRecord {
                        hash: tx.hash.to_owned(),
                        quantity: -tx_output.quantity,
                        data_hash: tx_output.data_hash.to_owned(),
                    },
                );
            } else if !self.utxos.is_empty() {
                return Err(anyhow!(
                    "Missing transaction output for input {tx_input:#?}"
                ));
            }
            self.delete_utxo(&tx_input.output_ref);
        }
        for (tx_output_index, tx_output) in &tx.outputs {
            self.add_transaction_record_for_address(
                tx_output.address.to_owned(),
                TransactionAddressRecord {
                    hash: tx.hash.to_owned(),
                    quantity: tx_output.quantity,
                    data_hash: tx_output.data_hash.to_owned(),
                },
            );
            let tx_output_ref = TransactionOutputRef {
                hash: tx.hash.to_owned(),
                index: *tx_output_index,
            };
            self.save_utxo(tx_output.to_owned(), tx_output_ref);
        }

        Ok(())
    }

    /// Add transaction record for address
    pub(crate) fn add_transaction_record_for_address(
        &mut self,
        address: Address,
        transaction_record: TransactionAddressRecord,
    ) {
        if let Some(transaction_records) = self.address_transactions.get_mut(&address) {
            transaction_records.push(transaction_record); //TODO: deal with duplicates
        } else {
            self.address_transactions
                .insert(address, vec![transaction_record]);
        }
    }

    /// Get all addresses transaction history
    pub fn get_transactions_for_all_addresses(&self) -> &AddressesTransactionsHistory {
        &self.address_transactions
    }

    /// Get transaction history for an address
    pub fn get_transactions_for_address(
        &self,
        address: &Address,
    ) -> Option<&Vec<TransactionAddressRecord>> {
        self.address_transactions.get(address)
    }

    /// Get balance for an address
    pub fn get_balance_for_address(&self, address: &Address) -> Lovelace {
        let balance = if let Some(transactions) = self.address_transactions.get(address) {
            transactions
                .iter()
                .fold(0, |acc, record| acc + record.quantity)
        } else {
            0
        };

        balance
    }

    ///.Get UTxO
    pub(crate) fn get_utxo(
        &self,
        tx_output_ref: &TransactionOutputRef,
    ) -> Option<&TransactionOutput> {
        self.utxos.get(tx_output_ref)
    }

    /// Save UTxO
    pub(crate) fn save_utxo(
        &mut self,
        tx_output: TransactionOutput,
        tx_output_ref: TransactionOutputRef,
    ) {
        self.utxos.insert(tx_output_ref, tx_output);
    }

    /// Delete UTxO
    pub(crate) fn delete_utxo(&mut self, tx_output_ref: &TransactionOutputRef) {
        self.utxos.remove(tx_output_ref);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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
    fn ledger_should_save_and_delete_utxos() {
        let mut ledger = Ledger::default();

        let (tx_output_ref, tx_output) =
            fake_transaction_output("addr_test_123".to_string(), 100, "hash-123".to_string(), 2);

        let utxo = ledger.get_utxo(&tx_output_ref);
        assert_eq!(None, utxo);

        ledger.save_utxo(tx_output.clone(), tx_output_ref.clone());
        let utxo = ledger.get_utxo(&tx_output_ref);
        assert_eq!(Some(&tx_output), utxo);

        ledger.delete_utxo(&tx_output_ref);
        let utxo = ledger.get_utxo(&tx_output_ref);
        assert_eq!(None, utxo);
    }

    #[test]
    fn ledger_should_return_correct_balance() {
        let mut ledger = Ledger::default();

        let address1 = "addr_test_123".to_string();
        let address2 = "addr_test_456".to_string();
        let address3 = "addr_test_7896".to_string();

        let balance = ledger.get_balance_for_address(&address1);
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
            .save_transaction(&tx1)
            .expect("save transaction should not fail");
        let balance1 = ledger.get_balance_for_address(&address1);
        assert_eq!(0, balance1);
        let balance2 = ledger.get_balance_for_address(&address2);
        assert_eq!(0, balance2);
        let balance3 = ledger.get_balance_for_address(&address3);
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
            .save_transaction(&tx2)
            .expect("save transaction should not fail");
        let balance1 = ledger.get_balance_for_address(&address1);
        assert_eq!(250, balance1);
        let balance2 = ledger.get_balance_for_address(&address2);
        assert_eq!(0, balance2);
        let balance3 = ledger.get_balance_for_address(&address3);
        assert_eq!(100, balance3);
    }
}
