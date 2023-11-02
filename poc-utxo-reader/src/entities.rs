use crate::errors::*;
use hex::ToHex;
use pallas::ledger::{primitives::babbage::PseudoDatumOption, traverse::MultiEraBlock};

pub type Era = String;
pub type SlotNumber = u64;
pub type BlockNumber = u64;
pub type Address = String;
pub type Lovelace = i128;
pub type TransactionHash = String;
pub type TransactionDataHash = String;
pub type TransactionIndex = u64;

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct TransactionInput {
    pub output_ref: TransactionOutputRef,
}

impl From<TransactionOutputRef> for TransactionInput {
    fn from(other: TransactionOutputRef) -> Self {
        Self { output_ref: other }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct TransactionOutput {
    pub address: Address,
    pub quantity: Lovelace,
    pub data_hash: TransactionDataHash,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct TransactionOutputRef {
    pub hash: TransactionHash,
    pub index: TransactionIndex,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct TransactionAddressRecord {
    pub hash: TransactionHash,
    pub quantity: Lovelace,
    pub data_hash: TransactionDataHash,
}

#[derive(Debug)]
pub struct Transaction {
    pub hash: TransactionHash,
    pub inputs: Vec<TransactionInput>,
    pub outputs: Vec<(TransactionIndex, TransactionOutput)>,
}

#[derive(Debug)]
pub struct Block {
    pub era: Era,
    pub number: BlockNumber,
    pub slot: SlotNumber,
    pub transactions: Vec<Transaction>,
}

impl TryFrom<MultiEraBlock<'_>> for Block {
    type Error = StdError;

    fn try_from(multi_era_block: MultiEraBlock) -> Result<Self, Self::Error> {
        let mut transactions = Vec::new();
        for tx in &multi_era_block.txs() {
            let tx_hash = tx.hash();
            let mut transactions_inputs = Vec::new();
            for tx_input in tx.consumes() {
                transactions_inputs.push(TransactionInput {
                    output_ref: TransactionOutputRef {
                        hash: tx_input.hash().to_string(),
                        index: tx_input.index(),
                    },
                });
            }
            let mut transactions_outputs = Vec::new();
            for (tx_index, tx_output) in tx.produces() {
                // TODO: Maybe use a different address format when error (Byron)
                let address = match tx_output.address().unwrap().to_bech32() {
                    Ok(address) => address,
                    Err(_) => tx_output.address().unwrap().to_hex(),
                };
                transactions_outputs.push((
                    tx_index as u64,
                    TransactionOutput {
                        address,
                        quantity: tx_output.lovelace_amount() as Lovelace,
                        data_hash: tx_output
                            .datum()
                            .map(|datum| match datum {
                                PseudoDatumOption::Hash(hash) => {
                                    hash.as_slice().encode_hex::<String>()
                                }
                                // TODO: handle the case of Data
                                PseudoDatumOption::Data(_cbor_wrap) => "".to_string(),
                            })
                            .unwrap_or_default(),
                    },
                ));
            }
            let transaction = Transaction {
                hash: tx_hash.to_string(),
                inputs: transactions_inputs,
                outputs: transactions_outputs,
            };
            transactions.push(transaction);
        }
        let block = Block {
            era: multi_era_block.era().to_string(),
            number: multi_era_block.number(),
            slot: multi_era_block.slot(),
            transactions,
        };

        Ok(block)
    }
}
