use crate::{errors::*, merkle_tree::MKTreeNode};
use blake2::{Blake2s256, Digest};
use hex::ToHex;
use pallas::ledger::{
    primitives::babbage::PseudoDatumOption,
    traverse::{ComputeHash, MultiEraBlock},
};
use serde::{Deserialize, Serialize};

pub type Bytes = Vec<u8>;
pub type Era = String;
pub type SlotNumber = u64;
pub type BlockNumber = u64;
pub type ImmutableFileNumber = usize;
pub type Address = String;
pub type Lovelace = i128;
pub type TransactionHash = String;
pub type TransactionDataHash = String;
pub type TransactionIndex = u64;

#[derive(Debug, PartialEq, Eq, Clone, Hash, Serialize, Deserialize)]
pub struct TransactionInput {
    pub output_ref: TransactionOutputRef,
}

impl From<TransactionOutputRef> for TransactionInput {
    fn from(other: TransactionOutputRef) -> Self {
        Self { output_ref: other }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Serialize, Deserialize)]
pub struct TransactionOutput {
    pub output_ref: TransactionOutputRef,
    pub address: Address,
    pub quantity: Lovelace,
    pub data_hash: Option<TransactionDataHash>,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash, Serialize, Deserialize)]
pub struct TransactionOutputRef {
    pub hash: TransactionHash,
    pub index: TransactionIndex,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Transaction {
    pub block_number: BlockNumber,
    pub hash: TransactionHash,
    pub inputs: Vec<TransactionInput>,
    pub outputs: Vec<TransactionOutput>,
}

#[derive(Debug, PartialEq, Eq, Clone, Serialize, Deserialize)]
pub struct UTxO {
    pub address: Address,
    pub hash: TransactionHash,
    pub index: TransactionIndex,
    pub quantity: Lovelace,
    pub data_hash: Option<TransactionDataHash>,
}

impl UTxO {
    pub fn compute_hash(&self) -> Bytes {
        let mut hasher = Blake2s256::new();
        hasher.update(self.address.as_bytes());
        hasher.update(self.hash.as_bytes());
        hasher.update(self.index.to_be_bytes());
        hasher.update(self.quantity.to_be_bytes());
        if let Some(data_hash) = &self.data_hash {
            hasher.update(data_hash.as_bytes());
        }
        let hash = hasher.finalize();
        hash.to_vec()
    }
}

impl From<UTxO> for MKTreeNode {
    fn from(other: UTxO) -> MKTreeNode {
        MKTreeNode::new(other.compute_hash())
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Block {
    pub era: Era,
    pub number: BlockNumber,
    pub slot_number: SlotNumber,
    pub immutable_file_number: ImmutableFileNumber,
    pub transactions: Vec<Transaction>,
}

impl Block {
    pub fn try_convert(
        multi_era_block: MultiEraBlock,
        immutable_file_number: ImmutableFileNumber,
    ) -> StdResult<Self> {
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
                // TODO: Maybe use a different address format when error (Byron) / Base58
                let address = match tx_output.address().unwrap().to_bech32() {
                    Ok(address) => address,
                    Err(_) => tx_output.address().unwrap().to_hex(),
                };
                transactions_outputs.push(TransactionOutput {
                    output_ref: TransactionOutputRef {
                        hash: tx_hash.to_string(),
                        index: tx_index as u64,
                    },
                    address,
                    quantity: tx_output.lovelace_amount() as Lovelace,
                    data_hash: tx_output.datum().map(|datum| match datum {
                        PseudoDatumOption::Hash(hash) => hash.as_slice().encode_hex::<String>(),
                        PseudoDatumOption::Data(cbor_wrap) => {
                            cbor_wrap.compute_hash().as_slice().encode_hex::<String>()
                        }
                    }),
                });
            }
            let transaction = Transaction {
                block_number: multi_era_block.number(),
                hash: tx_hash.to_string(),
                inputs: transactions_inputs,
                outputs: transactions_outputs,
            };
            transactions.push(transaction);
        }
        let block = Block {
            era: multi_era_block.era().to_string(),
            number: multi_era_block.number(),
            slot_number: multi_era_block.slot(),
            immutable_file_number,
            transactions,
        };

        Ok(block)
    }
}
