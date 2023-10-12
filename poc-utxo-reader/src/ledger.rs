use pallas::{
    codec::minicbor::{decode::Tokenizer, Decoder},
    ledger::traverse::{
        probe::{block_era, Outcome},
        Era, MultiEraBlock, MultiEraInput, MultiEraOutput,
    },
};
use rayon::prelude::*;
use std::{
    cmp::min,
    collections::{BTreeMap, HashMap},
    fs,
    path::{Path, PathBuf},
};

pub type SlotNumber = u64;
pub type BlockNumber = u64;
pub type Address = String;
pub type Lovelace = u64;
pub type TransactionHash = String;
pub type TransactionIndex = u64;

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct TransactionInput {
    pub hash: TransactionHash,
    pub index: TransactionIndex,
}

#[derive(Debug)]
pub struct TransactionOutput {
    pub hash: TransactionHash,
    pub index: TransactionIndex,
    pub address: Address,
    pub amount: Lovelace,
}

impl From<&TransactionOutput> for TransactionInput {
    fn from(value: &TransactionOutput) -> Self {
        TransactionInput {
            hash: value.hash.to_owned(),
            index: value.index,
        }
    }
}

#[derive(Debug)]
pub struct Transaction {
    pub hash: TransactionHash,
    pub inputs: Vec<TransactionInput>,
    pub outputs: Vec<TransactionOutput>,
}

#[derive(Debug)]
pub struct Block {
    pub era: Era,
    pub number: BlockNumber,
    pub slot: SlotNumber,
    pub transactions: Vec<Transaction>,
    pub total_transactions: u64,
}

pub type AddressTransactionHistory = BTreeMap<Address, Vec<TransactionHash>>;
pub type UnspentTransactionxOutput = HashMap<TransactionInput, Address>;
