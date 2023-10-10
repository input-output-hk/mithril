#![allow(unused_imports)]
#![allow(unused_variables)]
#![allow(dead_code)]
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

type SlotNumber = u64;
type BlockNumber = u64;
type Address = String;
type Lovelace = u64;
type TransactionHash = String;
type TransactionIndex = u64;
type StdResult<T> = Result<T, String>;

#[derive(Debug, PartialEq, Eq, Hash)]
struct TransactionInput {
    hash: TransactionHash,
    index: TransactionIndex,
}

#[derive(Debug)]
struct TransactionOutput {
    hash: TransactionHash,
    index: TransactionIndex,
    address: Address,
    amount: Lovelace,
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
struct Transaction {
    hash: TransactionHash,
    inputs: Vec<TransactionInput>,
    outputs: Vec<TransactionOutput>,
}

#[derive(Debug)]
struct Block {
    era: Era,
    number: BlockNumber,
    slot: SlotNumber,
    transactions: Vec<Transaction>,
    total_transactions: u64,
}

type AddressTransactionHistory = BTreeMap<Address, Vec<TransactionHash>>;
type UnspentTransactionxOutput = HashMap<TransactionInput, Address>;

fn main() -> StdResult<()> {
    let immutable_directory_path = "./db/preprod/immutable";
    eprintln!(">> Scanning {immutable_directory_path:?} immutable files...");

    let immutable_chunk_file_paths = list_immutable_files(Path::new(immutable_directory_path))?;

    let blocks_by_immutable_file: BTreeMap<_, _> = immutable_chunk_file_paths
        .par_iter()
        .map(|immutable_chunk_file_path| {
            (
                immutable_chunk_file_path,
                read_immutable_file(immutable_chunk_file_path).unwrap(),
            )
        })
        .collect();
    let (total_blocks, total_transactions) = blocks_by_immutable_file
        .iter()
        .flat_map(|blocks| blocks.1)
        .fold((0, 0), |acc, block| {
            (acc.0 + 1, acc.1 + block.total_transactions)
        });
    eprintln!(
        ">> Found {total_blocks} blocks with {total_transactions} transactions in {} immutable files",
        immutable_chunk_file_paths.len()
    );

    let mut transactions_history: AddressTransactionHistory = BTreeMap::new();
    for (immutable_file_path, blocks) in &blocks_by_immutable_file {
        eprintln!(
            ">> Compute addresses transaction history for blocks from {immutable_file_path:?}",
        );
        compute_transaction_history_from_blocks(&mut transactions_history, blocks)?;
    }
    println!("{:#?}", transactions_history);

    Ok(())
}

fn list_immutable_files(directory_path: &Path) -> StdResult<Vec<PathBuf>> {
    let entries = fs::read_dir(directory_path).unwrap();
    let mut immutable_chunk_file_paths = Vec::new();
    for entry in entries {
        let path = entry.unwrap().path();
        let extension = path.extension().unwrap();
        if extension.to_str().unwrap() == "chunk" {
            immutable_chunk_file_paths.push(path);
        }
    }
    immutable_chunk_file_paths.sort();
    immutable_chunk_file_paths.pop().unwrap(); // Keep only up to penultimate chunk

    Ok(immutable_chunk_file_paths)
}

fn read_immutable_file(file_path: &Path) -> StdResult<Vec<Block>> {
    let cbor = fs::read(file_path).expect("Should have been able to read the file");

    let mut blocks_start_byte_index: Vec<_> = (0..cbor.len())
        .filter(|&byte_index| {
            let cbor_header_maybe = &cbor[byte_index..min(byte_index + 2, cbor.len())];
            match block_era(cbor_header_maybe) {
                Outcome::Matched(_) | Outcome::EpochBoundary => true,
                Outcome::Inconclusive => false,
            }
        })
        .collect();
    blocks_start_byte_index.push(cbor.len() + 1);

    let mut blocks = Vec::new();
    let mut last_start_byte_index = 0;
    for block_start_index in blocks_start_byte_index.into_iter().skip(1) {
        let maybe_end_byte_index = min(block_start_index, cbor.len());
        if let Ok(multi_era_block) =
            MultiEraBlock::decode(&cbor[last_start_byte_index..maybe_end_byte_index])
        {
            let block = convert_multi_era_block_to_block(multi_era_block)?;
            blocks.push(block);
            last_start_byte_index = block_start_index;
        }
    }

    eprintln!(
        ">>>> Found {} blocks in {:?} immutable file",
        blocks.len(),
        file_path
    );

    Ok(blocks)
}

fn convert_multi_era_block_to_block(multi_era_block: MultiEraBlock) -> StdResult<Block> {
    let mut transactions = Vec::new();
    for tx in &multi_era_block.txs() {
        let tx_hash = tx.hash();
        let mut transactions_inputs = Vec::new();
        for tx_input in tx.consumes() {
            transactions_inputs.push(TransactionInput {
                hash: tx_input.hash().to_string(),
                index: tx_input.index(),
            });
        }
        let mut transactions_outputs = Vec::new();
        for (index, tx_output) in tx.produces() {
            let address = match tx_output.address().unwrap().to_bech32() {
                Ok(address) => address,
                Err(_) => tx_output.address().unwrap().to_hex(),
            };
            transactions_outputs.push(TransactionOutput {
                hash: tx_hash.to_string(),
                index: index as u64,
                address,
                amount: tx_output.lovelace_amount(),
            });
        }
        let transaction = Transaction {
            hash: tx_hash.to_string(),
            inputs: transactions_inputs,
            outputs: transactions_outputs,
        };
        transactions.push(transaction);
    }
    let block = Block {
        era: multi_era_block.era(),
        number: multi_era_block.number(),
        slot: multi_era_block.slot(),
        transactions,
        total_transactions: multi_era_block.tx_count() as u64,
    };

    Ok(block)
}

fn compute_transaction_history_from_blocks(
    transactions_history: &mut AddressTransactionHistory,
    blocks: &[Block],
) -> StdResult<()> {
    let save_transaction_for_address =
        |transactions_history: &mut AddressTransactionHistory, address, transaction_hash| {
            if let Some(transactions_history_address) = transactions_history.get_mut(&address) {
                if !transactions_history_address.contains(&transaction_hash) {
                    transactions_history_address.push(transaction_hash);
                }
            } else {
                transactions_history.insert(address, vec![transaction_hash]);
            }
        };

    let mut unspent_transaction_outputs: UnspentTransactionxOutput = HashMap::new();
    let save_unspent_transaction_output =
        |unspent_transaction_outputs: &mut UnspentTransactionxOutput,
         tx_output: &TransactionOutput| {
            let address = tx_output.address.to_owned();
            let tx_input = tx_output.into();
            unspent_transaction_outputs.insert(tx_input, address.to_owned());
        };
    let delete_unspent_transaction_output =
        |unspent_transaction_outputs: &mut UnspentTransactionxOutput,
         tx_input: &TransactionInput| {
            unspent_transaction_outputs.remove(tx_input);
        };

    for block in blocks {
        for tx in &block.transactions {
            for tx_input in &tx.inputs {
                if let Some(address) = unspent_transaction_outputs.get(tx_input) {
                    save_transaction_for_address(
                        transactions_history,
                        address.to_owned(),
                        tx.hash.to_owned(),
                    );
                }
                delete_unspent_transaction_output(&mut unspent_transaction_outputs, tx_input);
            }
            for tx_output in &tx.outputs {
                save_transaction_for_address(
                    transactions_history,
                    tx_output.address.to_owned(),
                    tx_output.hash.to_owned(),
                );
                save_unspent_transaction_output(&mut unspent_transaction_outputs, tx_output);
            }
        }
    }

    Ok(())
}
